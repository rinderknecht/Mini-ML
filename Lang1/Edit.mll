(* Editing text files (multiple inputs, multiple outputs) *)

{
(* HEADER *)


module type Trans =
  sig
    type t
    type trans = t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

module type S =
  sig
    type t
    type edit = t
    type trans

    val null  : t
    val copy  : trans -> Loc.t  -> edit -> edit
    val skip  : trans -> Loc.t  -> edit -> edit
    val write : trans -> string -> edit -> edit
    val goto  : trans -> char   -> edit -> edit

    exception Invalid of Loc.t * Loc.t

    val check : edit -> unit

    type filter

    val insert      : trans -> Loc.t option -> string          -> filter -> filter
    val overwrite   : trans -> Loc.t        -> string          -> filter -> filter
    val patch       : trans -> Loc.t option -> Loc.t  -> string -> filter -> filter
    val delete      : trans -> Loc.t option -> Loc.t           -> filter -> filter
    val discard     : trans ->                Loc.t           -> filter -> filter
    val copy_to     : trans ->                Loc.t           -> filter -> filter
    val skip_to     : trans ->                 char           -> filter -> filter
    val skip_to_end : trans                                  -> filter -> filter
    val append      : trans                         -> string -> filter -> filter
    val copy_to_end : trans                                           -> filter
    val stop        :                                                   filter

    val compile     : filter -> edit
    val compile_cps : filter -> edit

    type filename = string

    module TransSet : Set.S with type elt = trans
    module TransMap : Map.S with type key = trans
    module FileMap  : Map.S with type key = filename

    type binding = {
      lift : TransSet.t FileMap.t;
      drop : filename TransMap.t
    }

    type io_map = {
      input     : binding;
      output    : binding;
      to_string : trans -> string
    }

    val init_IO : (trans -> string) -> io_map
    val add     : trans -> in_:filename -> out:filename -> io_map -> io_map
    val show    : ?emacs:bool -> io_map -> edit list -> unit

    val print_io_map : io_map -> unit

    type in_desc  = (Loc.t * Lexing.lexbuf) TransMap.t
    type out_desc = out_channel TransMap.t
    type desc     = {in_desc: in_desc; out_desc: out_desc}


    val build : ?opt:bool -> io_map -> edit list -> desc * edit list
    val apply : ?emacs:bool -> ?io:io_map -> desc -> edit -> unit

    val close_out_desc : io_map -> desc -> unit
  end

module Make (Trans: Trans) =
  struct

    type trans = Trans.t

    module TransSet = Set.Make (Trans)
    module TransMap = Map.Make (Trans)
    module FileMap  = Map.Make (String)

    (* A low-level edition DSL.

       Edits are read and write operations on (possibly) multiple text
       files. For each file, edits are increasingly sorted by the
       locations where they apply.
    *)

    type t =
      Null
    | Copy  of trans *  Loc.t * t
    | Skip  of trans *  Loc.t * t
    | Write of trans * string * t
    | Goto  of trans *   char * t

    type edit = t

    (* LEXER ENGINE *)

    (* Let [loc] be a location in an input file with lexing buffer
       [lexbuf]. After a lexeme is read in the file, and stored in
       [lexbuf], the value of the call [update loc lexbuf] is the location
       after that lexeme. *)

    let update loc lexbuf =
      let open Lexing in
      let line = Loc.line loc
      and make = Loc.make ~file:(Loc.file loc) in
      match lexeme lexbuf with
        "\n" | "\r" -> new_line lexbuf; make ~line:(1+line) ~column:1
      | _ -> let column =
              Loc.column loc + lexeme_end lexbuf - lexeme_start lexbuf
            in make ~line ~column


    let null                  = Null
    let copy  trans loc  edit = Copy  (trans, loc,  edit)
    let skip  trans loc  edit = Skip  (trans, loc,  edit)
    let write trans text edit = Write (trans, text, edit)
    let goto  trans char edit = Goto  (trans, char, edit)

    (* The value of the call [check edit] is [true] if, and only if,
       the locations enabling the individual edits are strictly increasing
       for a given transformation, so they can be applied in one pass over
       the corresponding input file. If this condition is not satisfied,
       the exception [Invalid] is raised. *)

    exception Invalid of Loc.t * Loc.t

    let check edit =
      let rec chk map = function
        Null -> () | Write (_,_,edit) | Goto (_,_,edit) ->
          chk map edit
      | Copy (trans,loc,edit) | Skip (trans,loc,edit) ->
          try
            if Loc.leq (TransMap.find trans map) loc
            then chk (TransMap.add trans loc map) edit
            else raise (Invalid (TransMap.find trans map, loc))
          with Not_found -> chk (TransMap.add trans loc map) edit
      in chk TransMap.empty edit

    (* Filters: A high-level edition DSL *)

    type filter =
      Insert    of trans * Loc.t option * string          * filter
    | Overwrite of trans * Loc.t        * string          * filter
    | Patch     of trans * Loc.t option * Loc.t  * string * filter
    | Delete    of trans * Loc.t option * Loc.t           * filter
    | Discard   of trans *                Loc.t           * filter
    | CopyTo    of trans *                Loc.t           * filter
    | SkipTo    of trans *                char            * filter
    | SkipToEnd of trans                                  * filter
    | Append    of trans                         * string * filter
    | CopyToEnd of trans
    | Stop

    let insert trans loc text filter = Insert (trans, loc, text, filter)
    and overwrite trans loc text filter = Overwrite (trans, loc, text, filter)
    and patch trans start stop text filter = Patch (trans, start, stop, text, filter)
    and delete trans start stop filter = Delete (trans, start, stop, filter)
    and discard trans loc filter = Discard (trans, loc, filter)
    and copy_to trans loc filter = CopyTo (trans, loc, filter)
    and skip_to trans char filter = SkipTo (trans, char, filter)
    and skip_to_end trans filter = SkipToEnd (trans, filter)
    and append trans text filter = Append (trans, text, filter)
    and copy_to_end trans = CopyToEnd trans
    and stop = Stop

    (* The function [compile] translates these changes to the
       lower-level editing DSL defined above (see type [t]). *)

    let rec compile = function
      Insert (trans, Some loc, text, filter) ->
        Copy  (trans, loc,
        Write (trans, text, compile filter))
    | Insert (trans, None, text, filter) ->
        Write (trans, text, compile filter)
    | Overwrite (trans, loc, text, filter) ->
        Copy (trans, loc,
        Skip (trans, Loc.add_col loc ~offset:(String.length text),
        Write (trans, text, compile filter)))
    | Patch (trans, Some start, stop, patch, filter) ->
        Copy  (trans, start,
        Skip  (trans, stop,
        Write (trans, patch, compile filter)))
    | Patch (trans, None, stop, patch, filter) ->
        Skip  (trans, stop,
        Write (trans, patch, compile filter))
    | Delete (trans, Some start, stop, filter) ->
        Copy (trans, start,
        Skip (trans, stop, compile filter))
    | Delete (trans, None, stop, filter) ->
        Skip (trans, stop, compile filter)
    | Discard (trans, loc, filter) ->
      Skip (trans, loc, compile filter)
    | CopyTo (trans, loc, filter) ->
        Copy (trans, loc, compile filter)
    | SkipTo (trans, char, filter) ->
        Goto (trans, char, compile filter)
    | CopyToEnd trans ->
        Copy (trans, Loc.max ~file:"", Null)
    | SkipToEnd (trans,filter) ->
        Skip (trans, Loc.max ~file:"", compile filter)
    | Append (trans, text, filter) ->
        Copy  (trans, Loc.max ~file:"",
        Write (trans, text, compile filter))
    | Stop -> Null

    (* Compiling in Continuation-Passing Style *)

    let rec compile_cps k = function
      Insert (trans, Some loc, text, filter) ->
        compile_cps (fun v -> k (Copy (trans, loc, Write (trans, text, v)))) filter
    | Insert (trans, None, text, filter) ->
        compile_cps (fun v -> k (Write (trans, text, v))) filter
    | Overwrite (trans, loc, text, filter) ->
        compile_cps
          (fun v -> k (Copy  (trans, loc,
                    Skip (trans, Loc.add_col loc ~offset:(String.length text),
                    Write (trans, text, v))))) filter
    | Patch (trans, Some start, stop, patch, filter) ->
        compile_cps
          (fun v -> k (Copy (trans, start,
                    Skip (trans, stop,
                    Write (trans, patch, v))))) filter
    | Patch (trans, None, stop, patch, filter) ->
        compile_cps (fun v -> k (Skip (trans, stop, Write (trans, patch, v)))) filter
    | Delete (trans, Some start, stop, filter) ->
        compile_cps (fun v -> k (Copy (trans, start, Skip (trans, stop, v)))) filter
    | Delete (trans, None, stop, filter) ->
        compile_cps (fun v -> k (Skip (trans, stop, v))) filter
    | Discard (trans, loc, filter) ->
        compile_cps (fun v -> k (Skip (trans, loc, v))) filter
    | CopyTo (trans, loc, filter) ->
        compile_cps (fun v -> k (Copy (trans, loc, v))) filter
    | SkipTo (trans, char, filter) ->
        compile_cps (fun v -> k (Goto (trans, char, v))) filter
    | SkipToEnd (trans,filter) ->
        compile_cps (fun v -> k (Skip (trans, Loc.max ~file:"", v))) filter
    | Append (trans, text, filter) ->
        compile_cps (fun v -> k (Copy (trans, Loc.max ~file:"",
                              Write (trans, text, v)))) filter
    | CopyToEnd trans ->
      k (Copy (trans, Loc.max ~file:"", Null))
    | Stop -> k Null

    let compile_cps filter = compile_cps Utils.id filter

    (* I/O maps and file bindings *)

    type filename = string

    type binding = {
      lift : TransSet.t FileMap.t;
      drop : filename TransMap.t
    }

    type io_map = {
      input     : binding;
      output    : binding;
      to_string : trans -> string
    }

    let init_IO mk_str =
      let empty = {
        lift = FileMap.empty;
        drop = TransMap.empty
      }
      in {input=empty; output=empty; to_string=mk_str}

    let add_in trans file io =
      let t_set = try FileMap.find file io.input.lift with
                    Not_found -> TransSet.empty in
      let lift  = FileMap.add file (TransSet.add trans t_set) io.input.lift
      and drop  = TransMap.add trans file io.input.drop
      in {io with input = {lift; drop}}

    let add_out trans file io =
      let t_set = try FileMap.find file io.output.lift with
                    Not_found -> TransSet.empty in
      let lift  = FileMap.add file (TransSet.add trans t_set) io.output.lift
      and drop  = TransMap.add trans file io.output.drop
      in {io with output = {lift; drop}}

    let add trans ~in_ ~out io = add_out trans out (add_in trans in_ io)

    (* Pretty-printing of I/O maps (of type [io_map]) *)

    let print_trans to_string trans file =
      Printf.printf "%s: %s\n" (to_string trans) file

    let print_file to_string file tree =
      let show_trans tree = print_string (to_string tree ^ ", ")
      in Printf.printf "%s -> {" file;
      TransSet.iter show_trans tree;
      print_string "}"

    let print_bindings to_string {lift;drop} =
      print_endline " * Lift:";
      FileMap.iter (print_file to_string) lift;
      print_endline " * Drop:";
      TransMap.iter (print_trans to_string) drop

    let print_io_map io =
      print_endline "Displaying input:";
      print_bindings io.to_string io.input;
      print_endline "Displaying output:";
      print_bindings io.to_string io.output

    (* Pretty-printing of edits (of type [edit]) *)

    let string_of_copy ?(emacs=true) io trans loc =
      Printf.sprintf "*** Copy up to %s:%s:%s into %s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (Loc.to_string ~emacs loc)
        (Filename.basename (TransMap.find trans io.output.drop))

    let string_of_skip ?(emacs=true) io trans loc =
      Printf.sprintf "*** Skip up to %s:%s:%s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (Loc.to_string ~emacs loc)

    let string_of_goto io trans char =
      Printf.sprintf "*** Go to next character %s:%s:'%s'"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (Char.escaped char)

    let string_of_write io trans text =
      Printf.sprintf "*** Write to %s:%s:\n%s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.output.drop))
        (if text = "" then "<empty string>" else text)

    let rec string_of_edit ?(emacs=true) io = function
      Null -> "*** Null."
    | Copy (trans,loc,edit) ->
        string_of_copy ~emacs io trans loc ^ "\n"
      ^ string_of_edit ~emacs io edit
    | Skip (trans,loc,edit) ->
        string_of_skip ~emacs io trans loc ^ "\n"
      ^ string_of_edit ~emacs io edit
    | Goto (trans,char,edit) ->
        string_of_goto io trans char ^ "\n"
      ^ string_of_edit ~emacs io edit
    | Write (trans,text,edit) ->
        string_of_write io trans text ^ "\n"
      ^ string_of_edit ~emacs io edit

    let print ?(emacs=true) io edit =
      print_endline (string_of_edit ~emacs io edit)

    (* The call [show io edits] prints the edits [edits] interpreted
       with respect to the I/O map [io]. *)

    let show ?(emacs=true) io = List.iter (print ~emacs io)

    (* Two transformations, [trans1] and [trans2], are considered
       equal by [eq_input] if their corresponding file names are equal. *)

    let eq_input io trans1 trans2 =
      let in_drop = io.input.drop
      in TransMap.(find trans1 in_drop = find trans2 in_drop)

    let eq_output io trans1 trans2 =
      let out_drop = io.output.drop
      in TransMap.(find trans1 out_drop = find trans2 out_drop)

    let eq_io io trans1 trans2 =
      eq_input io trans1 trans2 && eq_output io trans1 trans2

    (* Reducing an edit

       The value of [reduce io edit] is an edit whose operational
       semantics is the same as that of [edit] (that is, its application
       to the same input yields the same output), but is possibly
       shorter. In the following, it is helpful to keep in mind that a
       [Skip] may only change the state of the input, a [Write] may only
       change the state of the output, and [Copy] may change both. We
       also assume that successive edits apply to increasing locations
       in the source.

       The first rule says that a [Copy] or [Skip] which apply up to the
       start of the file are simply discarded.

       The following four cases deal with all combinations of two
       consecutive [Copy] or [Skip] such that they apply up to the same
       location in the same input. In these cases, the reduction is tried
       without it.

       The sixth rule states that, in case of two successive [Copy] edits,
       both from the same input file to the same output file, the second
       applying further (or at the same location), then we can get rid of
       the first [Copy] (because it is contained in the second one, as we
       assume increasing locations as a precondition to calling [reduce]).

       The seventh rule allows a [Copy] to see through an empty [Write].

       The eighth rule mandates that, in case of two successive [Skip]
       edits, both applying to the same input file, if the location does
       not decrease (assumed), then we can get rid of the first [Skip]
       (again, this is a containment rule).

       The ninth rule says that a [Skip] followed by a [Write] can
       commute: we choose the bring the [Write] on top, so the fifth or
       sixth rules may have a chance to be applied.

       The tenth rule states that a [Skip] up to the end of the input file
       is equivalent to a [Null].

       The eleventh rule says that a [Skip] followed by a [Null] can be
       ignored.

       The twelfth rule says that the writing of an empty string can be
       ignored.

       The thirteenth rule says that if two [Write] edits are composed and
       they both write to the same output file, then we can replace them
       with a single [Write] with the concatenation of their respective
       strings.

       The remaining cases apply when no reduction actually takes place.
    *)

    let rec reduce io = function
      Copy (_,loc,next) | Skip (_,loc,next) when Loc.is_min loc ->
        reduce io next

    | Copy (trans1,loc1, (Skip (trans2,loc2,sub) as next)) when loc1 = loc2 ->
        if   eq_input io trans1 trans2
        then reduce io (Copy (trans1,loc1,sub))
        else Copy (trans1, loc1, reduce io next)
    | Copy (trans1,loc1, (Copy (trans2,loc2,sub) as next)) when loc1 = loc2 ->
        if   eq_io io trans1 trans2
        then reduce io (Copy(trans1,loc1,sub))
        else Copy (trans1, loc1, reduce io next)
    | Skip (trans1,loc1, (Copy (trans2,loc2,sub) as next)) when loc1 = loc2 ->
        if   eq_input io trans1 trans2
        then reduce io (Skip(trans1,loc1,sub))
        else Skip (trans1, loc1, reduce io next)
    | Skip (trans1,loc1, (Skip (trans2,loc2,sub) as next)) when loc1 = loc2 ->
        if   eq_io io trans1 trans2
        then reduce io (Skip(trans1,loc1,sub))
        else Skip (trans1, loc1, reduce io next)

    | Copy (trans1,stop1, (Copy (trans2,_,_) as next)) ->
        if   eq_io io trans1 trans2
        then reduce io next
        else Copy (trans1, stop1, reduce io next)
    | Copy (trans,stop, Write (_,"",next)) ->
        reduce io (Copy(trans,stop,next))

    | Skip (trans1,stop1, (Skip (trans2,_,_) as next)) ->
        if   eq_input io trans1 trans2
        then reduce io next
        else Skip (trans1, stop1, reduce io next)
    | Skip (trans1,stop, Write (trans2,text,next)) ->
        reduce io (Write(trans2,text,Skip(trans1,stop,next)))
    | Skip (_,stop,_) when Loc.is_max stop -> Null
    | Skip (_,_,Null) -> Null

    | Write (_,"",next) -> reduce io next
    | Write (trans1,text1, (Write (trans2,text2,edit) as next)) ->
        if   eq_output io trans1 trans2
        then reduce io (Write (trans1, text1 ^ text2, edit))
        else Write (trans1, text1, reduce io next)

    | Copy  (trans,stop,edit) -> Copy  (trans, stop, reduce io edit)
    | Skip  (trans,stop,edit) -> Skip  (trans, stop, reduce io edit)
    | Goto  (trans,char,edit) -> Goto  (trans, char, reduce io edit)
    | Write (trans,text,edit) -> Write (trans, text, reduce io edit)
    | Null -> Null

    (* Merging pairs of edits

       Given two edits meant to be applied one after the other, we want
       to know if they can be merged into a single edit, so only one
       pass on the (same) input file is sufficient, instead of two. If
       so, those two mergeable edits can then be conceived as
       non-overlapping overlays reading from the same source and writing
       to the same target.

       The value of [merge eqc io edit1' edit2'] is an edit equivalent to
       applying edit [edit1'] and then [edit2'], or else the exception
       [Disjoint] is raised.

       The value of [norm eqc edit] is an edit equivalent to [edit], but
       whose kinds (that is, the transformations) have been replaced by
       their representative in the equivalence class [eqc]. This is not
       strictly necessary, but it enables [merge] to yield an edit (if
       exception [Disjoint] was not raised) whose atomic edits are of the
       same kind, which makes them easier to understand and debug.
    *)

    module Partition = Partition3.Make (Trans)

    let rec norm (eqc: Partition.t) =
      let open Partition in function
                           Null -> Null
      | Copy  (trans, loc,edit) -> Copy  (repr trans eqc,  loc, norm eqc edit)
      | Skip  (trans, loc,edit) -> Skip  (repr trans eqc,  loc, norm eqc edit)
      | Goto  (trans,char,edit) -> Goto  (repr trans eqc, char, norm eqc edit)
      | Write (trans, loc,edit) -> Write (repr trans eqc,  loc, norm eqc edit)

    exception Disjoint

    let rec merge (eqc: Partition.t) (io: io_map) edit1' edit2' =
      match edit1', edit2' with
        Copy (trans1,loc1,edit1), Skip (trans2,loc2,edit2)
      | Skip (trans1,loc1,edit1), Copy (trans2,loc2,edit2) ->
          if eq_input io trans1 trans2 then
            let eqc = Partition.equiv trans1 trans2 eqc in
            if   Loc.lt loc1 loc2
            then let edit, eqc = merge eqc io edit1 edit2' in
                 Copy (Partition.repr trans1 eqc, loc1, edit), eqc
            else if Loc.lt loc2 loc1
            then let edit, eqc = merge eqc io edit1' edit2 in
                 Copy (Partition.repr trans2 eqc, loc2, edit), eqc
            else let edit, eqc = merge eqc io edit1 edit2 in
                 Copy (Partition.repr trans2 eqc, loc2, edit), eqc
          else raise Disjoint
      | Skip (trans1,loc1,edit1), Skip (trans2,loc2,edit2) ->
          if eq_input io trans1 trans2
          then
            let eqc = Partition.equiv trans1 trans2 eqc in
            if   Loc.lt loc1 loc2
            then let edit, eqc = merge eqc io edit1 edit2' in
                 Skip (Partition.repr trans1 eqc, loc1, edit), eqc
            else if Loc.lt loc2 loc1
            then let edit, eqc = merge eqc io edit1' edit2 in
                 Skip (Partition.repr trans2 eqc, loc2, edit), eqc
            else let edit, eqc = merge eqc io edit1 edit2 in
                 Skip (Partition.repr trans2 eqc, loc2, edit), eqc
          else raise Disjoint
      | Write (trans1,text1,edit1), Write (trans2,text2,edit2) ->
          if eq_input io trans1 trans2 && eq_output io trans1 trans2
          then
            let eqc = Partition.equiv trans1 trans2 eqc
            and text = text1 ^ text2 in
            merge eqc io (Write (Partition.repr trans1 eqc, text, edit1)) edit2
          else raise Disjoint
      | Write (trans1,text1,edit1), Skip (trans2,_,_) ->
          let eqc = Partition.equiv trans1 trans2 eqc in
          let edit, eqc = merge eqc io edit1 edit2'
          in Write (Partition.repr trans1 eqc, text1, edit), eqc
      | Skip (trans1,_,_), Write (trans2,text2,edit2)->
          let eqc = Partition.equiv trans1 trans2 eqc in
          let edit, eqc = merge eqc io  edit1' edit2
          in Write (Partition.repr trans2 eqc, text2, edit), eqc
      | Null, e | e, Null -> norm eqc e, eqc
      | _ -> raise Disjoint

    (* Merging edits pairwise in a list

       The value of [merge_seq eqc io edits] ("merge sequentially") is a
       pair containing a list of edits whose application yields the same
       effect as that of the list [edits], assuming the same I/O map
       [io].

       The function [merge_seq] applies [merge] to the edits in
       [edits] pairwise, and moves to the next pair if no merger can
       be performed. In particular, no attempt is made at permuting
       edits to find potentially better mergers. (In that sense, what
       is done here is similar to a peep hole optimisation: the width
       of the window is two elementary edits (both of type [t]). We
       leave open the problem of finding a global optimum to this
       combinatorial problem.)

       The parameter [eqc] is a partition of classes of equivalence of
       edits. When [merge_seq] discovers that two edits are mergeable,
       they are registered as equivalent in [eqc].

       The other component of the pair computed by [merge_seq eqc io
       edits] is the new equivalence class after the attempt at
       merging.
    *)

    let rec merge_seq (eqc: Partition.t) (io: io_map) = function
      edit1::(edit2::rest as edit) ->
        (match merge eqc io edit1 edit2 with
           edit12, eqc -> merge_seq eqc io (edit12::rest)
         | exception Disjoint ->
             let edit', eqc' = merge_seq eqc io edit
             in edit1::edit', eqc')
      | edit -> edit, eqc

    (* Mapping edits to their descriptors (buffers or channels) *)

    type in_desc  = (Loc.t * Lexing.lexbuf) TransMap.t
    type out_desc = out_channel TransMap.t

    type desc = {
       in_desc :  in_desc;
      out_desc : out_desc
    }

    (* The value of [mk_in_desc io eqc] is an input descriptor, that
       is, a map from transformations (whose type is [trans]) to the
       state of an Ocamllex buffer and file location.s

       The rationale for the function [mk_in_desc] stems from the benefit
       gained from sharing input buffers between transformations that have
       meargeable edits (we could think of them as non-overlapping
       overlays).

       For example, let us suppose that we have three transformations,
       distinguished as constant data constructors [Pre], [Mid] and
       [Post]. Let us further assume that, in the I/O map, it is recorded
       that all have "foo.txt" as input file and "bar.txt" as output
       file. Moreover, let us suppose that the partition [eqc] (of
       equivalence classes) of transformations, tells us that [Pre] and
       [Mid] are equivalent, but not equivalent to [Post]. This means that
       [Pre] and [Mid] are mergeable, that is, an edit can be made that
       performs only one pass over the input and has the same effects as
       both of them being applied sequentially in two passes. On the other
       hand, [Post] must be applied separately because it conflicts with
       either [Pre] or [Mid]. In total, we can do the editing in two
       passes instead of three. With this information, the input
       descriptor records that [Pre] and [Dir] are mapped to the _same_
       lexing buffer and location in "foo.txt", called a _descriptor_ in
       this context, whereas [Post] has is own descriptor of "foo.txt".

       To compute the input descriptors, we fold the list of input file
       names and apply to them the function [delta]. For each pair of
       transformation [trans] and file [file], we fetch the representative
       of [trans] in [eqc]. If that representative [repr] is already
       mapped (in the accumulator [acc] of the fold), then we record the
       transformation as sharing the same descriptor. Otherwise, we create
       a fresh descriptor and record the representative [repr] of [trans]
       in the accumulator. If the representative is the same
       transformation as [trans], we are done, else we also record
       [trans].

       Consider the following example. We have five transformations:
       [Pre], [Ori], [Dir], [Post] and [Inc]. The I/O map states that all
       read the file "foo.txt", and [Pre], [Ori], [Dir] and [Post] write
       to "bar.txt", whereas [Inc] writes to [baz.txt]. (The reverse map
       says that "foo.txt" is read by [Pre], [Ori], [Dir] and [Post] etc.)
       There are two equivalence classes: {[Pre], [Ori], [Dir], [Post]}
       and {[Incl]}, so only two passes over "foo.txt" are sufficient,
       instead of five. The function [mk_in_desc] takes these two pieces
       of information and maps all the transformations in the first class
       to a shared input descriptor, and [Incl] to a different
       descriptor. Here is how.

       The equivalence classes can be drawn as a forest with upward edges
       and an implicit loop on the roots, which are the representative of
       the class modelled by the tree. Let us assume that we have the
       following forest:

             Ori         Inc
            / | \
         Pre Dir Post

       (The assumption bears on [Ori] being the representative: another
       transformation in the same class would do as well.) First, the map
       is empty. Let us assume that a binding in [io.input.drop] links
       [Pre] to "foo.txt". The representative of [Pre] is [Ori], which is
       not in the accumulator, hence a new descriptor is created and [Ori]
       is mapped to it. Since [Pre] is different from [Ori], we also need
       to map [Pre] to the same descriptor. Then we encounter the binding
       of [Dir] to "foo.txt". The representative of [Dir] is [Ori], which
       we now find in the accumulator. We then simply map [Dir] to the
       descriptor of [Ori]. Next, let us suppose that we visit the binding
       of [Post] to "foo.txt". The representative of [Post] is [Ori],
       which is already recorded, so we map [Post] to the descriptor of
       [Ori]. Finally, we find the binding of [Inc] to "foo.txt". It is
       not found in the accumulator, hence it is mapped to a fresh
       descriptor. Because [Inc] is its own representative, we are
       finished.
    *)

    let mk_in_desc (io: io_map) (eqc: Partition.t) : in_desc =
      let delta trans file acc =
        let repr = Partition.repr trans eqc in
        try TransMap.add trans (TransMap.find repr acc) acc with
          Not_found ->
            let buf = Lexing.from_channel (open_in file) in
            let desc = Loc.min ~file:"", buf in
            let acc = TransMap.add repr desc acc in
            if Trans.equal repr trans then acc else TransMap.add trans desc acc
      in TransMap.fold delta io.input.drop TransMap.empty

    (* The value of [mk_out_desc io] is an output descriptor (of type
       [out_desc]), mapping transformations to output channels. First,
       each file name is mapped to a fresh output channel, in
       [out_file_map].  Second, for each transformation, we fetch the
       output channel for the associated file, and map the
       transformation directly to that channel (we compose the two
       maps). *)

    let mk_out_desc (io: io_map) : out_desc =
      let apply _ file acc =
        FileMap.add file (if file = "-" then stdout else open_out file) acc in
      let out_map = FileMap.empty in
      let out_file_map =
        TransMap.fold apply io.output.drop out_map in
      let delta trans file acc =
        TransMap.add trans (FileMap.find file out_file_map) acc
      in TransMap.fold delta io.output.drop TransMap.empty

    let close_out_desc (io: io_map) (desc: desc): unit =
      let io_map = io.output.drop
      and desc_map = desc.out_desc in
      TransMap.iter
        (fun key file ->
           if file <> "-" then
             try TransMap.find key desc_map |> close_out with Not_found -> ())
        io_map

    (* The call [mk_desc io eqc] calls [mk_in_desc] and [mk_out_desc]
       to create a complete descriptor according the I/O map [io] and
       the partition of transformations [eqc]. *)

    let mk_desc (io: io_map) (eqc: Partition.t) : desc =
      {in_desc = mk_in_desc io eqc; out_desc = mk_out_desc io}

    (* Making the descriptors and optionally optimising lists of edits
       by merging them pairwise whenever possible.

       The value of [build io edits] is a pair whose first component
       is a collection of I/O descriptors, of type [desc], mapping
       transformations of type [trans] to input and output descriptors
       (respectively, lexing buffers and output channels).

       If the call is [build ~opt:true io edits], then the second
       component is a list of edits equivalent to the list [edits], but
       hopefully shorter due to possible mergers between contiguous edits
       within the list. Mergers are left-associative, so the list of edits
       [[e1;e2;e3]] would yield the mergers [merge ((merge e1 e2) e3)] (we
       omitted the I/O map and partition of transformations). If no
       optimisation is required by having the labelled argument
       [~opt:false], then [edits] is returned instead (the default is no
       optimisation).

       The optimised list of edits is computed as follows. We attempt to
       merge pairwise the edits in [edits] (see function [merge_seq]). The
       resulting edits [edits'] are then individually reduced (see
       function [reduce]) and if any final edit is trivial, it is
       discarded.

       The partition of transformations [eqc], which resulted from
       merging the edits, is used to create the descriptors for the
       transformations (see function [mk_desc]).

       Note that the initial partition (of equivalence classes) maps
       each transformation [trans] to itself, with height [0] (since it is
       a tree with a single node).
    *)

    let build ?(opt=false) (io: io_map) (edits: edit list) =
      let init =
        let apply trans _ acc = Partition.equiv trans trans acc
        in TransMap.fold apply io.input.drop Partition.empty in
      let apply edit acc =
        match reduce io edit with
          Null -> acc
        |    e -> e::acc in
      if opt then
        let edits', eqc = merge_seq init io edits in
        mk_desc io eqc, List.fold_right apply edits' []
      else mk_desc io init, edits

(* END OF HEADER *)
}

(* The call [scan loc action stop lexbuf] applies, as a
   side-effect, the function [action] to the first character in
   the lexing buffer [lexbuf]. Then, it updates the location [loc]
   to denote that newly read character, and proceeds to checking
   whether the action has to be applied to the next character or
   not, until the location [stop] (excluded) is reached.

   The precondition is [Loc.lt loc stop], which is checked in the
   wrapper of [scan], the function [common].

   If the lexer [scan] reaches the end of the file before it could
   apply a pending action (from an edit), it gracefully returns [loc],
   in order to enable idioms like [Copy (trans, Loc.max ~file:"",
   Null)] and [Skip (trans, Loc.max ~file:"", Null)], respectively to
   copy the remains of the input of transform [trans] to its output,
   and to skip the remains of the input.

   Technical note: the scanner does not need to roll back any lexeme
   because it assumes that the locations used to trigger the actions
   are just one column after (they are excluded from the edit).
*)

rule scan loc action stop = parse
  _ as c { action c;
           let loc' = update loc lexbuf in
           if   Loc.lt loc' stop
           then scan loc' action stop lexbuf
           else loc' }
| eof    { loc }

and scan_until loc action char_stop = parse
  _ as c { action c;
           let loc' = update loc lexbuf in
           if   char_stop = c then loc'
           else scan_until loc' action char_stop lexbuf }
| eof    { loc }

{
(* POSTLUDE *)

  (* The evaluation of the call [apply desc edits] applies the edits
     in [edits], using the I/O descriptors [desc] to interpret the I/O
     as lexing buffers (for inputs) and channels (for outputs).

     As the name suggests, the function [common] implements the
     commonalities between the effect of a copy edit and a skip edit. As
     a consequence, it is parameterised over the appropriate action
     ([action]) in each case: in case of a skip, nothing is done,
     whereas the current character in the input is copied to the
     output.

     The optional parameters [emacs] and [io] are only used for tracing
     the edit being applied and [io] has to be one used to create
     [desc] (not checked here).
  *)

    let common (desc: desc) trans stop action =
      let loc, buffer = TransMap.find trans desc.in_desc in
      if Loc.lt loc stop then
        let loc' = scan loc action stop buffer in
        {desc with in_desc = TransMap.add trans (loc',buffer) desc.in_desc}
      else desc

    let rec apply ?(emacs=true) ?io (desc: desc) edit =
      let nothing _ = ()
      and () = match io with
                 None -> ()
               | Some io_map -> show ~emacs io_map [edit] in
      match edit with
        Copy (trans, stop, edit) ->
          let cout = TransMap.find trans desc.out_desc
          in apply (common desc trans stop (output_char cout)) edit
      | Skip (trans, stop, edit) ->
          apply (common desc trans stop nothing) edit
      | Goto (trans, char, edit) ->
          let loc, buffer = TransMap.find trans desc.in_desc in
          let loc' = scan_until loc nothing char buffer in
          let desc =
            {desc with in_desc = TransMap.add trans (loc',buffer) desc.in_desc}
          in apply desc edit
      | Write (trans, text, edit) ->
          let cout = TransMap.find trans desc.out_desc
          in output_string cout text; apply desc edit
      | Null -> flush_all ()
  end
}
