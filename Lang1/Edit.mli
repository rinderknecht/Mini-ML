(* Editing text files (multiple inputs, multiple outputs) *)


(* Edits are parameterised by transformations *)

module type Trans =
  sig
    type t
    type trans = t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* We define a tiny Domain Specific Language (DSL) with an abstract
   syntax whose interpretation results in editing the text of possibly
   multiple files, but not in-place: several output files may be
   produced as a result. Edits on the same input file can be defined
   separately and this module provides a function which tries to merge
   pairwise edits that are sequentially composable, in order to
   minimise the number of passes on a given input source.

     In the following documentation, this DSL is qualified as being
   "low-level" in contexts of another DSL (see filters below), called
   "high-level".
*)

module type S =
  sig
    (* The following abstract type [edit] defines the edits. The
       function applying edits (see [apply] in the postlude) must be
       able to map transforms to their lexing buffers and output
       channels, respectively for input and output files. *)

    type t
    type edit = t
    type trans

    (* Edits are modelled by the values of the following function
       calls and constants:

     * [null] is the empty edit;

     * the edit [copy trans loc edit] represents the action of copying
       the characters in the input of transform [trans] until the
       location [loc] (excluded) is reached, and writing them to its
       output, followed by the edit [edit] to be applied next;

     * the edit [skip trans loc edit] models the action of skipping
       the text in the input of transform [trans] until the location
       [loc] (excluded) is reached, followed by the edit [edit] to be
       applied next;

     * the edit [write trans text edit] denotes the action of writing
       the text [text] to the output of transform [trans], followed by
       the edit [edit] to be applied next;

     * the edit [goto trans char edit] means skipping the text in the
       input of transform [trans] until the character [char]
       (included) is reached, followed by the edit [edit] to be
       applied next. Note that this edit, contrary to the others, is
       not static, as it depends on the contents of the input text.
    *)

    val null  : t
    val copy  : trans -> Loc.t  -> edit -> edit
    val skip  : trans -> Loc.t  -> edit -> edit
    val write : trans -> string -> edit -> edit
    val goto  : trans -> char   -> edit -> edit

    (* The value of the call [check edit] is [true] if, and only if,
       the locations enabling the individual edits are strictly increasing
       for a given transformation, so they can be applied in one pass over
       the corresponding input file. If this condition is not satisfied,
       the exception [Invalid] is raised. *)

    exception Invalid of Loc.t * Loc.t

    val check : edit -> unit

    (* Filters

       It is convenient to specify edits in a manner more abstract
       than a series of copies, writes and skips (see type [edit]
       above), in other words, by means of a "high-level" DSL.

       The idea is to have the default semantics be a copy of the input
       to the output, like in XSLT, and only specify the
       differences. These can be expressed with the following function
       calls and constants:

     * [insert trans loc text filter] denotes the insertion of the
       text [text] when reaching location [loc] of the input of
       transformation [trans];

     * [overwrite trans loc text filter] denotes the overwriting by
       the text [text] when reaching location [loc] of the input of
       transformation [trans];

     * [patch trans start stop text filter] denotes the replacement,
       in the output of transformation [trans], of the text between
       locations [start] and [stop] in the input of [trans], by the
       text [text];

     * [delete trans start stop filter] denotes the deletion of the
       text between locations [start] and [stop] in the input of
       transformation [trans];

     * [discard trans loc filter] denotes the skipping of the text
       from the current location in the input of transformation
       [trans] until the location [loc] (excluded);

     * [copy_to trans loc filter] denotes the copying of the text from
       the current location in the input of transformation [trans]
       until the location [loc] (excluded);

     * [skip_to trans char filter] denotes the skipping of the text
       from the current location in the input of transformation
       [trans] until the character [char] (included);

     * [skip_to_end trans filter] denotes the skipping of the text
       from the current location in the input of transformation
       [trans] until the end of the input;

     * [copy_to_end trans] denotes the copying of the text from the
       current location in the input of transformation [trans] until
       the end of the input;

     * [append trans text filter] means copying until the end of the
       input of transformation [trans] and inserting the string
       [text];

     * [stop] denotes the end of the differences between the input and
       output of transformation [trans].

       Note that, for example, calls to [insert] and [overwrite] could be
       expressed solely by means of calls to [patch], but we retain the
       former functions for ease of use, as this is consistent with the
       purpose of the type [filter].

       Some start locations of the editing instructions above are
       optional, meaning that, when missing, they apply to the current
       location: this enable a relative mode of editing, with respect of
       the current position in the input, instead of an absolute mode (a
       given position in the input, where some edition takes place).
    *)

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

    (* Compiling filters (i.e., high-level editing commands) to
       (low-level) edits. *)

    val compile : filter -> edit

    type filename = string

    module TransSet : Set.S with type elt = trans
    module TransMap : Map.S with type key = trans
    module FileMap  : Map.S with type key = filename

    (* Values of the type [io_map] contain information about the
       mapping of transformations (of type [trans]) to file names, and
       vice-versa, both for inputs and outputs.

       The type [binding] describes such a two-way mapping. The field
       [lift] contains a map from file names to a set of
       transformations, because a file can be used by multiple
       transformations, either as input or output. Conversely, the
       field [drop] of type [binding] holds a map from transformations
       (of type [trans]) to file names, because each transformation is
       associated to one file for input, and one for output.
    *)

    type binding = {
      lift : TransSet.t FileMap.t;
      drop : filename TransMap.t
    }

    type io_map = {
      input     : binding;
      output    : binding;
      to_string : trans -> string
    }

    (* The value of [init_io mk_str] is an I/O map (of type [io_map]),
       initialised with the function [mk_str] that provides a string
       representing a transformation, of type [trans -> string]. *)

    val init_io : (trans -> string) -> io_map

    (* The value of [add trans ~in_ ~out io] is an updated version of
       the I/O map [io] with the transformation [trans] and its
       corresponding input file name [in_] and output file name
       [out]. *)

    val add : trans -> in_:filename -> out:filename -> io_map -> io_map

    (* The call [show io edits] prints the edits [edits] interpreted
       with respect to the I/O map [io]. The optional argument [emacs]
       is set to [true] by default, meaning that horizontal offsets,
       rather than column numbers, are used. *)

    val show : ?emacs:bool -> io_map -> edit list -> unit

    (* Pretty-printing of I/O maps (of type [io_map]) *)

    val print_io_map : io_map -> unit

    (* Mapping edits to their descriptors (buffers or channels)

       At a lower level of abstraction, the input of an edit is a lexing
       buffer (of type [Lexing.lexbuf]), and it is associated with the
       current location (of type [loc]) in the (implicit) source file
       (which is at an even lower level of abstraction): these data are
       enough to resume applying edits where we left.

       The output of an edit is an output channel (of type
       [out_channel]). Contrary to inputs, this kind of descriptor is not
       a lexing buffer, because we write in the file, not read with
       Ocamllex, and we also do not need the current location because we
       always resume writing where we left, as the current location is
       handled by the operating system (there is not an additional layer
       of abstraction due to Ocamllex).

       Edits are mapped to their descriptors (buffers or channels), in
       view of being applied (see function [apply] below):

     * At a lower level of abstraction, the input of an edit is a
       lexing buffer, and it is associated with the current location
       in the (implicit) source file (which is at an even lower level
       of abstraction): these data are enough to resume applying edits
       where we left.

     * The output of an edit is an output channel. Contrary to inputs,
       this kind of descriptor is not a lexing buffer, because we
       write in the file, not read with ocamllex, and we also do not
       need the current location because we always resume writing
       where we left, as the current location is handled by the
       operating system (there is not an additional layer of
       abstraction due to ocamllex).  *)

    type in_desc  = (Loc.t * Lexing.lexbuf) TransMap.t
    type out_desc = out_channel TransMap.t
    type desc     = {in_desc : in_desc; out_desc : out_desc}


    (* Making the descriptors and optionally optimising pptimising
       lists of edits by merging them pairwise whenever possible.

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
    *)

    val build : ?opt:bool -> io_map -> edit list -> desc * edit list

    (* The evaluation of the call [apply desc edits] applies the edits
       in [edits], using the I/O descriptors [desc] to interpret the I/O
       as lexing buffers (for inputs) and channels (for outputs).

       The optional parameters [emacs] and [io] are only used for tracing
       the edit being applied and [io] has to be one used to create
       [desc] (not checked here).
    *)

    val apply : ?emacs:bool -> ?io:io_map -> desc -> edit -> unit

    (* Closing the out-channel descriptors *)

    val close_out_desc : io_map -> desc -> unit
  end

module Make (Trans: Trans) : S with type trans = Trans.t
