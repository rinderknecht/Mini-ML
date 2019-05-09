[@@@warning "-30"]

(* Abstract Syntax Tree (AST) for Mini-ML *)

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

type 'a reg = 'a Region.reg

(* Some keywords of OCaml *)

type keyword   = Region.t
type kwd_and   = Region.t
type kwd_begin = Region.t
type kwd_else  = Region.t
type kwd_end   = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_let_entry = Region.t
type kwd_match = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_of    = Region.t
type kwd_rec   = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t
type kwd_type  = Region.t
type kwd_with  = Region.t

(* Symbols *)

type arrow  = Region.t                                               (* "->" *)
type cons   = Region.t                                               (* "::" *)
type cat    = Region.t                                               (* "^"  *)
type append = Region.t                                               (* "@"  *)
type dot    = Region.t                                               (* "."  *)

(* Arithmetic operators *)

type minus = Region.t                                                 (* "-" *)
type plus  = Region.t                                                 (* "+" *)
type div   = Region.t                                                 (* "/" *)
type mult  = Region.t                                                 (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t                                             (* "||" *)
type bool_and = Region.t                                             (* "&&" *)

(* Comparisons *)

type eq = Region.t                                                   (* "="  *)
type ne = Region.t                                                   (* "<>" *)
type lt = Region.t                                                   (* "<"  *)
type gt = Region.t                                                   (* ">"  *)
type le = Region.t                                                   (* "=<" *)
type ge = Region.t                                                   (* ">=" *)

(* Compounds *)

type lpar = Region.t                                                  (* "(" *)
type rpar = Region.t                                                  (* ")" *)
type lbra = Region.t                                                  (* "[" *)
type rbra = Region.t                                                  (* "]" *)

(* Separators *)

type comma = Region.t                                                 (* "," *)
type semi  = Region.t                                                 (* ";" *)
type vbar  = Region.t                                                 (* "|" *)
type colon = Region.t

(* Wildcard *)

type wild = Region.t                                                  (* "_" *)

(* Literals *)

type variable    = string reg
type fun_name    = string reg
type type_name   = string reg
type type_constr = string reg
type field_name  = string reg
type map_name    = string reg
type set_name    = string reg
type constr      = string reg
type ident       = string reg

(* Comma-separated non-empty lists *)

type 'a csv = ('a, comma) Utils.nsepseq

(* Bar-separated non-empty lists *)

type 'a bsv = ('a, vbar) Utils.nsepseq

(* Semicolon-separated lists *)

type 'a ssv = ('a, semi) Utils.sepseq

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* Brackets compounds *)

type 'a brackets = {
  lbra   : lbra;
  inside : 'a;
  rbra   : rbra
}

(* The Abstract Syntax Tree (finally) *)

type t = statement list * eof

and ast = t

and eof = Region.t

and statement =
                                                    (* let p = e and ...     *)
  Let      of (kwd_let * let_bindings) reg
                                                    (* let%entry p = e and ... *)
| LetEntry of (kwd_let_entry * let_binding) reg
                                                    (* let rec p = e and ... *)
| LetRec   of (kwd_let * kwd_rec * let_rec_bindings) reg
| TypeDecl of type_decl reg                                     (* type .... *)

(* Non-recursive values *)

and let_bindings =
  (let_binding, kwd_and) Utils.nsepseq            (* p1 = e1 and p2 = e2 ... *)

and let_binding = {                                    (* p = e   p : t = e *)
  pattern  : pattern;
  lhs_type : (colon * type_expr) option;
  eq       : eq;
  let_rhs  : expr
}

(* Recursive values *)

and let_rec_bindings = (let_rec_binding,  kwd_and) Utils.nsepseq

and let_rec_binding  = {
  pattern     : variable;
  lhs_type    : (colon * type_expr) option;
  eq          : eq;
  let_rec_rhs : expr
}

(* Recursive types *)

and type_decl = kwd_type * type_name * eq * type_expr

and type_expr =
  TProd   of cartesian reg
| TSum    of (variant reg, vbar) Utils.nsepseq reg
| TRecord of record_type
| TApp    of (type_constr * type_tuple) reg
| TPar    of type_expr par reg
| TPath   of path reg
| TFun    of fun_type reg

and fun_type = {
  domain : type_expr;
  arrow  : arrow;
  range  : type_expr
}

and type_tuple = type_expr csv par

and cartesian = (type_expr, mult) Utils.nsepseq

and variant = {
  constr : constr;
  args   : (kwd_of * cartesian) option
}

and record_type = field_decl reg injection reg

and 'a injection = {
  opening    : keyword;
  elements   : ('a, semi) Utils.sepseq;
  terminator : semi option;
  closing    : keyword
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and pattern =
  Ptuple  of pattern csv reg                                (* p1, p2, ...   *)
| Plist   of pattern ssv brackets reg                       (* [p1; p2; ...] *)
| Pvar    of variable                                       (*             x *)
| Punit   of the_unit reg                                   (*            () *)
| Pint    of (string * Z.t) reg                             (*             7 *)
| Ptrue   of kwd_true                                       (*          true *)
| Pfalse  of kwd_false                                      (*         false *)
| Pstr    of string reg                                     (*         "foo" *)
| Pwild   of wild                                           (*             _ *)
| Pcons   of (pattern * cons * pattern) reg                 (*      p1 :: p2 *)
| Ppar    of pattern par reg                                (*           (p) *)
| Pconstr of (constr * pattern reg option) reg              (*    A B(3,"")  *)
| Precord of record_pattern                                 (*  {a=...; ...} *)
| Ptyped  of typed_pattern reg                              (*     (x : int) *)

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and record_pattern = field_pattern reg injection reg

and field_pattern = {
  field_name : field_name;
  eq         : eq;
  pattern    : pattern
}

and expr =
  LetIn    of let_in reg       (* let p1 = e1 and p2 = e2 and ... in e       *)
| LetRecIn of let_rec_in reg   (* let rec p1 = e1 and p2 = e2 and ... in e   *)
| Fun      of fun_expr         (* fun x -> e                                 *)
| If       of conditional      (* if e1 then e2 else e3                      *)
| Tuple    of expr csv reg     (* e1, e2, ...                                *)
| Match    of match_expr reg   (* p1 -> e1 | p2 -> e2 | ...                  *)
| Seq      of sequence reg     (* begin e1; e2; ... ; en end                 *)
| Record   of record_expr      (* {f1=e1; ... }                              *)

| Cat     of (expr * cat * expr) reg                             (* e1  ^ e2 *)
| Append  of (expr * append * expr) reg                          (* e1  @ e2 *)
| Cons    of (expr * cons * expr) reg                            (* e1 :: e2 *)

| Or      of (expr * bool_or * expr) reg                         (* e1 || e2 *)
| And     of (expr * bool_and * expr) reg                        (* e1 && e2 *)

| Lt      of (expr * lt * expr) reg                              (* e1  < e2 *)
| LEq     of (expr * le * expr) reg                              (* e1 <= e2 *)
| Gt      of (expr * gt * expr) reg                              (* e1  > e2 *)
| GEq     of (expr * ge * expr) reg                              (* e1 >= e2 *)
| NEq     of (expr * ne * expr) reg                              (* e1 <> e2 *)
| Eq      of (expr * eq * expr) reg                              (* e1  = e2 *)

| Add     of (expr * plus   * expr) reg                          (* e1  + e2 *)
| Sub     of (expr * minus  * expr) reg                          (* e1  - e2 *)

| Mult    of (expr * mult    * expr) reg                        (* e1  *  e2 *)
| Div     of (expr * div     * expr) reg                        (* e1  /  e2 *)
| Mod     of (expr * kwd_mod * expr) reg                        (* e1 mod e2 *)

| Neg     of (minus   * expr) reg                                   (*    -e *)
| Not     of (kwd_not * expr) reg                                   (* not e *)

| Call    of (expr * expr) reg                                        (* f e *)

| Int     of (string * Z.t) reg                             (* 12345         *)
| Mtz     of (string * Z.t) reg                             (* 1.00tz 3tz    *)
| Pos     of (string * Z.t) reg                             (* 3p            *)
| Path    of path reg                                       (* x x.y.z       *)
| Str     of string reg                                     (* "foo"         *)
| Unit    of the_unit reg                                   (* ()            *)
| True    of kwd_true                                       (* true          *)
| False   of kwd_false                                      (* false         *)
| Par     of expr par reg                                   (* (e)           *)
| List    of expr ssv brackets reg                          (* [e1; e2; ...] *)
| Constr  of constr
| Extern  of extern

and path = {
  module_proj : (constr * dot) option;
  value_proj  : (selection, dot) Utils.nsepseq
}

and selection =
  Name      of ident
| Component of (string * Z.t) par reg

and record_expr = field_assignment reg injection reg

and field_assignment = {
  field_name : field_name;
  assignment : eq;
  field_expr : expr
}

and sequence = {
  kwd_begin : kwd_begin;
  sequence  : expr ssv;
  kwd_end   : kwd_end
}

and match_expr = kwd_match * expr * kwd_with * cases

and cases = vbar option * (pattern * arrow * expr) bsv

and let_in = kwd_let * let_bindings * kwd_in * expr

and let_rec_in = kwd_let * kwd_rec * let_rec_bindings * kwd_in * expr

and fun_expr = (kwd_fun * variable * arrow * expr) reg

and conditional =
  IfThen     of (kwd_if * expr * kwd_then * expr) reg
| IfThenElse of (kwd_if * expr * kwd_then * expr * kwd_else * expr) reg

and extern =
  Cast   of cast_expr
| Print  of print_expr
| Scanf  of scanf_expr
| PolyEq of (variable * variable)                    (* polymorphic equality *)

and cast_expr =
  StringOfInt  of variable                               (* string_of_int  x *)
| StringOfBool of variable                               (* string_of_bool x *)

and print_expr =
  PrintString of variable                                  (* print_string x *)
| PrintInt    of variable                                  (* print_int    x *)

and scanf_expr =
  ScanfString of variable                                  (* scanf_string x *)
| ScanfInt    of variable                                  (* scanf_int    x *)
| ScanfBool   of variable                                  (* scanf_bool   x *)


(* Normalising nodes of the AST so the interpreter is more uniform and
   no source regions are lost in order to enable all manner of
   source-to-source transformations from the rewritten AST and the
   initial source.

   The first kind of expressions to be normalised is lambdas, like:

     fun a -> fun b -> a
     fun a b -> a
     fun a (b,c) -> a

   to become

     fun a -> fun b -> a
     fun a -> fun b -> a
     fun a -> fun x -> let (b,c) = x in a

   The second kind is let-bindings introducing functions without the
   "fun" keyword, like

     let g a b = a
     let h a (b,c) = a

   which become

     let g = fun a -> fun b -> a
     let h = fun a -> fun x -> let (b,c) = x in a

   The former is actually a subcase of the latter. Indeed, the general
   shape of the former is

     fun <patterns> -> <expr>

   and the latter is

     let <ident> <patterns> = <expr>

   The isomorphic parts are "<patterns> -> <expr>" and "<patterns> =
   <expr>".

     The call [norm patterns sep expr], where [sep] is a region either
   of an "->" or a "=", evaluates in a function expression (lambda),
   as expected. In order to get the regions right in the case of
   lambdas, additional regions are passed: [norm ~reg:(total,kwd_fun)
   patterns sep expr], where [total] is the region for the whole
   lambda (even if the resulting lambda is actually longer: we want to
   keep the region of the original), and the region of the original
   "fun" keyword.
*)

type sep = Region.t

val norm : ?reg:(Region.t * kwd_fun) -> pattern Utils.nseq -> sep -> expr -> fun_expr

(* Undoing the above rewritings (for debugging by comparison with the
   lexer, and to feed the source-to-source transformations with only
   tokens that originated from the original input.

     Unparsing is performed on an expression which is expected to be a
   series "fun ... -> fun ... -> ...". Either this expression is the
   right-hand side of a let, or it is not. These two cases are
   distinguished by the function [unparse], depending on the first
   keyword "fun" being concrete or ghostly (virtual). In the former
   case, we are unparsing an expression which was originally starting
   with "fun"; in the latter, we are unparsing an expression that was
   parsed on the right-hand side of a let construct. In other words,
   in the former case, we expect to reconstruct

                    let f p_1 ... p_n = e

   whereas, in the second case, we want to obtain

                    fun p_1 ... p_n -> e

     In any case, the heart of the unparsing is the same, and this is
   why the data constructors [`Fun] and [`Let] of the type [unparsed]
   share a common type: [pattern * Region.t * expr], the region can
   either actually denote the alias type [arrow] or [eq]. Let us
   assume a value of this triple [patterns, separator_region,
   expression]. Then the context (handled by [unparse]) decides if
   [separator_region] is the region of a "=" sign or "->".

   There are two forms to be unparsed:

     fun x_1 -> let p_1 = x_1 in ... fun x_n -> let p_n = x_n in e

   or

     fun p_1 -> ... fun p_n -> e

   in the first case, the rightmost "=" becomes [separator_region]
   above, whereas, in the second case, it is the rightmost "->".

   Here are some example covering all cases:

   let rec f = fun a -> fun b -> a
   let rec g = fun a b -> a
   let rec h = fun a (b,c) -> a
   let rec fst = fun (x,_) -> x

   let rec g a b = a
   let rec h (b,c) a (d,e) = a
   let len = (fun n _ -> n)
   let f l = let n = l in n
*)

type unparsed = [
  `Fun  of (kwd_fun * (pattern Utils.nseq * arrow * expr))
| `Let  of (pattern Utils.nseq * eq * expr)
| `Idem of expr
]

val unparse : expr -> unparsed

(* Conversions to type [string] *)

val to_string         :       t -> string
val pattern_to_string : pattern -> string

(* Printing the tokens reconstructed from the AST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). The
   optional parameter [undo] is bound to [true] if the caller wants
   the AST to be unparsed before printing (those nodes that have been
   normalised with function [norm_let] and [norm_fun]). *)

val print_tokens : ?undo:bool -> ast -> unit

(* Projecting regions from sundry nodes of the AST. See the first
   comment at the beginning of this file. *)

val region_of_pattern : pattern -> Region.t
val region_of_expr    : expr -> Region.t

(* Removing all outermost parentheses from a given expression *)

val rm_par  : expr -> expr

(* Predicates on expressions *)

val is_var  : expr -> bool
val is_call : expr -> bool
val is_fun  : expr -> bool

(* Variables *)

module Vars     : Set.S with type elt = string
module FreeVars : Set.S with type elt = variable

(* The value of the call [vars t] is a pair of sets: the first is the
   set of variables whose definitions are in the scope at the end of
   the program corresponding to the AST [t], the second is the set of
   free variables in that same AST.

     Computing free variables is useful because we do not want to
   escape a variable that is a predefined variable in OCaml, when we
   translate the program to OCaml: this way, we make sure that an
   unbound variable is caught before the translation (where it would
   be wrongly captured by the OCaml compiler).

    Dually, computing bound variables is useful when compiling to
   OCaml.
*)

val vars : t -> Vars.t * FreeVars.t
