(* This module offers a concept of source location, implemented by the
   type [t]. It encompasses the name of the source file, the line
   number and the column number -- the first column on a line is 1,
   and the first line is 1. *)

type t = private {file: string; line: int; column: int}
type loc = t

(* Making locations *)

val make     : file:string -> line:int -> column:int -> t
val from_pos : Lexing.position -> t

val min : file:string -> t
val max : file:string -> t

(* Projections *)

val file   : t -> string
val line   : t -> int
val column : t -> int
val offset : t -> int

(* Updates *)

val set_file : t -> file:string -> t
val add_line : t -> offset:int  -> t
val add_col  : t -> offset:int  -> t

(* Comparisons

   When comparing locations, use the functions below, _not_ the
   polymorphic operators. In particular, if at least one of two
   locations refers to an empty filename, they will be nevertheless
   comparable: in other words, they are incomparable if, and only if,
   both filenames are not empty and are different.

     This weak form of comparison is used to enable some flexibility in
   the source to source transformations, and preprocessing.
*)

exception Incomparable of t * t

val lt      : t -> t -> bool
val leq     : t -> t -> bool
val eq      : t -> t -> bool
val compare : t -> t -> int

(* Predicates *)

val is_min : t -> bool
val is_max : t -> bool

(* Conversion to [string] *)

val to_string : ?emacs:bool -> t -> string
