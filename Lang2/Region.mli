(* Regions of the input file

   A region is a contiguous segment in a text file, starting at the
   lexing position [start], included, and ending at position [stop],
   excluded (so a region is empty if, and only if, [start =
   stop]). See module [Pos].

     The first character of a file starts at offset zero (that is,
   column one), and [start] is always lower than or equal to [stop],
   and they must refer to the same text file.

     The function [make] is the only way to create a region from two
   lexing positions. If the positions are not properly ordered or
   refer to different files, the exception [Invalid] is raised.

     The start and stop positions can be projected from a region by
   means of the eponym functions.

     To deal with ghost expressions, we need ghost regions. The module
   [Lexing] provides a [ghost] position, and we also provide a [ghost]
   region and a way to check for it.

     Regions can be compared by means of [leq] ("lower or equal").

     A region can be made a character string with [to_string].
*)

type t = private {start: Pos.t; stop: Pos.t}
type region = t

(* Injections *)

exception Invalid

val make: start:Pos.t -> stop:Pos.t -> t

val ghost : t

(* Projections *)

val start_pos : t -> Pos.t
val stop_pos  : t -> Pos.t
val start_loc : t -> Loc.t
val stop_loc  : t -> Loc.t
val file      : t -> string

val pos       : t -> Pos.t * Pos.t
val locs      : t -> Loc.t * Loc.t

(* Comparisons *)

val leq : t -> t -> bool

(* Predicates *)

val is_ghost : t -> bool

(* Conversions to [string] *)

val to_string : ?file:bool -> region -> string
val compact   : ?offsets:bool -> region -> string
