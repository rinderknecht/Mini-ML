(* This modules reexports [Lexing] plus conversions to string, as well
   as projections and injections for the type [Lexing.position]. *)

type t = Lexing.position
type pos = t

(* Conversions to [string] *)

val to_string: ?emacs:bool -> t -> string
val compact:   ?emacs:bool -> t -> string

(* Injections *)

val set_file: string -> t -> t
val set_line:    int -> t -> t

(* Projections *)

val get_file:        t -> string
val get_line:        t -> int
val get_offset:      t -> int
val get_line_offset: t -> int
val get_char_offset: t -> int
