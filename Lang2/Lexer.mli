(* Simple lexer for the Mini-ML language *)

type filename = string

(* Error reporting *)

type message = string
type diagnostic = message * Region.t

exception Error of diagnostic

(* Tokeniser *)

(* The call [get_token ~log] evaluates in a lexer (a.k.a
   tokeniser or scanner) whose type is [Lexing.lexbuf -> Token.t].

     The argument [log] is a logger. As its type shows and suggests,
   it is a pair made of an output channel and a printer for
   tokens. The lexer would use any logger to print the recognised
   tokens to the given channel. If no logger is given to [get_token],
   no printing takes place while the lexer runs.

     The call [reset ~file ~line buffer] modifies in-place the lexing
   buffer [buffer] so the lexing engine records that the file
   associated with [buffer] is named [file], and the current line is
   [line]. This function is useful when lexing a file that has been
   previously preprocessed by the C preprocessor, in which case the
   argument [file] is the name of the file that was preprocessed,
   _not_ the preprocessed file (of which the user is not normally
   aware). By default, the [line] argument is [1].
*)

type logger = out_channel * (out_channel -> Token.t -> unit)

val get_token: ?log:logger -> Lexing.lexbuf -> Token.t
val reset:     file:string -> ?line:int -> Lexing.lexbuf -> unit

(* Debugging *)

val trace:           filename -> unit
val prerr:           kind:string -> diagnostic -> unit
val error_to_string: kind:string -> message -> Region.t -> string
val output_token:    Lexing.lexbuf -> out_channel -> Token.t -> unit

(* Hack to roll back one lexeme in Ocamllex buffer (should be safe if
   used in the semantic actions of the regular expression recognising
   the lexeme rolled back. *)
(*
val rollback: Lexing.lexbuf -> unit
*)
