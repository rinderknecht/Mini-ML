(* Pilot for the parser of Slang *)

(* Error printing and exception tracing *)


let cin = open_in pp_input
let buffer = Lexing.from_channel cin
let () = Lexer.reset ~file:basename buffer

let tokeniser = Lexer.get_token ?log:None

let () =
  try
    let ast = Parser.program tokeniser buffer in
    if Utils.String.Set.mem "unparsing" EvalOpt.debug then
      AST.print_tokens ~undo:true ast
    else AST.print_tokens ast
  with
    Lexer.Error diag ->
      close_in cin; Lexer.prerr ~kind:"Lexical" diag
  | Parser.Error ->
      close_in cin;
      Lexer.prerr ~kind:"Syntactical"
                  ("Parse error.",
                   Region.make
                     ~start:(Lexing.lexeme_start_p buffer)
                     ~stop:(Lexing.lexeme_end_p buffer))
  | Sys_error msg -> Utils.highlight msg
