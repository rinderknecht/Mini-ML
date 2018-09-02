match Array.length Sys.argv with
  2 ->
    (try
       let cin = open_in Sys.argv.(1) in
       let buffer = Lexing.from_channel cin in
       try
         let ast = Parser.program Lexer.token buffer in
         let v = Eval.eval ast
         in print_string (Eval.Value.to_string v);
         print_newline ()
       with
         Lexer.Error diag -> close_in cin;
           Lexer.prerr ~kind:"Lexical" diag
       | Parser.Error ->
          close_in cin;
         Lexer.prerr ~kind:"Syntactical"
           ("Parse error.",
            Lexing.lexeme_start_p buffer,
            Lexing.lexeme_end_p buffer)
     with Sys_error msg -> prerr_endline msg)
| _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
;;
