{
 (* Lexical errors *)

 type message = string
 type start = Lexing.position
 type stop = Lexing.position
 type diagnostic = message * start * stop

 exception Error of diagnostic

 (* Keywords *)

 let keywords =
   Token.["let",   Let;
          "rec",   Rec;
          "and",   And;
          "in",    In;
          "fun",   Fun;
          "if",    If;
          "then",  Then;
          "else",  Else;
          "true",  True;
          "false", False;
          "not",   Not]

 module StringMap = Map.Make(String)

 let add map (key,value) = StringMap.add key value map

 let kwd_map = List.fold_left add StringMap.empty keywords
}

(* Auxiliary regular expressions *)

let newline    = '\n' | '\r' | "\r\n"
let blank      = ' ' | '\t'
let lowercase  = ['a'-'z' '_']
let uppercase  = ['A'-'Z' '_']
let ident_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let ident      = lowercase ident_char*
let constr     = uppercase ident_char*
let nat        = ['1'-'9']['0'-'9']* | '0'
let escaped    = "\\n" | "\\\"" | "\\?" | "\\\\" | "\\a"
                 | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v"
let str_char   = [^'"' '\\' '\n'] | escaped
let string     = '"' str_char* '"'

(* Rules *)

rule token = parse
  newline      { Lexing.new_line lexbuf; token lexbuf       }
| blank+       { token lexbuf                               }
| ","          { Token.COMMA                                }
| "="          { Token.EQ                                   }
| "<>"         { Token.NE                                   }
| "=<"         { Token.LE                                   }
| ">="         { Token.GE                                   }
| "<"          { Token.LT                                   }
| ">"          { Token.GT                                   }
| "->"         { Token.ARROW                                }
| "-"          { Token.MINUS                                }
| "+"          { Token.PLUS                                 }
| "/"          { Token.DIV                                  }
| "*"          { Token.MULT                                 }
| "&&"         { Token.BOOL_AND                             }
| "||"         { Token.BOOL_OR                              }
| "("          { Token.LPAR                                 }
| ")"          { Token.RPAR                                 }
| string as s  { Token.Str s                                }
| eof          { Token.EOF                                  }
| "(*"         { let start = Lexing.lexeme_start_p lexbuf
                 and stop  = Lexing.lexeme_end_p lexbuf
                 in comment start stop lexbuf;
                    token lexbuf                            }
| nat as n     { Token.Nat n                                }
| ident as id  { try StringMap.find id kwd_map with
                   Not_found -> Token.Ident id               }
| _ as c       { let msg   = Printf.sprintf "Invalid character %c." c
                 and start = Lexing.lexeme_start_p lexbuf
                 and stop  = Lexing.lexeme_end_p lexbuf
                 in raise (Error (msg, start, stop))        }

and comment start stop = parse
  "(*"         { let start' = Lexing.lexeme_start_p lexbuf
                 and stop'  = Lexing.lexeme_end_p lexbuf
                 in comment start' stop' lexbuf;
                    comment start  stop  lexbuf             }
| "*)"         { () }
| newline      { Lexing.new_line lexbuf;
                 comment start stop lexbuf                  }
| eof          { raise (Error ("Open comment.",start,stop)) }
| _            { comment start stop lexbuf                  }


{
(* Standalone lexer for debugging purposes *)

let prerr ~(kind: string) (msg, start, stop) =
  let open Lexing in
  let start_col_pos = start.pos_cnum - start.pos_bol
  and stop_col_pos = stop.pos_cnum - stop.pos_bol in
  let info = Printf.sprintf "%s error at line %i, character %i"
                            kind start.pos_lnum start_col_pos
  in if stop.pos_lnum = start.pos_lnum
     then Printf.eprintf "%s-%i:\n%s\n%!" info stop_col_pos msg
     else Printf.eprintf "%s to line %i, character %i:\n%s\n%!"
                         info stop.pos_lnum stop_col_pos msg

let trace ~(file: string) =
  try
    match open_in file with
      cin ->
        let buffer = Lexing.from_channel cin in
        let rec iter () =
          try
            match token buffer with
              Token.EOF -> close_in cin; close_out stdout
            | t -> Printf.printf "%s\n%!" (Token.string_of_token t);
                   iter ()
          with Error diag -> prerr ~kind:"Lexical" diag
        in iter ()
  with Sys_error msg -> prerr_endline msg
}
