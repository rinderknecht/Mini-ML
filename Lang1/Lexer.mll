(* Lexer specification for Mini-ML, to be processed by [ocamllex]. *)

{
(* START HEADER *)

open! Utils

(* Lexical errors *)

type 'a reg = 'a * Region.t
type message = string
type diagnostic = message reg

exception Error of diagnostic

let error lexbuf msg =
  let start  = Lexing.lexeme_start_p lexbuf
  and stop   = Lexing.lexeme_end_p   lexbuf in
  let region = Region.make ~start ~stop
  in raise (Error (msg, region))

(* Keywords *)

let keywords =
  Token.["and",    Some And;
         "else",   Some Else;
         "false",  Some False;
         "fun",    Some Fun;
         "if",     Some If;
         "in",     Some In;
         "else",   Some Else;
         "let",    Some Let;
         "match",  Some Match;
         "mod",    Some Mod;
         "not",    Some Not;
         "rec",    Some Rec;
         "then",   Some Then;
         "true",   Some True;
         "with",   Some With;

         (* Reserved *)

         "as",          None;
         "asr",         None;
         "assert",      None;
         "begin",       None;
         "class",       None;
         "constraint",  None;
         "do",          None;
         "done",        None;
         "downto",      None;
         "end",         None;
         "exception",   None;
         "external",    None;
         "for",         None;
         "function",    None;
         "functor",     None;
         "include",     None;
         "inherit",     None;
         "initializer", None;
         "land",        None;
         "lazy",        None;
         "lor",         None;
         "lsl",         None;
         "lsr",         None;
         "lxor",        None;
         "method",      None;
         "module",      None;
         "mutable",     None;
         "new",         None;
         "nonrec",      None;
         "object",      None;
         "of",          None;
         "open",        None;
         "or",          None;
         "private",     None;
         "sig",         None;
         "struct",      None;
         "to",          None;
         "try",         None;
         "type",        None;
         "val",         None;
         "virtual",     None;
         "when",        None;
         "while",       None
        ]

let add map (key,value) = String.Map.add key value map

let kwd_map = List.fold_left add String.Map.empty keywords

(* Resetting file name and line number (according to #line directives) *)

let reset_file ~file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line lnum buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_lnum = lnum}

let reset ~file ?(line=1) buffer =
  reset_file ~file buffer; reset_line line buffer

(* Hack to roll back one lexeme in the current semantic action *)
(*
let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <-
    {lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - len}
*)

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Auxiliary regular expressions *)

let newline    = '\n' | '\r' | "\r\n"
let blank      = ' ' | '\t'
let lowercase  = ['a'-'z' '_']
let uppercase  = ['A'-'Z' '_']
let letter     = lowercase | uppercase
let ident_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let ident      = lowercase ident_char*
let uident     = uppercase ident_char*
let digit      = ['0'-'9']
let integer    = '0' | ['1'-'9'] digit*
let number     = integer
let escaped    = "\\n" | "\\\"" | "\\?" | "\\\\" | "\\a"
                 | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v"
                 | "\\0" digit digit
let str_char   = [^'"' '\\' '\n'] | escaped
let string     = str_char*

(* Rules *)

rule scan = parse
  newline { Lexing.new_line lexbuf; scan lexbuf }
| blank+  { scan lexbuf }

| "->" { Token.ARROW    }
| "::" { Token.CONS     }
| "^"  { Token.CAT      }
| "|"  { Token.BAR      }

| "="  { Token.EQ       }
| "<>" { Token.NE       }
| "<"  { Token.LT       }
| ">"  { Token.GT       }
| "<=" { Token.LE       }
| ">=" { Token.GE       }

| "&&" { Token.BOOL_AND }
| "||" { Token.BOOL_OR  }

| "-"  { Token.MINUS    }
| "+"  { Token.PLUS     }
| "/"  { Token.DIV      }
| "*"  { Token.MULT     }

| "("  { Token.LPAR     }
| ")"  { Token.RPAR     }
| "["  { Token.LBRACK   }
| "]"  { Token.RBRACK   }

| ","  { Token.COMMA    }
| ";"  { Token.SEMI     }

| "_"  { Token.WILD     }

| '"' (string as s) '"' { Token.Str s }

| '0' digit+ as n {
    let msg = Printf.sprintf "Leading zeros in %s are not allowed." n
    in error lexbuf msg
  }

| number as n  { Token.Int (Z.of_string n) }

| ident as id {
    match String.Map.find id kwd_map with
      None -> let msg =
               Printf.sprintf "Keyword \"%s\" reserved for future use." id
             in error lexbuf msg
    | Some kwd -> kwd
    | exception Not_found -> Token.Ident id
  }

| uident as id {
    match String.Map.find id kwd_map with
      None -> let msg =
               Printf.sprintf "Keyword \"%s\" reserved for future use." id
             in error lexbuf msg
    | Some kwd -> kwd
    | exception Not_found ->
        let msg =
          Printf.sprintf "Identifiers cannot start with an uppercase letter."
        in error lexbuf msg
  }

| "(*" { let start = Lexing.lexeme_start_p lexbuf
         and stop  = Lexing.lexeme_end_p   lexbuf
         in scan_comment start stop lexbuf;
         scan lexbuf }

| eof    { Token.EOF }
| _ as c { let msg = Printf.sprintf "Invalid character %c." c
           in error lexbuf msg }

(* Comments *)

and scan_comment start stop = parse
  "(*"    { let start' = Lexing.lexeme_start_p lexbuf
            and stop'  = Lexing.lexeme_end_p   lexbuf
            in scan_comment start' stop' lexbuf;
               scan_comment start  stop  lexbuf }
| "*)"    { () }
| newline { Lexing.new_line lexbuf;
            scan_comment start stop lexbuf }
| eof     { let region = Region.make ~start ~stop
            in raise (Error ("Open comment.", region)) }
| '"' string '"'
| _       { scan_comment start stop lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

type logger = out_channel * (out_channel -> Token.t -> unit)

let get_token ?log =
  match log with
    None -> scan
  | Some (out_chan, print) ->
      let print = print out_chan in
      fun buffer -> let t = scan buffer in print t; flush out_chan; t

(* Standalone lexer for debugging purposes *)

let format_error ~(kind: string) (msg, region) =
  Printf.sprintf "%s error in %s:\n%s%!"
    kind (Region.to_string region) msg

let prerr ~(kind: string) msg =
  highlight (format_error ~kind msg)

type file_path = string

let output_token buffer chan token =
  let open Lexing in
  let conc = Token.to_string token in
  let start_pos = buffer.lex_start_p
  and curr_pos  = buffer.lex_curr_p
  in Printf.fprintf chan "%s-%s: %s\n%!"
       (Pos.compact start_pos) (Pos.compact curr_pos) conc

let iter action file_opt =
  try
    let cin, reset =
      match file_opt with
        None | Some "-" -> stdin, fun ?(line=1) _buffer -> ignore line
      |       Some file -> open_in file, reset ~file in
    let buffer = Lexing.from_channel cin in
    let rec iter () =
      try
        let t = scan buffer in
        action buffer stdout t;
        if t = Token.EOF then (close_in cin; close_out stdout) else iter ()
      with Error diag ->
             close_in cin; close_out stdout; prerr ~kind:"Lexical" diag
    in reset buffer; iter ()
  with Sys_error msg -> highlight msg

let trace = iter output_token
(* END TRAILER *)
}
