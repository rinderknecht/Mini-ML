(* Abstract lexical tokens for Mini-ML *)

type file = {start_pos: Pos.t; name: string}

type t =
  (* Symbols *)

  ARROW
| CONS
| CAT
| MINUS
| PLUS
| DIV
| MULT
| LPAR
| RPAR
| LBRACK
| RBRACK
| COMMA
| SEMI
| WILD
| EQ
| NE
| LT
| GT
| LE
| GE
| BOOL_OR
| BOOL_AND

(* Identifiers, numbers and strings *)

| Ident of string
| Int   of Z.t
| Str   of string

(* Some keywords of OCaml *)

| And
| Else
| False
| Fun
| If
| In
| Let
| Mod
| Not
| Rec
| Then
| True

(* Virtual tokens *)

| EOF (* End of file       *)

type token = t

let to_string = function
  ARROW      -> "->"
| CONS       -> "::"
| CAT        -> "^"
| MINUS      -> "-"
| PLUS       -> "+"
| DIV        -> "/"
| MULT       -> "*"
| LPAR       -> "("
| RPAR       -> ")"
| LBRACK     -> "["
| RBRACK     -> "]"
| COMMA      -> ","
| SEMI       -> ";"
| WILD       -> "_"
| EQ         -> "="
| NE         -> "<>"
| LT         -> "<"
| GT         -> ">"
| LE         -> "<="
| GE         -> ">="
| BOOL_OR    -> "||"
| BOOL_AND   -> "&&"
| Ident id   -> Printf.sprintf "Ident %s"      id
| Int n      -> Printf.sprintf "Int %s"        (Z.to_string n)
| Str n      -> Printf.sprintf "Str \"%s\"" n
| And        -> "and"
| Else       -> "else"
| False      -> "false"
| Fun        -> "fun"
| If         -> "if"
| In         -> "in"
| Let        -> "let"
| Mod        -> "mod"
| Not        -> "not"
| Rec        -> "rec"
| Then       -> "then"
| True       -> "true"
| EOF        -> "EOF"
