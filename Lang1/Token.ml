(* Abstract lexical tokens for Mini-ML *)

type t =
  (* Symbols *)

  ARROW
| VBAR
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
| End
| False
| Fun
| If
| In
| Let
| Match
| Mod
| Not
| Rec
| Then
| True
| With

(* Virtual tokens *)

| EOF (* End of file       *)

type token = t

let to_string = function
  ARROW    -> "->"
| VBAR     -> "|"
| CONS     -> "::"
| CAT      -> "^"
| MINUS    -> "-"
| PLUS     -> "+"
| DIV      -> "/"
| MULT     -> "*"
| LPAR     -> "("
| RPAR     -> ")"
| LBRACK   -> "["
| RBRACK   -> "]"
| COMMA    -> ","
| SEMI     -> ";"
| WILD     -> "_"
| EQ       -> "="
| NE       -> "<>"
| LT       -> "<"
| GT       -> ">"
| LE       -> "<="
| GE       -> ">="
| BOOL_OR  -> "||"
| BOOL_AND -> "&&"
| Ident id -> Printf.sprintf "Ident %s"   id
| Int n    -> Printf.sprintf "Int %s"     (Z.to_string n)
| Str n    -> Printf.sprintf "Str \"%s\"" n
| And      -> "and"
| Else     -> "else"
| End      -> "end"
| False    -> "false"
| Fun      -> "fun"
| If       -> "if"
| In       -> "in"
| Let      -> "let"
| Match    -> "match"
| Mod      -> "mod"
| Not      -> "not"
| Rec      -> "rec"
| Then     -> "then"
| True     -> "true"
| With     -> "with"
| EOF      -> "EOF"
