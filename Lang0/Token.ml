type token =
  (* Symbols *)

  MINUS
| PLUS
| DIV
| MULT

| LPAR
| RPAR

| ARROW
| COMMA
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

| Ident  of string
| Nat    of string
| Str    of string

(* Keywords *)

| Let
| Rec
| And
| In
| Fun
| If
| Then
| Else
| True
| False
| Not

(* Sentinels *)

| EOF


let string_of_token = function
  MINUS -> "-"
| PLUS  -> "+"
| DIV   -> "/"
| MULT  -> "*"

| LPAR   -> "("
| RPAR   -> ")"

| ARROW  -> "->"
| COMMA  -> ","
| WILD   -> "_"

| EQ -> "="
| NE -> "<>"
| LT -> "<"
| GT -> ">"
| LE -> "<="
| GE -> ">="

| BOOL_AND -> "&&"
| BOOL_OR  -> "||"

| Ident id -> Printf.sprintf "Ident %s" id
| Nat n    -> Printf.sprintf "Nat %s" n
| Str n    -> Printf.sprintf "Str %s" n

| Let   -> "let"
| Rec   -> "rec"
| And   -> "and"
| In    -> "in"
| Fun   -> "fun"
| If    -> "if"
| Then  -> "then"
| Else  -> "else"
| True  -> "true"
| False -> "false"
| Not   -> "not"

| EOF -> "EOF"
