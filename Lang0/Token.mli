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

(* Sentinel *)

| EOF

val string_of_token : token -> string
