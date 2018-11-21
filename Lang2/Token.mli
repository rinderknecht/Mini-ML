(* Lexical tokens for Mini-ML *)

type t =
  (* Symbols *)

  ARROW    (* "->" *)
| BAR      (* "|"  *)
| CONS     (* "::" *)
| CAT      (* "^"  *)

  (* Arithmetics *)

| MINUS    (* "-" *)
| PLUS     (* "+" *)
| DIV      (* "/" *)
| MULT     (* "*" *)

  (* Compounds *)

| LPAR     (* "(" *)
| RPAR     (* ")" *)
| LBRACK   (* "[" *)
| RBRACK   (* "]" *)

  (* Separators *)

| COMMA    (* "," *)
| SEMI     (* ";" *)

  (* Wildcard *)

| WILD     (* "_" *)

  (* Comparisons *)

| EQ       (* "="  *)
| NE       (* "<>" *)
| LT       (* "<"  *)
| GT       (* ">"  *)
| LE       (* "=<" *)
| GE       (* ">=" *)

| BOOL_OR  (* "||" *)
| BOOL_AND (* "&&" *)

  (* Identifiers, labels, numbers and strings *)

| Ident of string
| Int   of Z.t
| Str   of string

  (* Keywords *)

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

| EOF (* End of file *)

type token = t

val to_string: t -> string
