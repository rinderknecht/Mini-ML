(* Lexical tokens for Mini-ML *)

type file = {start_pos: Pos.t; name: string}

type t =
(* Symbols *)

  ARROW    (* "->" *)
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

| EOF                 (* End of file       *)

type token = t

val to_string: t -> string
