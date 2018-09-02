(* Lexical tokens for the STICK language *)

type syn_kind = V | Ge | Gf | Gate

type file = {start_pos: Pos.t; name: string}

type t =
(* Symbols *)

  ONE_SYN  (* "<-" *)
| MANY_SYN (* "<=" *)
| ARROW    (* "->" *)
| CONS     (* "::" *)
| CAT      (* "^"  *)

(* Arithmetics *)

| MINUS   (* "-" *)
| PLUS    (* "+" *)
| DIV     (* "/" *)
| MULT    (* "*" *)

| QMINUS  (* "-." *)
| QPLUS   (* "+." *)
| QDIV    (* "/." *)
| QMULT   (* "*." *)

(* Compounds *)

| LPAR    (* "(" *)
| RPAR    (* ")" *)
| LBRACK  (* "[" *)
| RBRACK  (* "]" *)

(* Separators *)

| COMMA   (* "," *)
| SEMI    (* ";" *)

(* Wildcard *)

| WILD    (* "_" *)

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

| Ident  of string
| Label  of (Region.t * (Region.t * string))
| Int    of Z.t
| Frac   of (string * Q.t)
| String of string

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

| Net
| Fuse
| Input
| Output
| Node
| Gate
| Ge
| Gf
| V

(* Virtual tokens *)

| CPP_include of file (* #include "file"   *)
| CPP_line_2  of file (* # <line> "file" 2 *)
| EOF                 (* End of file       *)

type token = t

val to_string: t -> string
