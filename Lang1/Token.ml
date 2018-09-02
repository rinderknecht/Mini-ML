type syn_kind = V | Ge | Gf | Gate

type file = {start_pos: Pos.t; name: string}

type t =
  (* Symbols *)

| ONE_SYN
| MANY_SYN
| ARROW
| CONS
| CAT

| MINUS
| PLUS
| DIV
| MULT

| QMINUS
| QPLUS
| QDIV
| QMULT

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

| Ident  of string
| Label  of (Region.t * (Region.t * string))
| Int    of Z.t
| Frac   of (string * Q.t)
| String of string

(* Keywords in common with OCaml *)

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

(* Keywords specific to the STICK networks *)

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

let to_string = function
  ONE_SYN  -> "<-"
| MANY_SYN -> "<="
| ARROW    -> "->"
| CONS     -> "::"
| CAT      -> "^"

| MINUS -> "-"
| PLUS  -> "+"
| DIV   -> "/"
| MULT  -> "*"

| QMINUS  -> "-."
| QPLUS   -> "+."
| QDIV    -> "/."
| QMULT   -> "*."

| LPAR   -> "("
| RPAR   -> ")"
| LBRACK -> "["
| RBRACK -> "]"

| COMMA  -> ","
| SEMI   -> ";"

| WILD   -> "_"

| EQ -> "="
| NE -> "<>"
| LT -> "<"
| GT -> ">"
| LE -> "=<"
| GE -> ">="

| BOOL_OR  -> "||"
| BOOL_AND -> "&&"

| Ident id         -> Printf.sprintf "Ident %s"      id
| Int n            -> Printf.sprintf "Int %s"        (Z.to_string n)
| Frac (s,q  )     -> Printf.sprintf "Frac (%s,%s)"  s (Q.to_string q)
| String n         -> Printf.sprintf "String \"%s\"" n
| Label (_,(r,id)) -> Printf.sprintf "Label (%s,%s)" (Region.compact r) id

| And     -> "and"
| Else    -> "else"
| False   -> "false"
| Fun     -> "fun"
| If      -> "if"
| In      -> "in"
| Let     -> "let"
| Mod     -> "mod"
| Not     -> "not"
| Rec     -> "rec"
| Then    -> "then"
| True    -> "true"

| Net     -> "net"
| Fuse    -> "fuse"
| Input   -> "input"
| Output  -> "output"
| Node    -> "node"
| Gate    -> "Gate"
| Ge      -> "Ge"
| Gf      -> "Gf"
| V       -> "V"

| CPP_include {start_pos; name} ->
    Printf.sprintf "#include \"%s\" at line %d" name (Pos.get_line start_pos)

| CPP_line_2 {start_pos; name} ->
    Printf.sprintf "# %d \"%s\" 2" (Pos.get_line start_pos) name

| EOF -> "EOF"
