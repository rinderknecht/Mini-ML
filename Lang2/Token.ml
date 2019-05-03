(* Abstract lexical tokens for Mini-ML *)

type t =
  (* Symbols *)

  ARROW
| CONS
| CAT
| MINUS
| PLUS
| SLASH
| TIMES
| LPAR
| RPAR
| LBRACK
| RBRACK
| LBRACE
| RBRACE
| COMMA
| SEMI
| VBAR
| COLON
| DOT
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
| Constr of string
| Int    of Z.t
| Str    of string

(* Some keywords of OCaml *)

| And
| Begin
| Else
| End
| False
| Fun
| If
| In
| Let
| List
| Map
| Match
| Mod
| Not
| Of
| Rec
| Set
| Then
| True
| Type
| With

(* Virtual tokens *)

| EOF (* End of file *)

type token = t

let to_string = function
  ARROW    -> "->"
| CONS     -> "::"
| CAT      -> "^"
| MINUS    -> "-"
| PLUS     -> "+"
| SLASH    -> "/"
| TIMES    -> "*"
| LPAR     -> "("
| RPAR     -> ")"
| LBRACK   -> "["
| RBRACK   -> "]"
| LBRACE   -> "{"
| RBRACE   -> "}"
| COMMA    -> ","
| SEMI     -> ";"
| VBAR     -> "|"
| COLON    -> ":"
| DOT      -> "."
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
| Constr id -> Printf.sprintf "Constr %s" id
| Int n    -> Printf.sprintf "Int %s"     (Z.to_string n)
| Str n    -> Printf.sprintf "Str \"%s\"" n
| And      -> "and"
| Begin    -> "begin"
| Else     -> "else"
| End      -> "end"
| False    -> "false"
| Fun      -> "fun"
| If       -> "if"
| In       -> "in"
| Let      -> "let"
| List     -> "list"
| Map      -> "map"
| Match    -> "match"
| Mod      -> "mod"
| Not      -> "not"
| Of       -> "of"
| Rec      -> "rec"
| Set      -> "set"
| Then     -> "then"
| True     -> "true"
| Type     -> "type"
| With     -> "with"
| EOF      -> "EOF"
