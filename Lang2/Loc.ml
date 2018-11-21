type t = {file: string; line: int; column: int}
type loc = t

(* Making locations *)

let make ~file ~line ~column =
  assert (line >= 1); assert (column >= 1); {file; line; column}

let from_pos p =
  Lexing.{file = p.pos_fname;
          line = p.pos_lnum;
          column = p.pos_cnum - p.pos_bol + 1}

let min ~file = {file; line=1; column=1}
let max ~file = {file; line=max_int; column=max_int}

(* Projections *)

let file   {file;   _} = file
let line   {line;   _} = line
let column {column; _} = column
let offset {column; _} = column - 1

(* Updates *)

let set_file loc ~file   = {loc with file}
let add_line loc ~offset = {loc with line = loc.line + offset}
let add_col  loc ~offset = {loc with column = loc.column + offset}

(* Comparisons *)

exception Incomparable of t * t

let lt loc1 loc2 =
  if loc1.file = loc2.file || loc1.file = "" || loc2.file = "" then
    loc1.line < loc2.line || loc1.line = loc2.line && loc1.column < loc2.column
  else raise (Incomparable (loc1, loc2))

let leq loc1 loc2 =
  if loc1.file = loc2.file || loc1.file = "" || loc2.file = "" then
    loc1.line < loc2.line || loc1.line = loc2.line && loc1.column <= loc2.column
  else raise (Incomparable (loc1, loc2))

let eq loc1 loc2 =
  if loc1.file = loc2.file || loc1.file = "" || loc2.file = "" then
    loc1.line = loc2.line && loc1.column = loc2.column
  else raise (Incomparable (loc1, loc2))

let compare loc1 loc2 =
  if loc1.file = loc2.file || loc1.file = "" || loc2.file = "" then
    match loc2.line - loc1.line with
          0 -> loc2.column - loc1.column
    | delta -> delta
  else raise (Incomparable (loc1, loc2))

(* Predicates *)

let is_min loc = loc.line = 1 && loc.column = 1
let is_max loc = loc.line = max_int && loc.column = max_int

(* Conversion to [string] *)

let to_string ?(emacs=true) {file; line; column} =
  Printf.sprintf "%s:%i:%i" file line (if emacs then column - 1 else column)
