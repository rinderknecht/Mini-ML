type t = Lexing.position
type pos = t

let to_string ?(offsets=true) p =
  let open Lexing in
  let offset = p.pos_cnum - p.pos_bol in
  Printf.sprintf "File \"%s\", line %i, character %i"
    p.pos_fname p.pos_lnum (if offsets then offset else offset + 1)

let compact ?(offsets=true) p =
  let open Lexing in
  if p = dummy_pos then "ghost"
  else
    let offset = p.pos_cnum - p.pos_bol in
    Printf.sprintf "%s:%i:%i"
      p.pos_fname p.pos_lnum (if offsets then offset else offset + 1)

let set_file file p = Lexing.{p with pos_fname = file}

let set_line line p = Lexing.{p with pos_lnum = line}

let get_file p = p.Lexing.pos_fname

let get_line p = p.Lexing.pos_lnum

let get_offset p = Lexing.(p.pos_cnum - p.pos_bol)

let get_column p = 1 + get_offset p

let get_line_offset p = p.Lexing.pos_bol

let get_char_offset p = p.Lexing.pos_cnum
