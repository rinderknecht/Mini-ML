(* Regions of the input file *)

type t = {start: Pos.t; stop: Pos.t}
type region = t

(* Injections *)

exception Invalid

let make ~start ~stop =
  let open Lexing in
  if start.pos_fname <> stop.pos_fname
  || start.pos_cnum > stop.pos_cnum then raise Invalid
  else {start; stop}

let ghost = {start = Lexing.dummy_pos; stop = Lexing.dummy_pos}

(* Projections *)

let start_pos {start; _} = start
let stop_pos  {stop;  _} = stop

let start_loc {start; _} = Loc.from_pos start
let stop_loc  {stop;  _} = Loc.from_pos stop

let file {start; _} = start.Lexing.pos_fname

let pos  {start; stop} = start, stop
let locs {start; stop} = Loc.(from_pos start, from_pos stop)

(* Comparisons *)

let leq r1 r2 = r1.start <= r2.start

(* Predicates *)

let is_ghost = (=) ghost

(* Conversions to [string] *)

let to_string ?(file=true) {start; stop} =
  let open Lexing in
  let start_col_pos = start.pos_cnum - start.pos_bol
  and stop_col_pos  = stop.pos_cnum - stop.pos_bol in
  let info =
    if file then
      Printf.sprintf "file \"%s\", line %i, char" start.pos_fname start.pos_lnum
    else Printf.sprintf "line %i, char" start.pos_lnum
  in if stop.pos_lnum = start.pos_lnum
     then Printf.sprintf "%sacters %i-%i" info start_col_pos stop_col_pos
     else Printf.sprintf "%s %i to line %i, char %i"
                         info start_col_pos stop.pos_lnum stop_col_pos

let anon_pos ?(offsets=true) pos =
  if pos = Lexing.dummy_pos then "ghost"
  else Printf.sprintf "%i:%i"
        (Pos.get_line pos)
        Pos.((if offsets then get_offset else get_column) pos)

let compact ?(offsets=true) {start; stop} =
  let start_file = Pos.get_file start
  and stop_file  = Pos.get_file stop in
  if start_file = stop_file then
    Printf.sprintf "%s:%s-%s"
      start_file (anon_pos ~offsets start) (anon_pos ~offsets stop)
  else
    Printf.sprintf "%s:%s-%s:%s"
      start_file (anon_pos ~offsets start)
      stop_file  (anon_pos ~offsets stop)
