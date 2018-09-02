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

let compact {start; stop} =
  Printf.sprintf "%s-%s" (Pos.compact start) (Pos.compact stop)

(*
exception Crossfile of t * t

let merge r1 r2 =
  if is_ghost r1 then r2
  else if is_ghost r2 then r1
  else if r1.start.Pos.pos_fname <> r2.start.Pos.pos_fname
  then raise (Crossfile (r1, r2))
  else
    let start = if r1.start <= r2.start then r1.start else r2.start
    and stop  = if r1.stop <= r2.stop   then r2.stop  else r2.stop
    in {start; stop}
*)
