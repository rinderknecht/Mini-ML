type 'a nseq = 'a * 'a list

let nseq_to_list s = fst s :: snd s

let rec nseq_map f = function
  e,   [] -> f e, []
| e, h::t -> let h', t' = nseq_map f (h,t)
             in f e, h'::t'

let list_to_string sep to_string l =
  let apply b a =
    let s = to_string b in
    if a <> "" then s ^ sep ^ " " ^ a else s
  in List.fold_right apply l ""

let list_to_string  to_string = list_to_string ";" to_string
and tuple_to_string to_string = list_to_string "," to_string

let nseq_to_string to_string (h,t) =
  "[" ^ list_to_string to_string (h::t) ^ "]"

type var = string

type t = expr

and expr =
  LetIn    of let_bind nseq * expr
| LetRecIn of rec_bind nseq * expr
| Fun      of var * expr
| If       of expr * expr * expr
| Tuple    of bool_expr list
| BoolExpr of bool_expr

and let_bind = pattern * expr

and pattern =
  Pwild
| Pvar   of var
| Ptuple of pattern list

and rec_bind = rec_pattern * expr

and rec_pattern =
  RWild
| RVar of var

and bool_expr =
  Or       of bool_expr * conj_expr
| ConjExpr of conj_expr

and conj_expr =
  And      of conj_expr * comp_expr
| CompExpr of comp_expr

and comp_expr =
  Lt      of comp_expr * add_expr
| Le      of comp_expr * add_expr
| Gt      of comp_expr * add_expr
| Ge      of comp_expr * add_expr
| Eq      of comp_expr * add_expr
| Ne      of comp_expr * add_expr
| AddExpr of add_expr

and add_expr =
  Add      of add_expr * mult_expr
| Sub      of add_expr * mult_expr
| MultExpr of mult_expr

and mult_expr =
  Mult      of mult_expr * unary_expr
| Div       of mult_expr * unary_expr
| UnaryExpr of unary_expr

and unary_expr =
  Minus       of primary_expr
| PrimaryExpr of primary_expr

and primary_expr =
  Apply    of core_expr * core_expr nseq
| Not      of primary_expr
| CoreExpr of core_expr

and core_expr =
  Var    of var
| Nat    of int
| Str    of string
| Par    of expr
| True
| False

let mk_var x =
  BoolExpr (
    ConjExpr (
      CompExpr (
        AddExpr (MultExpr (UnaryExpr (PrimaryExpr (CoreExpr (Var x))))))))

let rec to_string = function
  LetIn (let_binds, expr) ->
    Printf.sprintf "LetIn (%s, %s)"
      (let_binds_to_string let_binds)
      (to_string expr)
| LetRecIn (rec_binds, expr) ->
    Printf.sprintf "LetRecIn ([%s], %s)"
      (rec_binds_to_string rec_binds)
      (to_string expr)
| Fun (var, expr) ->
    Printf.sprintf "Fun (%s, %s)"
      var
      (to_string expr)
| If (expr1, expr2, expr3) ->
    Printf.sprintf "If (%s, %s, %s)"
      (to_string expr1)
      (to_string expr2)
      (to_string expr3)
| Tuple exprs ->
    Printf.sprintf "(%s)"
      (tuple_to_string bool_to_string exprs)
| BoolExpr e ->
    bool_to_string e

and let_binds_to_string nseq =
  nseq_to_string let_bind_to_string nseq

and let_bind_to_string (pat, expr) =
  Printf.sprintf "(%s, %s)"
    (pattern_to_string pat)
    (to_string expr)

and pattern_to_string = function
  Pvar x       -> x
| Pwild        -> "_"
| Ptuple patts ->
    Printf.sprintf "(%s)" (tuple_to_string pattern_to_string patts)

and rec_binds_to_string nseq =
  nseq_to_string rec_bind_to_string nseq

and rec_bind_to_string (rpat, expr) =
  Printf.sprintf "(%s, %s)"
    (rec_pattern_to_string rpat)
    (to_string expr)

and rec_pattern_to_string = function
  RWild  -> "_"
| RVar x -> x

and bool_to_string = function
  Or (e1,e2) ->
    Printf.sprintf "Or (%s, %s)"
      (bool_to_string e1) (conj_to_string e2)
| ConjExpr e ->
    conj_to_string e

and conj_to_string = function
  And (e1,e2) ->
    Printf.sprintf "And (%s, %s)"
      (conj_to_string e1) (comp_to_string e2)
| CompExpr e ->
    comp_to_string e

and comp_to_string = function
  Lt (e1,e2) ->
    Printf.sprintf "%s < %s" (comp_to_string e1) (add_to_string e2)
| Le (e1,e2) ->
    Printf.sprintf "%s =< %s" (comp_to_string e1) (add_to_string e2)
| Gt (e1,e2) ->
    Printf.sprintf "%s > %s" (comp_to_string e1) (add_to_string e2)
| Ge (e1,e2) ->
    Printf.sprintf "%s >= %s" (comp_to_string e1) (add_to_string e2)
| Eq (e1,e2) ->
    Printf.sprintf "%s = %s" (comp_to_string e1) (add_to_string e2)
| Ne (e1,e2) ->
    Printf.sprintf "%s <> %s" (comp_to_string e1) (add_to_string e2)
| AddExpr e ->
    add_to_string e

and add_to_string = function
 Add (e1,e2) ->
   Printf.sprintf "Add (%s, %s)"
     (add_to_string e1)
     (mult_to_string e2)
| Sub (e1,e2) ->
   Printf.sprintf "Sub (%s, %s)"
     (add_to_string e1)
     (mult_to_string e2)
| MultExpr e ->
    mult_to_string e

and mult_to_string = function
  Mult (e1,e2) ->
    Printf.sprintf "Mult (%s, %s)"
      (mult_to_string e1)
      (unary_to_string e2)
| Div (e1,e2) ->
    Printf.sprintf "Div (%s, %s)"
      (mult_to_string e1)
      (unary_to_string e2)
| UnaryExpr e ->
    unary_to_string e

and unary_to_string = function
  Minus e ->
    Printf.sprintf "-%s"
      (primary_to_string e)
| PrimaryExpr e ->
     primary_to_string e

and primary_to_string = function
  Apply (f,args) ->
    Printf.sprintf "Apply (%s, %s)"
      (core_expr_to_string f)
      (nseq_to_string core_expr_to_string args)
| Not e ->
    Printf.sprintf "not %s" (primary_to_string e)
| CoreExpr e ->
    core_expr_to_string e

and core_expr_to_string = function
  Var x        -> x
| Nat n        -> string_of_int n
| Str s        -> Printf.sprintf "\"%s\"" s
| Par e        -> Printf.sprintf "(%s)"  (to_string e)
| True         -> "true"
| False        -> "false"

