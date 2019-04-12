(* Abstract Syntax Tree (AST) for Mini-ML *)

open! Utils

(* Regions *)

type 'a reg = Region.t * 'a

(* Keywords of OCaml *)

type kwd_and   = Region.t
type kwd_else  = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_match = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_rec   = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t
type kwd_with  = Region.t

(* Symbols *)

type arrow    = Region.t  (* "->" *)
type cons     = Region.t  (* "::" *)
type cat      = Region.t  (* "^"  *)

(* Arithmetic operators *)

type minus    = Region.t  (* "-" *)
type plus     = Region.t  (* "+" *)
type div      = Region.t  (* "/" *)
type mult     = Region.t  (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t  (* "||" *)
type bool_and = Region.t  (* "&&" *)

(* Comparisons *)

type eq = Region.t  (* "="  *)
type ne = Region.t  (* "<>" *)
type lt = Region.t  (* "<"  *)
type gt = Region.t  (* ">"  *)
type le = Region.t  (* "=<" *)
type ge = Region.t  (* ">=" *)

(* Compounds *)

type lpar = Region.t  (* "(" *)
type rpar = Region.t  (* ")" *)
type lbra = Region.t  (* "[" *)
type rbra = Region.t  (* "]" *)

(* Separators *)

type comma = Region.t  (* "," *)
type semi  = Region.t  (* ";" *)
type bar   = Region.t  (* "|" *)

(* Wildcard *)

type wild = Region.t  (* "_" *)

(* Literals *)

type var = string

(* Non-empty comma-separated values *)

type 'a csv = ('a, comma) nsepseq

(* Bar-separated non-empty lists *)

type 'a bsv = ('a, bar) nsepseq

(* Semicolon-separated lists (possibly empty) *)

type 'a ssv = ('a, semi) sepseq

(* Parentheses *)

type 'a par = lpar * 'a * rpar

type unit__ = lpar * rpar

(* Brackets compounds *)

type 'a bra = lbra * 'a * rbra

(* The Abstract Syntax Tree *)

type t  = statement list * eof
and ast = t

and eof = Region.t

and statement =
  Let     of (kwd_let           * let_bindings) reg
| LetRec  of (kwd_let * kwd_rec * let_rec_bindings) reg

and let_bindings = (let_binding, kwd_and) nsepseq

and let_binding = pattern * eq * expr

and let_rec_bindings = (let_rec_binding,  kwd_and) nsepseq

and let_rec_binding  = var reg * eq * fun_expr

and pattern =
  Ptuple of pattern csv reg
| Plist  of pattern ssv bra reg
| Pvar   of var reg
| Punit  of unit__ reg
| Pint   of Z.t reg
| Ptrue  of kwd_true
| Pfalse of kwd_false
| Pstr   of string reg
| Pwild  of wild
| Pcons  of (pattern * cons * pattern) reg
| Ppar   of pattern par reg

and expr =
  LetExpr of let_expr reg       (* let [rec] p1 = e1 and p2 = e2 and ... in e *)
| Fun     of fun_expr           (* fun x -> e                                 *)
| If      of conditional reg    (* if e1 then e2 else e3                      *)
| Tuple   of expr csv reg       (* e1, e2, ...                                *)
| Match   of match_expr reg     (* p1 -> e1 | p2 -> e2 | ...                  *)

| Cat     of (expr * cat * expr) reg                              (* e1  ^ e2 *)
| Cons    of (expr * cons * expr) reg                             (* e1 :: e2 *)

| Or      of (expr * bool_or * expr) reg                          (* e1 || e2 *)
| And     of (expr * bool_and * expr) reg                         (* e1 && e2 *)

| Lt      of (expr * lt * expr) reg                               (* e1  < e2 *)
| LEq     of (expr * le * expr) reg                               (* e1 <= e2 *)
| Gt      of (expr * gt * expr) reg                               (* e1  > e2 *)
| GEq     of (expr * ge * expr) reg                               (* e1 >= e2 *)
| NEq     of (expr * ne * expr) reg                               (* e1 <> e2 *)
| Eq      of (expr * eq * expr) reg                               (* e1  = e2 *)

| Add     of (expr * plus   * expr) reg                           (* e1  + e2 *)
| Sub     of (expr * minus  * expr) reg                           (* e1  - e2 *)

| Mult    of (expr * mult    * expr) reg                         (* e1  *  e2 *)
| Div     of (expr * div     * expr) reg                         (* e1  /  e2 *)
| Mod     of (expr * kwd_mod * expr) reg                         (* e1 mod e2 *)

| Neg     of (minus   * expr) reg                                    (*    -e *)
| Not     of (kwd_not * expr) reg                                    (* not e *)

| Call    of (expr * expr) reg                                         (* f e *)

| Int     of Z.t reg                                         (* 12345         *)
| Var     of var reg                                         (* x             *)
| Str     of string reg                                      (* "foo"         *)
| Unit    of unit__ reg                                      (* ()            *)
| True    of kwd_true                                        (* true          *)
| False   of kwd_false                                       (* false         *)
| Par     of expr par reg                                    (* (e)           *)
| List    of expr ssv bra reg                                (* [e1; e2; ...] *)
| Extern  of extern

and match_expr = kwd_match * expr * kwd_with * cases

and cases = (pattern * arrow * expr) bsv

and let_expr =
  LetIn    of kwd_let           * let_bindings     * kwd_in * expr
| LetRecIn of kwd_let * kwd_rec * let_rec_bindings * kwd_in * expr

and fun_expr = (kwd_fun * var reg * arrow * expr) reg

and conditional = kwd_if * expr * kwd_then * expr * kwd_else * expr

and extern =
  Cast   of cast_expr
| Print  of print_expr
| Scanf  of scanf_expr
| PolyEq of (var * var)

and cast_expr =
  StringOfInt  of var
| StringOfBool of var

and print_expr =
  PrintString of var
| PrintInt    of var

and scanf_expr =
  ScanfString of var
| ScanfInt    of var
| ScanfBool   of var

(* Conversions to strings *)

let sprintf = Printf.sprintf

let tuple_to_string to_string csv =
  let apply b a =
    let s = to_string b in
    if a = "" then s else sprintf "%s, %s" s a
  in Utils.nsepseq_foldr apply csv ""

let list_to_string to_string ssv =
  let apply e a =
    let s = to_string e in
    if a = "" then s else sprintf "%s; %s" s a
  in sprintf "[%s]" (Utils.sepseq_foldr apply ssv "")

(*
let seq_to_string to_string seq =
  let apply e a =
    let s = to_string e in
    if a = "" then s else sprintf "%s; %s" s a
  in sprintf "[%s]" (List.fold_right apply seq "")

let nseq_to_string to_string (item, seq) =
  seq_to_string to_string (item::seq)
*)

let rec to_string (statements,_) =
  let apply statement acc =
    let str = statement_to_string statement in
    if acc = "" then str else sprintf "%s\n%s" str acc
  in List.fold_right apply statements ""

and statement_to_string = function
  Let (_,(_,let_bindings)) ->
    sprintf "Let %s" (let_bindings_to_string let_bindings)
| LetRec (_,(_,_,let_rec_bindings)) ->
    sprintf "LetRec %s" (let_rec_bindings_to_string let_rec_bindings)

and let_rec_bindings_to_string bindings =
  let apply binding acc =
    let str = let_rec_binding_to_string binding in
    if acc = "" then str else sprintf "%s\nAnd %s" acc str
  in Utils.nsepseq_foldr apply bindings ""

and let_bindings_to_string bindings =
  let apply binding acc =
    let str = let_binding_to_string binding in
    if acc = "" then str else sprintf "%s\nAnd %s" acc str
  in Utils.nsepseq_foldr apply bindings ""

and let_binding_to_string (pattern, _, expr) =
  sprintf "%s = %s" (pattern_to_string pattern) (expr_to_string expr)

and let_rec_binding_to_string ((_,var), _, expr) =
  sprintf "%s = %s" var (fun_expr_to_string expr)

and expr_to_string = function
        LetExpr (_,expr) -> let_expr_to_string expr
|        Tuple (_,exprs) -> tuple_to_string (expr_to_string) exprs
|         Match (_,expr) -> match_expr_to_string expr
|               Fun expr -> fun_expr_to_string expr
|              Int (_,z) -> Z.to_string z
|              Var (_,x) -> x
|              Str (_,s) -> sprintf "\"%s\"" s
|                False _ -> "false"
|                 True _ -> "true"
|                 Unit _ -> "()"
|        Par (_,(_,e,_)) -> sprintf "(%s)" (expr_to_string e)
|       List (_,(_,l,_)) -> list_to_string expr_to_string l
|               Extern e -> extern_to_string e
|          Neg (_,(_,e)) -> sprintf "-%s"    (expr_to_string e)
|          Not (_,(_,e)) -> sprintf "not %s" (expr_to_string e)
| If (_,(_,e,_,e1,_,e2)) ->
    sprintf "If (%s, %s, %s)"
      (expr_to_string e) (expr_to_string e1) (expr_to_string e2)
| Cat (_,(arg1,_,arg2)) ->
    sprintf "%s ^ %s" (expr_to_string arg1) (expr_to_string arg2)
| Cons (_,(hd,_,tl)) ->
    sprintf "%s::%s"(expr_to_string hd) (expr_to_string tl)
| Or (_,(e1,_,e2)) ->
    sprintf "%s || %s" (expr_to_string e1) (expr_to_string e2)
| And (_,(e1,_,e2)) ->
    sprintf "%s && %s" (expr_to_string e1) (expr_to_string e2)
| Lt (_,(e1,_,e2)) ->
    sprintf "%s < %s" (expr_to_string e1) (expr_to_string e2)
| LEq (_,(e1,_,e2)) ->
    sprintf "%s =< %s" (expr_to_string e1) (expr_to_string e2)
| Gt (_,(e1,_,e2)) ->
    sprintf "%s > %s" (expr_to_string e1) (expr_to_string e2)
| GEq (_,(e1,_,e2)) ->
    sprintf "%s >= %s" (expr_to_string e1) (expr_to_string e2)
| Eq (_,(e1,_,e2)) ->
    sprintf "%s = %s" (expr_to_string e1) (expr_to_string e2)
| NEq (_,(e1,_,e2)) ->
    sprintf "%s <> %s" (expr_to_string e1) (expr_to_string e2)
| Add (_,(e1,_,e2)) ->
    sprintf "Add (%s, %s)" (expr_to_string e1) (expr_to_string e2)
| Sub (_,(e1,_,e2)) ->
    sprintf "Sub (%s, %s)" (expr_to_string e1) (expr_to_string e2)
| Mult (_,(e1,_,e2)) ->
    sprintf "Mult (%s, %s)" (expr_to_string e1) (expr_to_string e2)
| Div (_,(e1,_,e2)) ->
    sprintf "Div (%s, %s)" (expr_to_string e1) (expr_to_string e2)
| Mod (_,(e1,_,e2)) ->
    sprintf "Mod (%s, %s)" (expr_to_string e1) (expr_to_string e2)
| Call (_,(func,arg)) ->
    sprintf "Call (%s, %s)" (expr_to_string func) (expr_to_string arg)

and match_expr_to_string (_,expr,_,cases) =
  sprintf "Match (%s, %s)" (expr_to_string expr) (cases_to_string cases)

and cases_to_string cases =
  let apply case acc =
    let str = case_to_string case in
    if acc = "" then str else sprintf "%s\n| %s" acc str
  in Utils.nsepseq_foldr apply cases ""

and case_to_string (pattern, _arrow, expr) =
  sprintf "%s -> %s" (pattern_to_string pattern) (expr_to_string expr)

and fun_expr_to_string (_,(_,(_,var),_,expr)) =
  sprintf "Fun (%s, %s)" var (expr_to_string expr)

and let_expr_to_string = function
  LetIn (_,bindings,_, e) ->
    sprintf "LetIn ([%s], %s)"
      (let_bindings_to_string bindings) (expr_to_string e)
| LetRecIn (_,_,bindings,_, e) ->
    sprintf "LetRecIn ([%s], %s)"
      (let_rec_bindings_to_string bindings) (expr_to_string e)

and pattern_to_string = function
  Ptuple (_,p)        -> tuple_to_string pattern_to_string p
| Plist  (_,(_,p,_))  -> list_to_string pattern_to_string p
| Pvar (_,var)        -> var
| Ppar (_,(_,p,_))    -> sprintf "(%s)" (pattern_to_string p)
| Punit _             -> "()"
| Pint (_,z)          -> Z.to_string z
| Ptrue _             -> "true"
| Pfalse _            -> "false"
| Pstr (_,s)          -> sprintf "\"%s\"" s
| Pwild _             -> "_"
| Pcons (_,(p1,_,p2)) -> sprintf "(%s::%s)"
                          (pattern_to_string p1) (pattern_to_string p2)

and extern_to_string = function
    Cast e -> cast_to_string   e
|  Print e -> print_to_string  e
|  Scanf e -> scanf_to_string  e
| PolyEq e -> polyeq_to_string e

and polyeq_to_string (x,y) = sprintf "equal %s %s" x y

and print_to_string = function
  PrintString var -> sprintf "print_string %s" var
| PrintInt    var -> sprintf "print_int %s"    var

and cast_to_string = function
  StringOfInt  var -> sprintf "string_of_int %s"  var
| StringOfBool var -> sprintf "string_of_bool %s" var

and scanf_to_string = function
  ScanfInt    var -> sprintf "scanf_int %s"    var
| ScanfString var -> sprintf "scanf_string %s" var
| ScanfBool   var -> sprintf "scanf_bool %s"   var

(* Projecting regions of the input source code *)

let region_of_pattern = function
  Plist (r,_) | Ptuple (r,_) | Pvar (r,_)
| Punit (r,_) | Pint (r,_) | Ptrue r | Pfalse r
| Pstr (r,_) | Pwild r | Pcons (r,_) | Ppar (r,_) -> r

let region_of_expr = function
  LetExpr (r,_) | Fun (r,_) | If (r,_) | Tuple (r,_) | Match (r,_)
| Cat (r,_) | Cons (r,_) | Or (r,_) | And (r,_) | Lt (r,_) | LEq (r,_)
| Gt (r,_) | GEq (r,_) | NEq (r,_) | Eq (r,_) | Add (r,_) | Sub (r,_)
| Mult (r,_) | Div (r,_) | Mod (r,_) | Neg (r,_) | Not (r,_) | Call (r,_)
| Int (r,_) | Var (r,_) | Str (r,_) | Unit (r,_) | True r | False r
| Par (r,_) | List (r,_) -> r
| Extern _ -> Region.ghost

(* Predicates *)

let rec is_var = function
  Par (_,(_,e,_)) -> is_var e
|           Var _ -> true
|               _ -> false

let rec is_call = function
  Par (_,(_,e,_)) -> is_call e
|          Call _ -> true
|               _ -> false

let rec is_fun = function
  Par (_,(_,e,_)) -> is_fun e
|           Fun _ -> true
|               _ -> false

let rec rm_par = function
  Par (_,(_,e,_)) -> rm_par e
|               e -> e

(* Rewriting let-expressions and fun-expressions, with some optimisations *)

type sep = Region.t

let ghost_fun, ghost_arrow, ghost_let, ghost_eq, ghost_in =
  Region.(ghost, ghost, ghost, ghost, ghost)

let norm_fun reg kwd_fun pattern eq expr =
  reg,
  match pattern with
    Pvar v -> kwd_fun, v, eq, expr
  |      _ -> let open Region in
              let fresh    = ghost, gen_sym () in
              let bindings = (pattern, eq, Var fresh), [] in
              let let_in   = LetIn (ghost_let, bindings, ghost_in, expr) in
              let expr     = LetExpr (ghost, let_in)
              in kwd_fun, fresh, ghost_arrow, expr

let norm ?reg (pattern, patterns) sep expr =
  let open Region in
  let reg, fun_reg =
    match reg with
        None -> ghost, ghost_fun
    | Some p -> p in
  let apply pattern (sep, expr) =
    ghost_eq, Fun (norm_fun ghost ghost_fun pattern sep expr) in
  let sep, expr = List.fold_right apply patterns (sep, expr)
  in norm_fun reg fun_reg pattern sep expr

(* Unparsing expressions *)

type unparsed = [
  `Fun  of (kwd_fun * (pattern nseq * arrow * expr))
| `Let  of (pattern nseq * eq * expr)
| `Idem of expr
]

(* The function [unparse'] returns a triple [patterns,
   separator_region, expression], and the context (handled by
   [unparse]) decides if [separator_region] is the region of a "="
   sign or "->". *)

let rec unparse' = function
  Fun (_,(_, var, arrow, expr)) ->
    if Region.is_ghost (fst var) then
      match expr with
        LetExpr (_, LetIn (_,((pattern,eq,_),[]),_,expr)) ->
          if Region.is_ghost eq then
            let patterns, sep, e = unparse' expr
            in Utils.nseq_cons pattern patterns, sep, e
          else (pattern,[]), eq, expr
      | _ -> assert false
    else if Region.is_ghost arrow then
           let patterns, sep, e = unparse' expr
           in Utils.nseq_cons (Pvar var) patterns, sep, e
         else (Pvar var, []), arrow, expr
| _ -> assert false

let unparse = function
  Fun (_,(kwd_fun,_,_,_)) as e ->
    let binding = unparse' e in
    if Region.is_ghost kwd_fun then `Let binding else `Fun (kwd_fun, binding)
| e -> `Idem e

(* Printing the tokens with their source locations *)

let print_nsepseq sep print (head,tail) =
  let print_aux (sep_reg, item) =
    Printf.printf "%s: %s\n" (Region.compact sep_reg) sep;
    print item
  in print head; List.iter print_aux tail

let print_sepseq sep print = function
      None -> ()
| Some seq -> print_nsepseq sep print seq

let print_csv print = print_nsepseq "," print
let print_bsv print = print_nsepseq "|" print
let print_ssv print = print_sepseq  ";" print

let print_token reg conc =
  Printf.printf "%s: %s\n" (Region.compact reg) conc

let print_var (reg,var) =
  Printf.printf "%s: Ident %s\n" (Region.compact reg) var

let print_str (reg,str) =
  Printf.printf "%s: Str \"%s\"\n" (Region.compact reg) str

let rec print_tokens ?(undo=false) (statements,eof) =
  List.iter (print_statement undo) statements; print_token eof "EOF"

and print_statement undo = function
  Let (_,(kwd_let, let_bindings)) ->
    print_token kwd_let "let";
    print_let_bindings undo let_bindings
| LetRec (_,(kwd_let, kwd_rec, let_rec_bindings)) ->
    print_token kwd_let "let";
    print_token kwd_rec "rec";
    print_let_rec_bindings undo let_rec_bindings

and print_let_bindings undo = print_nsepseq "and" (print_let_binding undo)

and print_let_binding undo (pattern,eq,expr) =
  print_pattern pattern;
  if undo then
    match unparse expr with
      `Let (patterns, eq, e) ->
         Utils.nseq_iter print_pattern patterns;
         print_token eq "=";
         print_expr undo e
    | `Fun (kwd_fun, (patterns, arrow, e)) ->
         print_token eq "=";
         print_token kwd_fun "fun";
         Utils.nseq_iter print_pattern patterns;
         print_token arrow "->";
         print_expr undo e
    | `Idem _ ->
         print_token eq "="; print_expr undo expr
  else (print_token eq "="; print_expr undo expr)

and print_let_rec_bindings undo bindings =
  print_nsepseq "and" (print_let_rec_binding undo) bindings

and print_let_rec_binding undo (var,eq,expr) =
  print_let_binding undo (Pvar var, eq, Fun expr)

and print_pattern = function
  Ptuple (_,patterns) -> print_csv print_pattern patterns
| Plist (_,(lbra,patterns,rbra)) ->
    print_token lbra "[";
    print_ssv print_pattern patterns;
    print_token rbra "]"
| Pvar (reg,var) -> Printf.printf "%s: Pvar %s\n" (Region.compact reg) var
| Punit (_,(lpar,rpar)) -> print_token lpar "("; print_token rpar ")"
| Pint (reg,z) -> print_token reg (sprintf "Int %s" (Z.to_string z))
| Ptrue kwd_true -> print_token kwd_true "true"
| Pfalse kwd_false -> print_token kwd_false "false"
| Pstr s -> print_str s
| Pwild wild -> print_token wild "_"
| Pcons (_,(p1,c,p2)) -> print_pattern p1; print_token c "::"; print_pattern p2
| Ppar (_,(lpar,p,rpar)) ->
    print_token lpar "("; print_pattern p; print_token rpar ")"

and print_expr undo = function
  LetExpr (_,e) -> print_let_expr undo e
|      If (_,e) -> print_conditional undo e
| Tuple (_,csv) -> print_csv (print_expr undo) csv
|   Match (_,e) -> print_match_expr undo e
| Fun (_,((kwd_fun,_,_,_) as f)) as e ->
    if undo then
      let patterns, arrow, expr = unparse' e in
      print_token kwd_fun "fun";
      Utils.nseq_iter print_pattern patterns;
      print_token arrow "->";
      print_expr undo expr
    else print_fun_expr undo f
| Cat (_,(e1,cons,e2)) ->
    print_expr undo e1; print_token cons "^"; print_expr undo e2
| Cons (_,(e1,cons,e2)) ->
    print_expr undo e1; print_token cons "::"; print_expr undo e2
| Or (_,(e1,bool_or,e2)) ->
    print_expr undo e1; print_token bool_or "||"; print_expr undo e2
| And (_,(e1,bool_and,e2)) ->
    print_expr undo e1; print_token bool_and "&&"; print_expr undo e2
| Lt (_,(e1,lt,e2)) ->
    print_expr undo e1; print_token lt "<"; print_expr undo e2
| LEq (_,(e1,le,e2)) ->
    print_expr undo e1; print_token le "<="; print_expr undo e2
| Gt (_,(e1,gt,e2)) ->
    print_expr undo e1; print_token gt ">"; print_expr undo e2
| GEq (_,(e1,ge,e2)) ->
    print_expr undo e1; print_token ge ">="; print_expr undo e2
| NEq (_,(e1,ne,e2)) ->
    print_expr undo e1; print_token ne "<>"; print_expr undo e2
| Eq (_,(e1,eq,e2)) ->
    print_expr undo e1; print_token eq "="; print_expr undo e2
| Add (_,(e1,plus,e2)) ->
    print_expr undo e1; print_token plus "+"; print_expr undo e2
| Sub (_,(e1,minus,e2)) ->
    print_expr undo e1; print_token minus "-"; print_expr undo e2
| Mult (_,(e1,mult,e2)) ->
    print_expr undo e1; print_token mult "*"; print_expr undo e2
| Div (_,(e1,div,e2)) ->
    print_expr undo e1; print_token div "/"; print_expr undo e2
| Mod (_,(e1,kwd_mod,e2)) ->
    print_expr undo e1; print_token kwd_mod "mod"; print_expr undo e2
| Neg (_,(minus,e)) -> print_token minus "-"; print_expr undo e
| Not (_,(kwd_not,e)) -> print_token kwd_not "not"; print_expr undo e
| Call (_,(e1,e2)) -> print_expr undo e1; print_expr undo e2
| Int (r,z) -> print_token r (sprintf "Int %s" (Z.to_string z))
| Var v -> print_var v
| Str s -> print_str s
| Unit (_,(lpar,rpar)) ->
    print_token lpar "("; print_token rpar ")"
| True kwd_true -> print_token kwd_true "true"
| False kwd_false -> print_token kwd_false "false"
| Par (_,(lpar,e,rpar)) ->
    print_token lpar "("; print_expr undo e; print_token rpar ")"
| List (_,(lbra,ssv,rbra)) ->
    print_token lbra "["; print_ssv (print_expr undo) ssv; print_token rbra "]"
| Extern _ -> ()

and print_match_expr undo (kwd_match, expr, kwd_with, cases) =
  print_token kwd_match "match";
  print_expr undo expr;
  print_token kwd_with "with";
  print_bsv (print_case undo) cases;
  print_token Region.ghost "end"

and print_case undo (pattern, arrow, expr) =
  print_pattern pattern;
  print_token arrow "->";
  print_expr undo expr

and print_let_expr undo = function
  LetIn (kwd_let, let_bindings, kwd_in, expr) ->
    print_token kwd_let "let";
    print_let_bindings undo let_bindings;
    print_token kwd_in "in";
    print_expr undo expr
| LetRecIn (kwd_let, kwd_rec, let_rec_bindings, kwd_in, expr) ->
    print_token kwd_let "let";
    print_token kwd_rec "rec";
    print_let_rec_bindings undo let_rec_bindings;
    print_token kwd_in "in";
    print_expr undo expr

and print_fun_expr undo (kwd_fun, rvar, arrow, expr) =
  print_token kwd_fun "fun";
  print_var rvar;
  print_token arrow "->";
  print_expr undo expr

and print_conditional undo (kwd_if, e1, kwd_then, e2, kwd_else, e3) =
  print_token kwd_if "if";
  print_expr undo e1;
  print_token kwd_then "then";
  print_expr undo e2;
  print_token kwd_else "else";
  print_expr undo e3

(* Variables (free and bound) *)

type rvar = var reg

module FreeVars =
  Set.Make (struct type t = rvar let compare = compare end)

module Vars = String.Set

(* Here, the environment is a set of variables with their locations,
   _not_ a value of type [Env.t]). On the same register, keep in mind
   also that the parameters [state] below are _not_ of type [State.t],
   but, instead, are a pair made of the current environment and the
   current set of free variables (only the latter is threaded, whilst
   the former flows top-down only). *)

let rec vars env (statements,_) =
  List.fold_left fv_statement (env, FreeVars.empty) statements

and fv_statement state = function
  Let    (_,(_,  bindings)) -> fv_let_bindings state bindings
| LetRec (_,(_,_,bindings)) -> fv_let_rec_bindings state bindings

and fv_let_bindings (env, _ as state) bindings =
  nsepseq_foldl (fv_let_binding env) state bindings

and fv_let_binding env (env',fv) (pattern,_,expr) =
  Vars.union (pattern_vars Vars.empty pattern) env', fv_expr env fv expr

and fv_let_rec_bindings (env, fv) bindings =
  let env' =
    nsepseq_foldl (fun a ((_,v),_,_) -> Vars.add v a) env bindings
  in env', nsepseq_foldl (fv_let_rec_binding env') fv bindings

and fv_let_rec_binding env fv (_,_,expr) = fv_expr env fv (Fun expr)

and pattern_vars fv = function
  Ptuple (_,patterns)         -> nsepseq_foldl pattern_vars fv patterns
| Plist (_,(_,patterns,_))    -> sepseq_foldl  pattern_vars fv patterns
| Pvar (_,var)                -> Vars.add var fv
| Pcons (_,(p1,_,p2))         -> pattern_vars (pattern_vars fv p1) p2
| Ppar (_,(_,pattern,_))      -> pattern_vars fv pattern
| Punit _ | Pwild _ | Pint _
| Ptrue _ | Pfalse _ | Pstr _ -> fv

and fv_expr env fv = function
            LetExpr (_,e) -> fv_let_expr (env, fv) e
|   Fun (_,(_,(_,v),_,e)) -> fv_expr (Vars.add v env) fv e
|         Tuple (_,comps) -> nsepseq_foldl (fv_expr env) fv comps
|             Match (_,e) -> fv_match_expr env fv e
| If (_,(_,e1,_,e2,_,e3)) -> let f = fv_expr env in f (f (f fv e1) e2) e3
|       Cat (_,(e1,_,e2))
|      Cons (_,(e1,_,e2))
|        Or (_,(e1,_,e2))
|       And (_,(e1,_,e2))
|        Lt (_,(e1,_,e2))
|       LEq (_,(e1,_,e2))
|        Gt (_,(e1,_,e2))
|       GEq (_,(e1,_,e2))
|        Eq (_,(e1,_,e2))
|       NEq (_,(e1,_,e2))
|       Add (_,(e1,_,e2))
|       Sub (_,(e1,_,e2))
|      Mult (_,(e1,_,e2))
|      Div  (_,(e1,_,e2))
|       Mod (_,(e1,_,e2))
|        Call (_,(e1,e2)) -> fv_expr env (fv_expr env fv e1) e2
|           Neg (_,(_,e))
|           Not (_,(_,e))
|         Par (_,(_,e,_)) -> fv_expr env fv e
|          Var (_,x as v) -> if Vars.mem x env then fv else FreeVars.add v fv
|        List (_,(_,l,_)) -> sepseq_foldl (fv_expr env) fv l
| Int _ | Str _ | Unit _ | True _ | False _ | Extern _ -> fv

and fv_match_expr env fv (_, expr, _, cases(*,_*)) =
  fv_cases env (fv_expr env fv expr) cases

and fv_cases env fv cases = nsepseq_foldl (fv_case env) fv cases

and fv_case env fv (pattern, _, expr) =
  fv_expr (Vars.union (pattern_vars Vars.empty pattern) env) fv expr

and fv_let_expr state = function
  LetIn (_,bindings,_,expr) ->
    (uncurry fv_expr) (fv_let_bindings state bindings) expr
| LetRecIn (_,_,bindings,_,expr) ->
    (uncurry fv_expr) (fv_let_rec_bindings state bindings) expr

let init_env = Vars.empty
             |> Vars.add "string_of_int"
             |> Vars.add "string_of_bool"
             |> Vars.add "print_string"
             |> Vars.add "print_int"
             |> Vars.add "scanf_string"
             |> Vars.add "scanf_int"
             |> Vars.add "scanf_bool"
             |> Vars.add "equal"

let vars = vars init_env
