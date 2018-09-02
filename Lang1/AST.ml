(* Abstract Syntax Tree (AST) for Slang, the STICK language *)

open Utils

(* Regions *)

type 'a reg = Region.t * 'a

(* Keywords in common between Slang and OCaml *)

type kwd_and   = Region.t
type kwd_else  = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_rec   = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t

(* Keywords specific to the STICK networks *)

type kwd_net    = Region.t
type kwd_fuse   = Region.t
type kwd_input  = Region.t
type kwd_output = Region.t
type kwd_node   = Region.t
type kwd_Gate   = Region.t
type kwd_Ge     = Region.t
type kwd_Gf     = Region.t
type kwd_V      = Region.t

(* Symbols *)

type one_syn  = Region.t  (* "<-" *)
type many_syn = Region.t  (* "<=" *)
type arrow    = Region.t  (* "->" *)
type cons     = Region.t  (* "::" *)
type cat      = Region.t  (* "^"  *)

(* Arithmetic operators *)

type minus  = Region.t   (* "-" *)
type plus   = Region.t   (* "+" *)
type div    = Region.t   (* "/" *)
type mult   = Region.t   (* "*" *)

type minusQ  = Region.t  (* "-." *)
type plusQ   = Region.t  (* "+." *)
type divQ    = Region.t  (* "/." *)
type multQ   = Region.t  (* "*." *)

(* Boolean operators *)

type bool_or  = Region.t  (* "||" *)
type bool_and = Region.t  (* "&&" *)

(* Comparisons *)

type eq = Region.t   (* "="  *)
type ne = Region.t   (* "<>" *)
type lt = Region.t   (* "<"  *)
type gt = Region.t   (* ">"  *)
type le = Region.t   (* "=<" *)
type ge = Region.t   (* ">=" *)

(* Compounds *)

type lpar = Region.t  (* "(" *)
type rpar = Region.t  (* ")" *)
type lbra = Region.t  (* "[" *)
type rbra = Region.t  (* "]" *)

(* Separators *)

type comma = Region.t  (* "," *)
type semi  = Region.t  (* ";" *)

(* Wildcard *)

type wild = Region.t  (* "_" *)

(* Literals *)

type var = string
type label = string (* "~<label>:" *)

(* Non-empty comma-separated values *)

type 'a csv = ('a, comma) nsepseq

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
| Include of Token.file

and let_bindings = (let_binding, kwd_and) nsepseq

and let_binding = pattern * eq * expr

and let_rec_bindings =
  RecFun  of (let_rec_fun_binding,  kwd_and) nsepseq
| RecNode of (let_rec_node_binding, kwd_and) nsepseq reg

and let_rec_fun_binding  = var reg * eq * fun_expr
and let_rec_node_binding = var reg * eq * node_expr

and pattern =
  Ptuple of pattern csv reg
| Plist  of pattern ssv bra reg
| Pvar   of var reg
| Punit  of unit__ reg
| Pwild  of wild
| Pcons  of (pattern * cons * pattern) reg
| Ppar   of pattern par reg

and expr =
  LetExpr of let_expr reg
| Fun     of fun_expr
| If      of conditional reg
| Tuple   of cat_expr csv reg
| CatExpr of cat_expr

and let_expr =
  LetIn    of kwd_let           * let_bindings     * kwd_in * expr
| LetRecIn of kwd_let * kwd_rec * let_rec_bindings * kwd_in * expr

and fun_expr = (kwd_fun * var reg * arrow * expr) reg

and conditional = kwd_if * expr * kwd_then * expr * kwd_else * expr

and cat_expr =
  Cat      of (cons_expr * cat * cat_expr) reg
| ConsExpr of cons_expr

and cons_expr =
  Cons     of (disj_expr * cons * cons_expr) reg
| DisjExpr of disj_expr

and disj_expr =
  Or       of (disj_expr * bool_or * conj_expr) reg
| ConjExpr of conj_expr

and conj_expr =
  And      of (conj_expr * bool_and * comp_expr) reg
| CompExpr of comp_expr

and comp_expr =
  Lt      of (comp_expr * lt * add_expr) reg
| LEq     of (comp_expr * le * add_expr) reg
| Gt      of (comp_expr * gt * add_expr) reg
| GEq     of (comp_expr * ge * add_expr) reg
| NEq     of (comp_expr * ne * add_expr) reg
| Eq      of (comp_expr * eq * add_expr) reg
| AddExpr of add_expr

and add_expr =
  Add      of (add_expr * plus   * mult_expr) reg
| AddQ     of (add_expr * plusQ  * mult_expr) reg
| Sub      of (add_expr * minus  * mult_expr) reg
| SubQ     of (add_expr * minusQ * mult_expr) reg
| MultExpr of mult_expr

and mult_expr =
  Mult      of (mult_expr * mult    * unary_expr) reg
| MultQ     of (mult_expr * multQ   * unary_expr) reg
| Div       of (mult_expr * div     * unary_expr) reg
| DivQ      of (mult_expr * divQ    * unary_expr) reg
| Mod       of (mult_expr * kwd_mod * unary_expr) reg
| UnaryExpr of unary_expr

and unary_expr =
  Neg     of (minus   * core_expr) reg
| NegQ    of (minusQ  * core_expr) reg
| Not     of (kwd_not * core_expr) reg
| Primary of primary_expr

and primary_expr =
  GenExpr of gen_expr
| NeuExpr of neuron_expr
| NetExpr of net_expr reg

and net_expr = kwd_net * core_expr * primary_expr

and gen_expr =
  CallExpr of (gen_expr * core_expr) reg
| CoreExpr of core_expr

and core_expr =
  Q      of (string * Q.t) reg
| Z      of Z.t reg
| Var    of var reg
| String of string reg
| Unit   of unit__ reg
| True   of kwd_true
| False  of kwd_false
| Par    of expr par reg
| List   of expr ssv bra reg
| Extern of extern

and extern =
  Fuse     of (var * var)
| Cast     of cast_expr
| Print    of print_expr
| Scanf    of scanf_expr
| QComp    of qcomp_expr
| NodeEq   of (var * var)
| PolyEq   of (var * var)
| Str2Ints of var                            (* TEMPORARY: string_to_int_list *)

and qcomp_expr =
  EqQ  of (var * var)
| LtQ  of (var * var)
| LeqQ of (var * var)
| GtQ  of (var * var)
| GeqQ of (var * var)

and cast_expr =
  StringOfZ     of var
| StringOfBool  of var
| StringOfWe    of var
| StringOfSlope of var
| StringOfGmul  of var
| StringOfDec   of var
| Ratio         of var
| Trunc         of var
| NameOfNode    of var
| AddrOfNode    of var

and print_expr =
  PrintString of var
| PrintInt    of var
| PrintBool   of var
| PrintWe     of var
| PrintSlope  of var
| PrintGmul   of var
| PrintDec    of var

and scanf_expr =
  ScanfInt    of var
| ScanfString of var
| ScanfBool   of var
| ScanfDec    of var

and neuron_expr =
  Node   of node_expr
| Output of (kwd_output * annotations * neuron_name * synapses) reg
| Input  of (kwd_input  * annotations * neuron_name) reg

and node_expr = (kwd_node * annotations * neuron_name * synapses) reg

and annotations = label_assoc reg list

and label_assoc = {
  label: label reg reg;
  annot: core_expr option
}

and neuron_name = core_expr

and synapses = synapse reg nseq reg option

and synapse = {
  kind:   syn_kind reg;
  weight: core_expr;
  delay:  core_expr;
  map:    bool reg;
  source: gen_expr
}

and syn_kind = V | Ge | Gf | Gate

(* Conversions to strings *)

let tuple_to_string to_string csv =
  let apply b a =
    let s = to_string b in
    if a = "" then s else Printf.sprintf "%s, %s" s a
  in Utils.nsepseq_foldr apply csv ""

let list_to_string to_string ssv =
  let apply e a =
    let s = to_string e in
    if a = "" then s else Printf.sprintf "%s; %s" s a
  in Printf.sprintf "[%s]" (Utils.sepseq_foldr apply ssv "")

let seq_to_string to_string seq =
  let apply e a =
    let s = to_string e in
    if a = "" then s else Printf.sprintf "%s; %s" s a
  in Printf.sprintf "[%s]" (List.fold_right apply seq "")

let nseq_to_string to_string (item, seq) =
  seq_to_string to_string (item::seq)

let rec to_string (statements,_) =
  let apply statement acc =
    let str = statement_to_string statement in
    if acc = "" then str else Printf.sprintf "%s\n%s" str acc
  in List.fold_right apply statements ""

and statement_to_string = function
  Let (_,(_,let_bindings)) ->
    Printf.sprintf "Let %s" (let_bindings_to_string let_bindings)
| LetRec (_,(_,_,let_rec_bindings)) ->
    Printf.sprintf "LetRec %s" (let_rec_bindings_to_string let_rec_bindings)
| Include {Token.start_pos; name} ->
    Printf.sprintf "Include \"%s\" at line %d" name (Pos.get_line start_pos)

and let_rec_bindings_to_string = function
  RecFun bindings ->
    let apply binding acc =
      let str = let_rec_fun_binding_to_string binding in
      if acc = "" then str else Printf.sprintf "%s\nAnd %s" acc str
    in Utils.nsepseq_foldr apply bindings ""
| RecNode (_,bindings) ->
    let apply binding acc =
      let str = let_rec_node_binding_to_string binding in
      if acc = "" then str else Printf.sprintf "%s\nAnd %s" acc str
    in Utils.nsepseq_foldr apply bindings ""

and let_bindings_to_string bindings =
  let apply binding acc =
    let str = let_binding_to_string binding in
    if acc = "" then str else Printf.sprintf "%s\nAnd %s" acc str
  in Utils.nsepseq_foldr apply bindings ""

and let_binding_to_string (pattern, _, expr) =
  Printf.sprintf "%s = %s" (pattern_to_string pattern) (expr_to_string expr)

and let_rec_fun_binding_to_string ((_,var), _, expr) =
  Printf.sprintf "%s = %s" var (fun_expr_to_string expr)

and let_rec_node_binding_to_string ((_,var), _, expr) =
  Printf.sprintf "%s = %s" var (node_expr_to_string expr)

and expr_to_string = function
  LetExpr (_,expr) -> let_expr_to_string expr
|     CatExpr expr -> cat_expr_to_string expr
|  Tuple (_,exprs) -> tuple_to_string (cat_expr_to_string) exprs
|         Fun expr -> fun_expr_to_string expr
| If (_,(_,e,_,e1,_,e2)) -> Printf.sprintf "If (%s, %s, %s)"
                              (expr_to_string e)
                              (expr_to_string e1)
                              (expr_to_string e2)

and fun_expr_to_string (_,(_,(_,var),_,expr)) =
  Printf.sprintf "Fun (%s, %s)" var (expr_to_string expr)

and let_expr_to_string = function
  LetIn (_,bindings,_, e) ->
    Printf.sprintf "LetIn ([%s], %s)"
      (let_bindings_to_string bindings) (expr_to_string e)
| LetRecIn (_,_,bindings,_, e) ->
    Printf.sprintf "LetRecIn ([%s], %s)"
      (let_rec_bindings_to_string bindings) (expr_to_string e)

and pattern_to_string = function
  Ptuple (_,p)        -> tuple_to_string pattern_to_string p
| Plist  (_,(_,p,_))  -> list_to_string pattern_to_string p
| Pvar (_,var)        -> var
| Ppar (_,(_,p,_))    -> Printf.sprintf "(%s)" (pattern_to_string p)
| Punit _             -> "()"
| Pwild _             -> "_"
| Pcons (_,(p1,_,p2)) -> Printf.sprintf "(%s::%s)"
                           (pattern_to_string p1) (pattern_to_string p2)

and cat_expr_to_string = function
  Cat (_,(arg1,_,arg2)) -> Printf.sprintf "%s ^ %s"
                             (cons_expr_to_string arg1)
                             (cat_expr_to_string arg2)
|            ConsExpr e -> cons_expr_to_string e

and cons_expr_to_string = function
  Cons (_,(hd,_,tl)) ->
    Printf.sprintf "%s::%s" (disj_expr_to_string hd) (cons_expr_to_string tl)
| DisjExpr e -> disj_expr_to_string e

and disj_expr_to_string = function
  Or (_,(e1,_,e2)) ->
    Printf.sprintf "%s || %s" (disj_expr_to_string e1) (conj_expr_to_string e2)
| ConjExpr e -> conj_expr_to_string e

and conj_expr_to_string = function
  And (_,(e1,_,e2)) ->
    Printf.sprintf "%s && %s" (conj_expr_to_string e1) (comp_expr_to_string e2)
| CompExpr e -> comp_expr_to_string e

and comp_expr_to_string = function
  Lt (_,(e1,_,e2)) ->
    Printf.sprintf "%s < %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| LEq (_,(e1,_,e2)) ->
    Printf.sprintf "%s =< %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| Gt (_,(e1,_,e2)) ->
    Printf.sprintf "%s > %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| GEq (_,(e1,_,e2)) ->
    Printf.sprintf "%s >= %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| Eq (_,(e1,_,e2)) ->
    Printf.sprintf "%s = %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| NEq (_,(e1,_,e2)) ->
    Printf.sprintf "%s <> %s" (comp_expr_to_string e1) (add_expr_to_string e2)
| AddExpr e -> add_expr_to_string e

and add_expr_to_string = function
   Add (_,(e1,_,e2)) -> Printf.sprintf "Add (%s, %s)"
                          (add_expr_to_string e1)
                          (mult_expr_to_string e2)
| AddQ (_,(e1,_,e2)) -> Printf.sprintf "AddQ (%s, %s)"
                          (add_expr_to_string e1)
                          (mult_expr_to_string e2)
|  Sub (_,(e1,_,e2)) -> Printf.sprintf "Sub (%s, %s)"
                          (add_expr_to_string e1)
                          (mult_expr_to_string e2)
| SubQ (_,(e1,_,e2)) -> Printf.sprintf "SubQ (%s, %s)"
                          (add_expr_to_string e1)
                          (mult_expr_to_string e2)
|         MultExpr e -> mult_expr_to_string e

and mult_expr_to_string = function
   Mult (_,(e1,_,e2)) -> Printf.sprintf "Mult (%s, %s)"
                           (mult_expr_to_string e1)
                           (unary_expr_to_string e2)
| MultQ (_,(e1,_,e2)) -> Printf.sprintf "MultQ (%s, %s)"
                           (mult_expr_to_string e1)
                           (unary_expr_to_string e2)
|   Div (_,(e1,_,e2)) -> Printf.sprintf "Div (%s, %s)"
                           (mult_expr_to_string e1)
                           (unary_expr_to_string e2)
|  DivQ (_,(e1,_,e2)) -> Printf.sprintf "DivQ (%s, %s)"
                           (mult_expr_to_string e1)
                           (unary_expr_to_string e2)
|   Mod (_,(e1,_,e2)) -> Printf.sprintf "Mod (%s, %s)"
                           (mult_expr_to_string e1)
                           (unary_expr_to_string e2)
|         UnaryExpr e -> unary_expr_to_string e

and unary_expr_to_string = function
   Neg (_,(_,e)) -> Printf.sprintf "-%s"    (core_expr_to_string e)
| NegQ (_,(_,e)) -> Printf.sprintf "~.%s"   (core_expr_to_string e)
|  Not (_,(_,e)) -> Printf.sprintf "not %s" (core_expr_to_string e)
|      Primary e -> primary_expr_to_string e

and primary_expr_to_string = function
  GenExpr     e -> gen_expr_to_string e
| NeuExpr     e -> neuron_expr_to_string e
| NetExpr (_,e) -> net_expr_to_string e

and net_expr_to_string (_,e1,e2) =
  Printf.sprintf "Net %s %s" (core_expr_to_string e1) (primary_expr_to_string e2)

and gen_expr_to_string = function
  CallExpr (_,(func,arg)) -> Printf.sprintf "CallExpr (%s, %s)"
                               (gen_expr_to_string func)
                               (core_expr_to_string arg)
| CoreExpr e -> core_expr_to_string e

and core_expr_to_string = function
       Q (_,(s,_)) -> s
|          Z (_,z) -> Z.to_string z
|        Var (_,x) -> x
|     String (_,s) -> Printf.sprintf "\"%s\"" s
|          False _ -> "false"
|           True _ -> "true"
|           Unit _ -> "()"
|  Par (_,(_,e,_)) -> Printf.sprintf "(%s)" (expr_to_string e)
| List (_,(_,l,_)) -> list_to_string expr_to_string l
|         Extern e -> extern_to_string e

and extern_to_string = function
    Fuse _ -> "%fuse"
|   Cast e -> cast_to_string   e
|  Print e -> print_to_string  e
|  Scanf e -> scanf_to_string  e
|  QComp e -> qcomp_to_string  e
| NodeEq e -> nodeeq_to_string e
| PolyEq e -> polyeq_to_string e
| Str2Ints var -> Printf.sprintf "string_to_int_list %s" var

and polyeq_to_string (x,y) = Printf.sprintf "equal %s %s" x y

and nodeeq_to_string (x,y) = Printf.sprintf "node_eq %s %s" x y

and qcomp_to_string = function
   EqQ (x,y) -> Printf.sprintf "w_eq %s %s"  x y
|  LtQ (x,y) -> Printf.sprintf "w_lt %s %s"  x y
| LeqQ (x,y) -> Printf.sprintf "w_leq %s %s" x y
|  GtQ (x,y) -> Printf.sprintf "w_gt %s %s"  x y
| GeqQ (x,y) -> Printf.sprintf "w_geq %s %s" x y

and print_to_string = function
  PrintString var -> Printf.sprintf "print_string %s" var
| PrintInt    var -> Printf.sprintf "print_int %s"    var
| PrintBool   var -> Printf.sprintf "print_bool %s"   var
| PrintWe     var -> Printf.sprintf "print_we %s"     var
| PrintSlope  var -> Printf.sprintf "print_slope %s"  var
| PrintGmul   var -> Printf.sprintf "print_gmul %s"   var
| PrintDec    var -> Printf.sprintf "print_dec %s"    var

and cast_to_string = function
  StringOfZ     var -> Printf.sprintf "string_of_int %s"   var
| StringOfBool  var -> Printf.sprintf "string_of_bool %s"  var
| StringOfWe    var -> Printf.sprintf "string_of_we %s"    var
| StringOfSlope var -> Printf.sprintf "string_of_slope %s" var
| StringOfGmul  var -> Printf.sprintf "string_of_gmul %s"  var
| StringOfDec   var -> Printf.sprintf "string_of_dec %s"   var
|         Ratio var -> Printf.sprintf "ratio %s"           var
|         Trunc var -> Printf.sprintf "trunc %s"           var
|    NameOfNode var -> Printf.sprintf "name_of_node %s"    var
|    AddrOfNode var -> Printf.sprintf "addr_of_node %s"    var

and scanf_to_string = function
  ScanfInt    var -> Printf.sprintf "scanf_int %s"    var
| ScanfString var -> Printf.sprintf "scanf_string %s" var
| ScanfBool   var -> Printf.sprintf "scanf_bool %s"   var
| ScanfDec    var -> Printf.sprintf "scanf_dec %s"    var

and neuron_expr_to_string = function
  Node expr ->
    Printf.sprintf "Node %s" (node_expr_to_string expr)
| Output expr ->
    Printf.sprintf "Output %s" (node_expr_to_string expr)
| Input (_, (_, annotations, name_expr)) ->
    Printf.sprintf "Input %s%s"
      (annotations_to_string annotations)
      (core_expr_to_string name_expr)

and annotations_to_string annotations =
  let apply acc (_, {label=_,(_,name); annot}) =
    match annot with
      None -> ""
    | Some annot ->
        let label_str =
          Printf.sprintf "~%s:%s" name (core_expr_to_string annot)
        in if acc = "" then label_str
           else Printf.sprintf "%s %s" acc label_str
  in List.fold_left apply "" annotations

and node_expr_to_string (_, (_, annotations, name_expr, syn)) =
  Printf.sprintf "%s%s %s"
   (annotations_to_string annotations)
   (core_expr_to_string name_expr)
   (match syn with
               None -> ""
    | Some (_,nseq) -> nseq_to_string (synapse_to_string <@ snd) nseq)

and synapse_to_string {kind=_,kind; weight; delay; map; source} =
  Printf.sprintf "%s (%s, %s, %s, %s)"
    (kind_to_string kind)
    (core_expr_to_string weight)
    (core_expr_to_string delay)
    (map_to_string map)
    (gen_expr_to_string source)

and kind_to_string = function
  V -> "V" | Ge -> "Ge" | Gf -> "Gf" | Gate -> "Gate"

and map_to_string = function
  _,  true -> "<="
| _, false -> "<-"


(* Projecting regions of the input source code *)

let rec region_of_pattern = function
  Plist (r,_) | Ptuple (r,_) | Pvar (r,_) | Punit (r,_)
| Pwild r | Pcons (r,_) | Ppar (r,_) -> r

and region_of_neuron_expr = function
  Node (r, _) | Output (r,_) | Input  (r,_) -> r

and region_of_primary_expr = function
  GenExpr     e -> region_of_gen_expr e
| NeuExpr     e -> region_of_neuron_expr e
| NetExpr (r,_) -> r

and region_of_unary_expr = function
  Neg (r,_) | NegQ (r,_) | Not (r,_) -> r
| Primary e -> region_of_primary_expr e

and region_of_mult_expr = function
  Mult (r,_) | MultQ (r,_) | Div (r,_) | DivQ (r,_) | Mod (r,_) -> r
| UnaryExpr e -> region_of_unary_expr e

and region_of_add_expr = function
  Add (r,_) | AddQ (r,_) | Sub (r,_) | SubQ (r,_) -> r
| MultExpr e -> region_of_mult_expr e

and region_of_comp_expr = function
  Lt (r,_) | LEq (r,_) | Gt (r,_) | GEq (r,_) | NEq (r,_) | Eq (r,_) -> r
| AddExpr e -> region_of_add_expr e

and region_of_conj_expr = function
   And (r,_) -> r
| CompExpr e -> region_of_comp_expr e

and region_of_disj_expr = function
    Or (r,_) -> r
| ConjExpr e -> region_of_conj_expr e

and region_of_cons_expr = function
  Cons (r,_) -> r
| DisjExpr e -> region_of_disj_expr e

and region_of_cat_expr = function
   Cat (r,_) -> r
| ConsExpr e -> region_of_cons_expr e

and region_of_expr = function
  LetExpr (r,_) | Fun (r,_) | If (r,_) | Tuple (r,_) -> r
| CatExpr e -> region_of_cat_expr e

and region_of_core_expr = function
  Q (r,_) | Z (r,_) | Var (r,_) | String (r,_) | Unit (r,_) | True r | False r
| List (r,_) | Par (r,_) -> r
| Extern _ -> Region.ghost

and region_of_gen_expr = function
  CoreExpr e -> region_of_core_expr e
| CallExpr (r,_) -> r

(* Predicates *)

let rec is_var = function
  CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr (UnaryExpr (Primary p)))))))) -> is_prim_a_var p
| _ -> false

and is_prim_a_var = function
            GenExpr e -> is_gen_a_var e
| NetExpr (_,(_,_,e)) -> is_prim_a_var e
|                   _ -> false

and is_gen_a_var = function
  CoreExpr e -> is_core_a_var e
|          _ -> false

and is_core_a_var = function
  Par (_,(_,e,_)) -> is_var e
| Var _ -> true
|     _ -> false

let rec is_neuron = function
  CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr (UnaryExpr (Primary p)))))))) -> is_prim_a_neu p
| _ -> false

and is_prim_a_neu = function
            GenExpr e -> is_gen_a_neu e
| NetExpr (_,(_,_,e)) -> is_prim_a_neu e
|           NeuExpr _ -> true

and is_gen_a_neu = function
  CoreExpr e -> is_core_a_neu e
|          _ -> false

and is_core_a_neu = function
  Par (_,(_,e,_)) -> is_neuron e
|               _ -> false

let rec is_call = function
  CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr (UnaryExpr (Primary p)))))))) -> is_prim_a_call p
| _ -> false

and is_prim_a_call = function
            GenExpr e -> is_gen_a_call e
| NetExpr (_,(_,_,e)) -> is_prim_a_call e
|                   _ -> false

and is_gen_a_call = function
  CoreExpr e -> is_core_a_call e
| CallExpr _ -> true

and is_core_a_call = function
  Par (_,(_,e,_)) -> is_call e
|               _ -> false

let rec is_fun = function
  Fun _ -> true
| CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr
               (UnaryExpr
                 (Primary
                   (GenExpr
                     (CoreExpr (Par (_,(_,e,_))))))))))))) -> is_fun e
| _ -> false


let rec rm_par = function
  CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr
               (UnaryExpr
                 (Primary
                   (GenExpr
                     (CoreExpr (Par (_,(_,e,_))))))))))))) -> rm_par e
| e -> e

(* Rewriting the AST *)

let var_to_unary_expr v = Primary   (GenExpr (CoreExpr (Var v)))
let var_to_mult_expr  v = UnaryExpr (var_to_unary_expr v)
let var_to_add_expr   v = MultExpr  (var_to_mult_expr v)
let var_to_comp_expr  v = AddExpr   (var_to_add_expr v)
let var_to_conj_expr  v = CompExpr  (var_to_comp_expr v)
let var_to_disj_expr  v = ConjExpr  (var_to_conj_expr v)
let var_to_cons_expr  v = DisjExpr  (var_to_disj_expr v)
let var_to_cat_expr   v = ConsExpr  (var_to_cons_expr v)
let var_to_expr       v = CatExpr   (var_to_cat_expr v)

let primary_to_expr e =
  CatExpr
   (ConsExpr
     (DisjExpr
       (ConjExpr
         (CompExpr
           (AddExpr
             (MultExpr (UnaryExpr (Primary e))))))))

let gen_to_expr e = primary_to_expr (GenExpr e)

let core_to_expr e = gen_to_expr (CoreExpr e)

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
              let bindings = (pattern, eq, var_to_expr fresh), [] in
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
let print_ssv print = print_sepseq  ";" print

let print_token reg conc =
  Printf.printf "%s: %s\n" (Region.compact reg) conc

let print_var (reg,var) =
  if var = "fuse" then
    Printf.printf "%s: fuse\n" (Region.compact reg)
  else Printf.printf "%s: Ident %s\n" (Region.compact reg) var

let print_str (reg,str) =
  Printf.printf "%s: String \"%s\"\n" (Region.compact reg) str

let print_label (outer_reg, (inner_reg, label)) =
  Printf.printf "%s: Label (%s, \"%s\")\n"
    (Region.compact outer_reg) (Region.compact inner_reg) label

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
| Include Token.{start_pos; name} ->
    Printf.printf "%s: #include \"%s\"\n" (Pos.compact start_pos) name

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

and print_let_rec_bindings undo = function
  RecFun bindings ->
    print_nsepseq "and" (print_let_rec_fun_binding undo) bindings
| RecNode (_,bindings) ->
    print_nsepseq "and" (print_let_rec_node_binding undo) bindings

and print_let_rec_fun_binding undo (var,eq,expr) =
  print_let_binding undo (Pvar var, eq, Fun expr)

and print_let_rec_node_binding undo (var,eq,expr) =
  print_let_binding undo (Pvar var, eq, primary_to_expr (NeuExpr (Node expr)))

and print_pattern = function
  Ptuple (_,patterns) ->
    print_csv print_pattern patterns
| Plist (_,(lbra,patterns,rbra)) ->
    print_token lbra "[";
    print_ssv print_pattern patterns;
    print_token rbra "]"
| Pvar (reg,var) ->
    Printf.printf "%s: Ident %s\n" (Region.compact reg) var
| Punit (_,(lpar,rpar)) ->
    print_token lpar "("; print_token rpar ")"
| Pwild wild ->
    print_token wild "_"
| Pcons (_,(p1,c,p2)) ->
    print_pattern p1; print_token c "::"; print_pattern p2
| Ppar (_,(lpar,p,rpar)) ->
    print_token lpar "("; print_pattern p; print_token rpar ")"

and print_expr undo = function
   LetExpr (_,e) -> print_let_expr undo e
|       If (_,e) -> print_conditional undo e
|  Tuple (_,csv) -> print_csv (print_cat_expr undo) csv
|      CatExpr e -> print_cat_expr undo e
| Fun (_,((kwd_fun,_,_,_) as f)) as e ->
    if undo then
      let patterns, arrow, expr = unparse' e in
      print_token kwd_fun "fun";
      Utils.nseq_iter print_pattern patterns;
      print_token arrow "->";
      print_expr undo expr
    else print_fun_expr undo f

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

and print_cat_expr undo = function
  Cat (_,(e1,cons,e2)) ->
    print_cons_expr undo e1; print_token cons "^"; print_cat_expr undo e2
| ConsExpr e -> print_cons_expr undo e

and print_cons_expr undo = function
  Cons (_,(e1,cons,e2)) ->
    print_disj_expr undo e1; print_token cons "::"; print_cons_expr undo e2
| DisjExpr e -> print_disj_expr undo e

and print_disj_expr undo = function
  Or (_,(e1,bool_or,e2)) ->
    print_disj_expr undo e1; print_token bool_or "||"; print_conj_expr undo e2
| ConjExpr e -> print_conj_expr undo e

and print_conj_expr undo = function
  And (_,(e1,bool_and,e2)) ->
    print_conj_expr undo e1; print_token bool_and "&&"; print_comp_expr undo e2
| CompExpr e -> print_comp_expr undo e

and print_comp_expr undo = function
  Lt (_,(e1,lt,e2)) ->
    print_comp_expr undo e1; print_token lt "<"; print_add_expr undo e2
| LEq (_,(e1,le,e2)) ->
    print_comp_expr undo e1; print_token le "<="; print_add_expr undo e2
| Gt (_,(e1,gt,e2)) ->
    print_comp_expr undo e1; print_token gt ">"; print_add_expr undo e2
| GEq (_,(e1,ge,e2)) ->
    print_comp_expr undo e1; print_token ge ">="; print_add_expr undo e2
| NEq (_,(e1,ne,e2)) ->
    print_comp_expr undo e1; print_token ne "<>"; print_add_expr undo e2
| Eq (_,(e1,eq,e2)) ->
    print_comp_expr undo e1; print_token eq "="; print_add_expr undo e2
| AddExpr e -> print_add_expr undo e

and print_add_expr undo = function
  Add (_,(e1,plus,e2)) ->
    print_add_expr undo e1; print_token plus "+"; print_mult_expr undo e2
| AddQ (_,(e1,qplus,e2)) ->
    print_add_expr undo e1; print_token qplus "+."; print_mult_expr undo e2
| Sub (_,(e1,minus,e2)) ->
    print_add_expr undo e1; print_token minus "-"; print_mult_expr undo e2
| SubQ (_,(e1,qminus,e2)) ->
    print_add_expr undo e1; print_token qminus "-."; print_mult_expr undo e2
| MultExpr e -> print_mult_expr undo e

and print_mult_expr undo = function
  Mult (_,(e1,mult,e2)) ->
    print_mult_expr undo e1; print_token mult "*"; print_unary_expr undo e2
| MultQ (_,(e1,qmult,e2)) ->
    print_mult_expr undo e1; print_token qmult "*."; print_unary_expr undo e2
| Div (_,(e1,div,e2)) ->
    print_mult_expr undo e1; print_token div "/"; print_unary_expr undo e2
| DivQ (_,(e1,qdiv,e2)) ->
    print_mult_expr undo e1; print_token qdiv "/."; print_unary_expr undo e2
| Mod (_,(e1,kwd_mod,e2)) ->
    print_mult_expr undo e1; print_token kwd_mod "mod"; print_unary_expr undo e2
| UnaryExpr e -> print_unary_expr undo e

and print_unary_expr undo = function
    Neg (_,(minus,e)) -> print_token minus "-"; print_core_expr undo e
| NegQ (_,(qminus,e)) -> print_token qminus "~."; print_core_expr undo e
| Not (_,(kwd_not,e)) -> print_token kwd_not "not"; print_core_expr undo e
|           Primary e -> print_primary_expr undo e

and print_primary_expr undo = function
  GenExpr     e -> print_gen_expr undo e
| NeuExpr     e -> print_neu_expr undo e
| NetExpr (_,e) -> print_net_expr undo e

and print_net_expr undo (kwd_net,e1,e2) =
  print_token kwd_net "net"; print_core_expr undo e1; print_primary_expr undo e2

and print_gen_expr undo = function
  CallExpr (_,(e1,e2)) -> print_gen_expr undo e1; print_core_expr undo e2
| CoreExpr e -> print_core_expr undo e

and print_core_expr undo = function
  Q (r,(conc,q)) ->
    print_token r (Printf.sprintf "Frac (%s,%s)" conc (Q.to_string q))
| Z (r,z) -> print_token r (Printf.sprintf "Int %s" (Z.to_string z))
| Var v -> print_var v
| String s -> print_str s
| Unit (_,(lpar,rpar)) ->
    print_token lpar "("; print_token rpar ")"
| True kwd_true -> print_token kwd_true "true"
| False kwd_false -> print_token kwd_false "false"
| Par (_,(lpar,e,rpar)) ->
    print_token lpar "("; print_expr undo e; print_token rpar ")"
| List (_,(lbra,ssv,rbra)) ->
    print_token lbra "["; print_ssv (print_expr undo) ssv; print_token rbra "]"
| Extern _ -> ()

and print_neu_expr undo = function
  Node (_,(kwd_node,annotations,name,syn)) ->
    print_token kwd_node "node";
    print_annotations undo annotations;
    print_core_expr undo name;
    print_synapses undo syn
| Output (_,(kwd_output,annotations,name,syn)) ->
    print_token kwd_output "output";
    print_annotations undo annotations;
    print_core_expr undo name;
    print_synapses undo syn
| Input (_,(kwd_input,annotations,name)) ->
    print_token kwd_input "input";
    print_annotations undo annotations;
    print_core_expr undo name

and print_annotations undo = List.iter (print_annotation undo)

and print_annotation undo (_, {label; annot}) =
  print_label label;
  match annot with
          None -> ()
  | Some annot -> print_core_expr undo annot

and print_synapses undo = function
               None -> ()
| Some (_,synapses) -> nseq_iter (print_synapse undo <@ snd) synapses

and print_synapse undo {kind; weight; delay; map; source} =
  print_kind kind;
  print_core_expr undo weight;
  print_core_expr undo delay;
  print_map map;
  print_gen_expr undo source

and print_kind = function
  reg, V    -> print_token reg "V"
| reg, Ge   -> print_token reg "Ge"
| reg, Gf   -> print_token reg "Gf"
| reg, Gate -> print_token reg "Gate"

and print_map = function
  reg, true  -> print_token reg "<="
| reg, false -> print_token reg "<-"


(* Variables (free and bound) *)

type rvar = var reg

module FreeVars =
  Set.Make (struct type t = rvar let compare = compare end)

module Vars = String.Set

(* The value of the call [free_vars env ast] is the set of free
   variables in the Abstract Syntax Tree [ast] in the environment
   [env] (here, the environment is a set of variables with their
   locations, _not_ a value of type [Env.t]). On the same register,
   keep in mind also that the parameters [state] below are _not_ of
   type [State.t], but, instead, are a pair made of the current
   environment and the current set of free variables (only the latter
   is threaded, whilst the former flows top-down only). *)

let rec vars env (statements,_) =
  List.fold_left fv_statement (env, FreeVars.empty) statements

and fv_statement state = function
  Let    (_,(_,  bindings)) -> fv_let_bindings state bindings
| LetRec (_,(_,_,bindings)) -> fv_let_rec_bindings state bindings
| Include _                 -> state

and fv_let_bindings (env, _ as state) =
  nsepseq_foldl (fv_let_binding env) state

and fv_let_rec_bindings state = function
      RecFun bindings  -> fv_let_rec_fun_bindings  state bindings
| RecNode (_,bindings) -> fv_let_rec_node_bindings state bindings

and fv_let_rec_fun_bindings (env, fv) bindings =
  let env' =
    nsepseq_foldl (fun a ((_,v),_,_) -> Vars.add v a) env bindings
  in env', nsepseq_foldl (fv_let_rec_fun_binding env') fv bindings

and fv_let_rec_node_bindings (env, fv) bindings =
  let env' =
    nsepseq_foldl (fun a ((_,v),_,_) -> Vars.add v a) env bindings
  in env', nsepseq_foldl (fv_let_rec_node_binding env') fv bindings

and fv_let_binding env (env',fv) (pattern,_,expr) =
  Vars.union (pattern_vars Vars.empty pattern) env', fv_expr env fv expr

and fv_let_rec_fun_binding  env fv (_,_,expr) = fv_expr env fv (Fun expr)

and fv_let_rec_node_binding env fv (_,_,expr) =
  fv_neu_expr env fv (Node expr)

and pattern_vars fv = function
  Ptuple (_,patterns)      -> nsepseq_foldl pattern_vars fv patterns
| Plist (_,(_,patterns,_)) -> sepseq_foldl  pattern_vars fv patterns
| Pvar (_,var)             -> Vars.add var fv
| Punit _ | Pwild _        -> fv
| Pcons (_,(p1,_,p2))      -> pattern_vars (pattern_vars fv p1) p2
| Ppar (_,(_,pattern,_))   -> pattern_vars fv pattern

and fv_expr env fv = function
  LetExpr (_,e) ->
    fv_let_expr (env, fv) e
| Fun (_,(_,(_,v),_,e)) ->
    fv_expr (Vars.add v env) fv e
| CatExpr e ->
    fv_cat_expr env fv e
| Tuple (_,comps) ->
    nsepseq_foldl (fv_cat_expr env) fv comps
| If (_,(_,e1,_,e2,_,e3)) ->
    fv_expr env (fv_expr env (fv_expr env fv e1) e2) e3

and fv_let_expr state = function
  LetIn (_,bindings,_,expr) ->
    (curry fv_expr) (fv_let_bindings state bindings) expr
| LetRecIn (_,_,bindings,_,expr) ->
    (curry fv_expr) (fv_let_rec_bindings state bindings) expr

and fv_cat_expr env fv = function
  Cat (_,(e1,_,e2)) -> fv_cat_expr env (fv_cons_expr env fv e1) e2
|        ConsExpr e -> fv_cons_expr env fv e

and fv_cons_expr env fv = function
  Cons (_,(e1,_,e2)) -> fv_cons_expr env (fv_disj_expr env fv e1) e2
|         DisjExpr e -> fv_disj_expr env fv e

and fv_disj_expr env fv = function
  Or (_,(e1,_,e2)) -> fv_conj_expr env (fv_disj_expr env fv e1) e2
|       ConjExpr e -> fv_conj_expr env fv e

and fv_conj_expr env fv = function
  And (_,(e1,_,e2)) -> fv_comp_expr env (fv_conj_expr env fv e1) e2
|        CompExpr e -> fv_comp_expr env fv e

and fv_comp_expr env fv = function
  Lt (_,(e1,_,e2)) | LEq (_,(e1,_,e2))
| Gt (_,(e1,_,e2)) | GEq (_,(e1,_,e2))
| Eq (_,(e1,_,e2)) | NEq (_,(e1,_,e2)) ->
    fv_add_expr env (fv_comp_expr env fv e1) e2
| AddExpr e -> fv_add_expr env fv e

and fv_add_expr env fv = function
  Add (_,(e1,_,e2)) | AddQ (_,(e1,_,e2))
| Sub (_,(e1,_,e2)) | SubQ (_,(e1,_,e2)) ->
    fv_mult_expr env (fv_add_expr env fv e1) e2
| MultExpr e -> fv_mult_expr env fv e

and fv_mult_expr env fv = function
  Mult (_,(e1,_,e2)) | MultQ (_,(e1,_,e2))
| Div  (_,(e1,_,e2)) | DivQ  (_,(e1,_,e2)) | Mod (_,(e1,_,e2)) ->
    fv_unary_expr env (fv_mult_expr env fv e1) e2
| UnaryExpr e -> fv_unary_expr env fv e

and fv_unary_expr env fv = function
  Neg (_,(_,e)) | NegQ (_,(_,e)) | Not (_,(_,e)) ->
    fv_core_expr env fv e
| Primary e -> fv_primary_expr env fv e

and fv_primary_expr env fv = function
  GenExpr     e -> fv_gen_expr env fv e
| NeuExpr     e -> fv_neu_expr env fv e
| NetExpr (_,e) -> fv_net_expr env fv e

and fv_net_expr env fv (_,e1,e2) =
  fv_primary_expr env (fv_core_expr env fv e1) e2

and fv_gen_expr env fv = function
      CoreExpr e -> fv_core_expr env fv e
| CallExpr (_,e) -> fv_call_expr env fv e

and fv_call_expr env fv (e1,e2) =
  fv_core_expr env (fv_gen_expr env fv e1) e2

and fv_core_expr env fv = function
  Q _ | Z _ | String _ | Unit _ | True _ | False _ | Extern _ -> fv
|  Par (_,(_,e,_)) -> fv_expr env fv e
|            Var v -> fv_var  env fv v
| List (_,(_,l,_)) -> sepseq_foldl (fv_expr env) fv l

and fv_var env fv (_,x as v) =
  if Vars.mem x env then fv else FreeVars.add v fv

and fv_neu_expr env fv = function
  Node   (_, (_, annot, name, syn))
| Output (_, (_, annot, name, syn)) ->
    fv |> swap (fv_core_expr   env) name
       |> swap (fv_annotations env) annot
       |> swap (fv_synapses    env) syn
| Input (_, (_, annot, name)) ->
    fv_annotations env (fv_core_expr env fv name) annot

and fv_annotations env =
  List.fold_left (fv_annotation env)

and fv_annotation env fv (_,{annot;_}) =
  match annot with
          None -> fv
  | Some annot -> fv_core_expr env fv annot

and fv_synapses env fv = function
          None -> fv
| Some (_,syn) -> nseq_foldl (fun fv (_,s) -> fv_synapse env fv s) fv syn

and fv_synapse env fv {weight; delay; source; _} =
  fv |> swap (fv_core_expr env) weight
     |> swap (fv_core_expr env) delay
     |> swap (fv_gen_expr  env) source

let init_env = Vars.empty
             |> Vars.add "string_of_int"
             |> Vars.add "string_of_bool"
             |> Vars.add "string_of_we"
             |> Vars.add "string_of_slope"
             |> Vars.add "string_of_gmul"
             |> Vars.add "string_of_dec"
             |> Vars.add "ratio"
             |> Vars.add "trunc"
             |> Vars.add "name_of_node"
             |> Vars.add "addr_of_node"
             |> Vars.add "print_string"
             |> Vars.add "print_int"
             |> Vars.add "print_we"
             |> Vars.add "print_slope"
             |> Vars.add "print_gmul"
             |> Vars.add "print_dec"
             |> Vars.add "fuse"
             |> Vars.add "scanf_int"
             |> Vars.add "scanf_string"
             |> Vars.add "scanf_bool"
             |> Vars.add "scanf_dec"
             |> Vars.add "we"
             |> Vars.add "slope"
             |> Vars.add "gmul"
             |> Vars.add "string_to_int_list" (* TEMPORARY *)
             |> Vars.add "weight_eq"
             |> Vars.add "weight_lt"
             |> Vars.add "weight_leq"
             |> Vars.add "weight_gt"
             |> Vars.add "weight_geq"
             |> Vars.add "node_eq"
             |> Vars.add "equal"

let vars = vars init_env


(* Checking and normalising the AST *)

exception Duplicate_label of rvar
exception Invalid_label   of rvar

let valid_labels = ["state"; "debug"; "min"] (* Add here more annotations *)

module Labels = Utils.String.Set

let valid_labels = List.fold_right Labels.add valid_labels Labels.empty

let rec norm_statements statements =
  List.(fold_left (fun acc s -> norm_statement s :: acc) [] statements |> rev)

and norm_statement = function
  Let (reg, (kwd_let, let_bindings)) ->
    Let (reg, (kwd_let, norm_let_bindings let_bindings))
| LetRec (reg, (kwd_let, kwd_rec, let_rec_bindings)) ->
    LetRec (reg, (kwd_let, kwd_rec, norm_let_rec_bindings let_rec_bindings))
| Include _ as s -> s

and norm_let_bindings (head,tail) =
  let acc = norm_let_binding head, []
  and app acc (sep, binding) =
    Utils.nsepseq_cons (norm_let_binding binding) sep acc
  in List.fold_left app acc tail |> Utils.nsepseq_rev

and norm_let_binding (pattern, eq, e) = pattern, eq, norm_expr e

and norm_let_rec_bindings = function
         RecFun seq -> RecFun  (norm_let_rec_fun_bindings seq)
| RecNode (reg,seq) -> RecNode (reg, norm_rec_node_bindings seq)

and norm_let_rec_fun_bindings (head,tail) =
  let acc = norm_let_rec_fun_binding head, []
  and app acc (sep, binding) =
    Utils.nsepseq_cons (norm_let_rec_fun_binding binding) sep acc
  in List.fold_left app acc tail |> Utils.nsepseq_rev

and norm_let_rec_fun_binding (rvar,eq,e) = rvar, eq, norm_fun_expr e

and norm_rec_node_bindings (head,tail) =
  let acc = norm_rec_node_binding head, []
  and app acc (sep, binding) =
    Utils.nsepseq_cons (norm_rec_node_binding binding) sep acc
  in List.fold_left app acc tail |> Utils.nsepseq_rev

and norm_rec_node_binding (rvar, eq, e) = rvar, eq, norm_node_expr e

and norm_expr = function
  LetExpr (reg, e) -> LetExpr (reg, norm_let_expr e)
| Fun e -> Fun (norm_fun_expr e)
| If (reg,e) -> If (reg, norm_conditional e)
| Tuple (reg,(head,tail)) ->
    let acc = norm_cat_expr head, []
    and app acc (sep, e) =
      Utils.nsepseq_cons (norm_cat_expr e) sep acc in
    let csv = List.fold_left app acc tail |> Utils.nsepseq_rev
    in Tuple (reg, csv)
| CatExpr e -> CatExpr (norm_cat_expr e)

and norm_let_expr = function
  LetIn (kwd_let, let_bindings, kwd_in, e) ->
    LetIn (kwd_let, norm_let_bindings let_bindings, kwd_in, norm_expr e)
| LetRecIn (kwd_let, kwd_rec, let_rec_bindings, kwd_in, e) ->
    LetRecIn (kwd_let, kwd_rec, norm_let_rec_bindings let_rec_bindings,
              kwd_in, norm_expr e)

and norm_fun_expr (reg,(kwd_fun,rvar,arrow,e)) =
  reg, (kwd_fun, rvar, arrow, norm_expr e)

and norm_conditional (kwd_if, e1, kwd_then, e2, kwd_else ,e3) =
  kwd_if, norm_expr e1, kwd_then, norm_expr e2, kwd_else, norm_expr e3

and norm_cat_expr = function
  Cat (reg, (cons_expr, cat, cat_expr)) ->
    Cat (reg, (norm_cons_expr cons_expr, cat, norm_cat_expr cat_expr))
| ConsExpr e -> ConsExpr (norm_cons_expr e)

and norm_cons_expr = function
  Cons (reg, (disj_expr, cons, cons_expr)) ->
    Cons (reg, (norm_disj_expr disj_expr, cons, norm_cons_expr cons_expr))
| DisjExpr e -> DisjExpr (norm_disj_expr e)

and norm_disj_expr = function
  Or (reg, (disj_expr, bool_or, conj_expr)) ->
    Or (reg, (norm_disj_expr disj_expr, bool_or, norm_conj_expr conj_expr))
| ConjExpr e -> ConjExpr (norm_conj_expr e)

and norm_conj_expr = function
  And (reg, (conj_expr, bool_and, comp_expr)) ->
    And (reg, (norm_conj_expr conj_expr, bool_and, norm_comp_expr comp_expr))
| CompExpr e -> CompExpr (norm_comp_expr e)

and norm_comp_expr = function
  Lt (reg, (comp_expr, lt, add_expr)) ->
    Lt (reg, (norm_comp_expr comp_expr, lt, norm_add_expr add_expr))
| LEq (reg, (comp_expr, leq, add_expr)) ->
    LEq (reg, (norm_comp_expr comp_expr, leq, norm_add_expr add_expr))
| Gt (reg, (comp_expr, gt, add_expr)) ->
    Gt (reg, (norm_comp_expr comp_expr, gt, norm_add_expr add_expr))
| GEq (reg, (comp_expr, geq, add_expr)) ->
    GEq (reg, (norm_comp_expr comp_expr, geq, norm_add_expr add_expr))
| NEq (reg, (comp_expr, neq, add_expr)) ->
    NEq (reg, (norm_comp_expr comp_expr, neq, norm_add_expr add_expr))
| Eq (reg, (comp_expr, eq, add_expr)) ->
    Eq (reg, (norm_comp_expr comp_expr, eq, norm_add_expr add_expr))
| AddExpr e -> AddExpr (norm_add_expr e)

and norm_add_expr = function
  Add (reg, (add_expr, plus, mult_expr)) ->
    Add (reg, (norm_add_expr add_expr, plus, norm_mult_expr mult_expr))
| AddQ (reg, (add_expr, plusQ, mult_expr)) ->
    AddQ (reg, (norm_add_expr add_expr, plusQ, norm_mult_expr mult_expr))
| Sub (reg, (add_expr, minus, mult_expr)) ->
    Sub (reg, (norm_add_expr add_expr, minus, norm_mult_expr mult_expr))
| SubQ (reg, (add_expr, minusQ, mult_expr)) ->
    SubQ (reg, (norm_add_expr add_expr, minusQ, norm_mult_expr mult_expr))
| MultExpr e -> MultExpr (norm_mult_expr e)

and norm_mult_expr = function
  Mult (reg, (mult_expr, mult, unary_expr)) ->
    Mult (reg, (norm_mult_expr mult_expr, mult, norm_unary_expr unary_expr))
| MultQ (reg, (mult_expr, multQ, unary_expr)) ->
    MultQ (reg, (norm_mult_expr mult_expr, multQ, norm_unary_expr unary_expr))
| Div (reg, (mult_expr, div, unary_expr)) ->
    Div (reg, (norm_mult_expr mult_expr, div, norm_unary_expr unary_expr))
| DivQ (reg, (mult_expr, divQ, unary_expr)) ->
    DivQ (reg, (norm_mult_expr mult_expr, divQ, norm_unary_expr unary_expr))
| Mod (reg, (mult_expr, kwd_mod, unary_expr)) ->
    Mod (reg, (norm_mult_expr mult_expr, kwd_mod, norm_unary_expr unary_expr))
| UnaryExpr e -> UnaryExpr (norm_unary_expr e)

and norm_unary_expr = function
  Neg  (reg, (minus,   e)) -> Neg  (reg, (minus,   norm_core_expr e))
| NegQ (reg, (minusQ,  e)) -> NegQ (reg, (minusQ,  norm_core_expr e))
| Not  (reg, (kwd_not, e)) -> Not  (reg, (kwd_not, norm_core_expr e))
|                Primary e -> Primary (norm_primary_expr e)

and norm_primary_expr = function
        GenExpr e -> GenExpr (norm_gen_expr e)
|       NeuExpr e -> NeuExpr (norm_neuron_expr e)
| NetExpr (reg,e) -> NetExpr (reg, norm_net_expr e)

and norm_net_expr (kwd_net, core_expr, primary_expr) =
  kwd_net, norm_core_expr core_expr, norm_primary_expr primary_expr

and norm_gen_expr = function
  CallExpr (reg, (gen_expr, core_expr)) ->
    CallExpr (reg, (norm_gen_expr gen_expr, norm_core_expr core_expr))
| CoreExpr e -> CoreExpr (norm_core_expr e)

and norm_core_expr = function
  (Q _ | Z _ | Var _ | String _ | Unit _ | True _ | False _ | Extern _
   | List (_,(_,None,_))) as e -> e
| Par (reg,(left,e,right)) ->
    Par (reg, (left, norm_expr e, right))
| List (reg, (left, Some (head, tail), right)) ->
    let acc = norm_expr head, []
    and app acc (sep,e) = Utils.nsepseq_cons (norm_expr e) sep acc in
    let seq' = List.fold_left app acc tail |> Utils.nsepseq_rev
    in List (reg, (left, Some seq', right))

and norm_neuron_expr = function
  Node e ->  Node (norm_node_expr e)
| Output e -> Output (norm_node_expr e)
| Input (reg, (kwd_input, annotations, name)) ->
    Input (reg, (kwd_input, norm_annotations annotations, norm_core_expr name))

and norm_node_expr (reg, (kwd_node, annotations, name, synapses)) =
  reg,
 (kwd_node,
  norm_annotations annotations,
  norm_core_expr   name,
  norm_synapses    synapses)

and norm_annotations annotations =
  let check labels (_, {label=_, ((_,name) as label); _}) =
    if Labels.mem name labels then
      raise (Duplicate_label label)
    else if Labels.mem name valid_labels then
           Labels.add name labels
         else raise (Invalid_label label) in
  let present = List.fold_left check Labels.empty annotations in
  let missing = Labels.diff valid_labels present in
  let mk_dummy name =
    Region.(ghost, {label = ghost, (ghost, name); annot = None}) in
  Labels.fold (fun name acc -> mk_dummy name :: acc) missing annotations

and norm_synapses = function
  None -> None
| Some (reg, ((head_reg, head), tail)) ->
    let acc = (head_reg, norm_synapse head), []
    and app acc (reg, syn) = Utils.nseq_cons (reg, norm_synapse syn) acc
    in Some (reg, List.fold_left app acc tail |> Utils.nseq_rev)

and norm_synapse {kind; weight; delay; map; source} =
  {kind;
   weight = norm_core_expr weight;
   delay  = norm_core_expr delay;
   map;
   source = norm_gen_expr  source}

let normalise (ast,eof) = norm_statements ast, eof
