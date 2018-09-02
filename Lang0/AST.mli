type var = string
type 'a nseq = 'a * 'a list

val nseq_to_list: 'a nseq -> 'a list
val nseq_map: ('a -> 'b) -> 'a nseq -> 'b nseq

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

val mk_var:    string -> expr
val to_string: t -> string
