(* TO DO:
     (1) top-level "let" and "let rec"
     (2) check for free variables
     (3) sum types
     (4) unit type
*)

module rec Env: sig
  type t = Value.t Map.Make(String).t

  exception Unbound of AST.var

  val empty: t
  val add:   AST.var -> Value.t -> t -> t
  val find:  AST.var -> t -> Value.t
  val mem:   AST.var -> t -> bool
end = struct
  module StringMap = Map.Make (String)

  type t = Value.t StringMap.t

  exception Unbound of AST.var

  let empty = StringMap.empty
  let add   = StringMap.add
  let mem   = StringMap.mem

  let find var env =
    try StringMap.find var env with
      Not_found -> raise (Unbound var)
end
and Value: sig
  type closure = AST.var * AST.expr * Env.t

  type t =
    Int     of int
  | String  of string
  | Bool    of bool
  | Tuple   of t list
  | Closure of closure
  | RecClos of closure option ref

  val to_string: t -> string
end = struct
  type closure = AST.var * AST.expr * Env.t

  type t =
    Int     of int
  | String  of string
  | Bool    of bool
  | Tuple   of t list
  | Closure of closure
  | RecClos of closure option ref

  let rec to_string = function
    Int n     -> string_of_int n
  | String s  -> s
  | Bool b    -> string_of_bool b
  | Closure _
  | RecClos _ -> "<closure>"
  | Tuple v   ->
      Printf.sprintf "(%s)"
        (List.fold_right (fun e a -> to_string e ^ a) v "")
end

exception Type_error of string
exception Invalid_RHS
exception Arity_mismatch
exception Nonlinear_pattern

let minus = function
  Value.Int v -> Value.Int (-v)
| _ -> raise (Type_error "minus")

open AST

exception Multiple_decl of AST.var

let rec eval_expr env = function
  LetIn (b,e) ->
    let b = nseq_to_list b in
    let env = List.fold_left (eval_binding env) env b
    in eval_expr env e
| LetRecIn (b,e) ->
    let b = nseq_to_list b in
    let bind, env =
      let apply (bind,env' as acc) = function
        RWild, _ -> acc
      | RVar f, Fun(x,e') ->
          let dummy = ref None in
          let v = Value.RecClos dummy in
          if Env.mem f env' then raise (Multiple_decl f)
          else (x,e',dummy)::bind, Env.add f v env'
      | _ -> raise Invalid_RHS
      in List.fold_left apply ([],env) b in
    let () =
      let apply (x,e',dummy) =
        dummy := Some (x,e',env)
      in List.iter apply bind
    in eval_expr env e
| Fun (x,e) ->
    Value.Closure (x,e,env)
| Tuple v ->
    Value.Tuple (List.map (eval_bool env) v)
| BoolExpr e ->
    eval_bool env e
| If (e1,e2,e3) ->
    match eval_expr env e1 with
      Value.Bool true  -> eval_expr env e2
    | Value.Bool false -> eval_expr env e3
    | _ -> raise (Type_error "eval_expr")

and eval_binding env env' (p,e) =
  match p with
    Pvar x ->
      Env.add x (eval_expr env e) env'
  | Pwild ->
      env'
  | Ptuple p ->
      match eval_expr env e with
        Value.Tuple v -> filter env' v p
      | _ -> raise (Type_error "eval_binding")

and filter env values patterns =
  let apply env v p =
    match v, p with
      _, Pvar x ->
        if Env.mem x env then raise Nonlinear_pattern
        else Env.add x v env
    | _, Pwild ->
        env
    | Value.Tuple values, Ptuple patterns ->
        filter env values patterns
    | _ -> raise (Type_error "filter") in
  try List.fold_left2 apply env values patterns with
       Invalid_argument _ -> raise Arity_mismatch

and eval_bool env = function
  ConjExpr e -> eval_conj env e
| Or (e1,e2) ->
    let open Value in
    match eval_bool env e1, eval_conj env e2 with
      Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> raise (Type_error "eval_bool")

and eval_conj env = function
  CompExpr e  -> eval_comp env e
| And (e1,e2) ->
    let open Value in
    match eval_conj env e1, eval_comp env e2 with
      Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> raise (Type_error "eval_conj")

and eval_comp env = function
  Lt (e1,e2) -> comp env (<)  e1 e2
| Le (e1,e2) -> comp env (<=) e1 e2
| Gt (e1,e2) -> comp env (>)  e1 e2
| Ge (e1,e2) -> comp env (>=) e1 e2
| Eq (e1,e2) -> comp env (=)  e1 e2
| Ne (e1,e2) -> comp env (<>) e1 e2
| AddExpr e  -> eval_add env e

and comp env op e1 e2 =
  let v1 = eval_comp env e1
  and v2 = eval_add env e2 in
  let open Value in
  match v1, v2 with
    Int n1, Int n2 -> Bool (op n1 n2)
  (*  | String s1, String s2 -> Bool (op s1 s2)*)
  | _ -> raise (Type_error "comp")

and eval_add env = function
  Add (e1,e2) -> add_op env (+) e1 e2
| Sub (e1,e2) -> add_op env (-) e1 e2
| MultExpr e  -> eval_mult env e

and add_op env op e1 e2 =
  let v1 = eval_add env e1
  and v2 = eval_mult env e2 in
  let open Value in
  match v1, v2 with
    Int n1, Int n2 -> Int (op n1 n2)
  | _ -> raise (Type_error "add_op")

and eval_mult env = function
  Mult (e1, e2)-> mult_op env ( * ) e1 e2
| Div (e1, e2) -> mult_op env (/)   e1 e2
| UnaryExpr e  -> eval_unary env e

and mult_op env op e1 e2 =
  let v1 = eval_mult env e1
  and v2 = eval_unary env e2 in
  let open Value in
  match v1, v2 with
    Int n1, Int n2 -> Int (op n1 n2)
  | _ -> raise (Type_error "mult_op")

and eval_unary env = function
        Minus e -> minus (eval_primary env e)
| PrimaryExpr e -> eval_primary env e

and eval_primary env = function
  CoreExpr e ->
    eval_core env e
| Not e ->
   (match eval_primary env e with
      Value.Bool b -> Value.Bool (not b)
    | _ -> raise (Type_error "eval_primary (not)"))
| Apply (e,args) ->
    let v = eval_core env e
    and apply f arg =
      let open Value in
      match f with
        Closure (param,body,env')
      | RecClos {contents = Some (param,body,env')} ->
          let v = eval_core env arg in
          let env' = Env.add param v env' in
          eval_expr env' body
      | _ -> raise (Type_error "eval_primary (call)")
    in List.fold_left apply v (nseq_to_list args)

and eval_core env = function
  Var x    -> Env.find x env
| Nat n    -> Value.Int n
| Str s    -> Value.String s
| True     -> Value.Bool true
| False    -> Value.Bool false
| Par e    -> eval_expr env e

let eval = eval_expr Env.empty
