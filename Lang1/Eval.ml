(* This module implements the interpreter _Stickler_ of Slang, the
   STICK language. *)

open Utils
open Network
open AST

let ghost = Region.ghost

(* Semantic values environment *)

module rec Env: sig
  type t = Value.t String.Map.t

  exception Unbound of rvar

  val empty: t
  val add:   rvar -> Value.t -> t -> t
  val find:  rvar -> t -> Value.t
  val mem:   rvar -> t -> bool
  val union: (Value.t -> Value.t -> Value.t option) -> t -> t -> t
  val fold:  (string -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a

  val to_string: t -> string
end = struct
  type t = Value.t String.Map.t

  exception Unbound of rvar

  let empty     = String.Map.empty
  let add (_,v) = String.Map.add v
  let mem (_,v) = String.Map.mem v
  let union f   = String.Map.union (lambda f)
  let fold      = String.Map.fold

  let find v env =
    try String.Map.find (snd v) env with
      Not_found -> raise (Unbound v)

  let to_string env =
    let apply var value acc =
      Printf.sprintf "%s -> %s\n%s" var (Value.to_string value) acc
    in String.Map.fold apply env ""
end

(* Semantic values *)

and Value: sig
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Rational of Q.t reg
  | Integer  of Z.t reg
  | Str      of string reg
  | Bool     of bool reg
  | Unit     of Region.t
  | Tuple    of t list reg
  | List     of t list reg
  | Closure  of closure reg
  | RecClos  of closure reg option ref
  | Neuron   of Neuron.t reg

  val eq: State.thread -> t -> t -> bool

  val to_string: t -> string
  val to_region: t -> Region.t

  val set_region: Region.t -> t -> t
end
=
struct
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Rational of Q.t reg
  | Integer  of Z.t reg
  | Str      of string reg
  | Bool     of bool reg
  | Unit     of Region.t
  | Tuple    of t list reg
  | List     of t list reg
  | Closure  of closure reg
  | RecClos  of closure reg option ref
  | Neuron   of Neuron.t reg

  let to_region = function
    Rational (reg,_) | Integer (reg,_) | Str (reg,_) | Bool (reg,_)
  | Unit reg | Tuple (reg,_) | List (reg,_)
  | Closure (reg,_) | RecClos {contents = Some (reg,_)}
  | Neuron (reg,_) -> reg
  | RecClos {contents=None} -> ghost

  let rec to_string = function
    Rational (_,q) -> Q.to_string q
  | Integer (_,z)  -> Z.to_string z
  | Str (_,s)      -> "\"" ^ s ^ "\""
  | Bool (_,b)     -> string_of_bool b
  | Unit _         -> "()"
  | Closure _
  | RecClos _      -> "a closure"
  | Neuron (_,n)   -> Printf.sprintf "a neuron (%s)" (Neuron.to_string n)
  | Tuple (_,values) ->
      let apply e = function
        "" -> to_string e
      |  a -> to_string e ^ ", " ^ a
      in Printf.sprintf "(%s)" (List.fold_right apply values "")
  | List (_,values) ->
      let apply e = function
        "" -> to_string e
      |  a -> to_string e ^ "; " ^ a
      in Printf.sprintf "[%s]" (List.fold_right apply values "")

  let set_region r = function
    Rational (_,q) -> Rational (r,q)
  | Integer  (_,z) -> Integer (r,z)
  |     Str  (_,s) -> Str (r,s)
  |    Bool  (_,b) -> Bool (r,b)
  |         Unit _ -> Unit r
  |    Tuple (_,t) -> Tuple (r,t)
  |     List (_,l) -> List (r,l)
  |  Closure (_,c) -> Closure (r,c)
  | RecClos {contents = Some (_,c)}
                   -> RecClos {contents = Some (r,c)}
  |      RecClos c -> RecClos c
  |   Neuron (_,n) -> Neuron (r,n)

  let rec eq thread v1 v2 =
    match v1, v2 with
      Rational (_,r1), Rational (_,r2) -> Q.equal r1 r2
    | Integer (_,z1),   Integer (_,z2) -> Z.equal z1 z2
    | Str (_,s1),           Str (_,s2) -> s1 = s2
    | Bool (_,b1),         Bool (_,b2) -> b1 = b2
    | Unit _,                   Unit _ -> true
    | Neuron (_,n1),     Neuron (_,n2) ->
        let n1, _ = Zipper.resolve ?log:thread.State.log n1 thread.State.zip
        and n2, _ = Zipper.resolve ?log:thread.State.log n2 thread.State.zip
        in Neuron.eq n1 n2
    | Tuple (_,l1), Tuple (_,l2)
    | List (_,l1),   List (_,l2) ->
       (try List.for_all2 (eq thread) l1 l2 with Invalid_argument _ -> false)
    | _ -> false
end

(* State *)

and State: sig
  type thread = {
    zip:   Zipper.t;
    log:   out_channel option;
    dot:   out_channel option;
    net:   out_channel option;
    stats: out_channel option
  }
  type t = {env: Env.t; thread: thread}
  type state = t
end = State

(* Errors expected to be caught by OCaml static typing *)

exception Type_error        of State.t * string
exception Nonlinear_pattern of State.t * rvar
exception Multiple_decl     of State.t * rvar

(* Dynamic errors (STICK-specific) *)

exception Div_by_zero of State.t * Region.t
exception Nameless_input of State.t * Region.t
exception Nameless_output of State.t * Region.t
exception Homograph of State.t * Region.t * Neuron.name reg * Net.Addr.t
exception Multiple_syn of State.t * Neuron.t * Neuron.t * Region.t * Region.t
exception NotSource of State.t * Neuron.t reg * int
exception TwoStates of State.t * Neuron.t * Neuron.t reg
exception Duplicate of State.t * Neuron.t * Neuron.t reg * (Label.t * Neuron.t)
exception Toplevel_state of State.t * Region.t

exception NonPositiveDelay of State.t * Delay.t reg
exception NetCollision of State.t * Net.name reg

(* Evaluation *)

(* The evaluation of top-level bindings (called statements below) is
   slightly different from other constructs as the evaluator state
   must be threaded in stead of only the proper thread (the tree of
   networks traversed by a zipper). The reason is that the evaluation
   of a statement enriches the environment and that extended
   environment must then be used to evaluate the next statement,
   whereas in the case of subexpressions or local bindings, the
   environment only flows top-down (that is, local bindings cannot
   escape). *)

let rec eval_statements state (statements, _) =
  List.fold_left eval_statement state statements

and eval_statement state = function
  Let     (_,(_,  bindings)) -> eval_let_bindings     state bindings
| LetRec  (_,(_,_,bindings)) -> eval_let_rec_bindings state bindings
| Include _                  -> state

(* To evaluate (possibly parallel) let-bindings, we first project the
   current environment [env] and the bindings are evaluated within
   that same environment, so the left-hand sides cannot be captured in
   the right-hand sides. We also need an empty environment in which to
   store only the new bindings, so we can check that there is exactly
   one for each variable in the left-hand sides. After we are done, we
   resume with the environment we saved just before the bindings and
   we add the new bindings: note the function passed to [Env.union] in
   [eval_let_bindings], as we want a new binding to be able to shadow
   a previous one (before the let construct). In short:

   let x = ...
   let x = ...

   is fine, but not

   let x = ...
   and x = ...

   This property has to be checked deeply in patterns, so

   let (([x::l],(y,_))) = ...
   and u, ([_::x],v) = ...

   is found invalid. The actual binding is done by the function
   [filter], whose parameter [pat_env] is the environment resulting
   only from the bindings in one pattern and is needed to reject
   non-linear patterns like

   let x,x = 1,2

   by raising the exception [Nonlinear_pattern]. By contrast, the
   previous example yields the raising of [Multiple_decl]. In passing,
   note that OCaml does not distinguishes these two kinds of erroneous
   patterns.
*)

and eval_let_bindings state bindings =
  let env   = state.State.env in
  let state = State.{state with env = Env.empty} in
  let state = nsepseq_foldl (eval_let_binding env) state bindings in
  State.{state with env = Env.union (fun _ v -> Some v) env state.env}

and eval_let_binding env state (pattern,_,expr) =
  let thread, value = eval_expr State.{state with env} expr in
  let state = State.{state with thread} in
  fst (filter state Env.empty [pattern] [value])

and filter state pat_env patterns values =
  let rec apply (state, pat_env as acc) pattern value =
    match pattern, value with
      Pvar var, _ ->
        if Env.mem var pat_env then raise (Nonlinear_pattern (state, var));
        if Env.mem var state.State.env then raise (Multiple_decl (state, var));
        State.{state with env = Env.add var value state.env},
        Env.add var value pat_env
    | Pwild _, _ ->
        acc
    | Ppar (_,(_,pattern,_)), _ ->
        apply acc pattern value
    | Punit _, Value.Unit _ -> acc
    | Ptuple (_,patterns), Value.Tuple (_,values) ->
        filter state pat_env (nsepseq_to_list patterns) values
    | Plist (_,(_,patterns,_)), Value.List (_,values) ->
        filter state pat_env (sepseq_to_list patterns) values
    | Pcons (_,(head,_,tail)), Value.List (_, head_val::tail_val) ->
        filter state pat_env [head;tail]
                     [head_val; Value.List (ghost,tail_val)]
    | _ -> raise (Type_error (state, __LOC__)) in
  try List.fold_left2 apply (state, pat_env) patterns values with
    Invalid_argument _ -> raise (Type_error (state, __LOC__))

(* The evaluation of recursive bindings is more complex because the
   left-hand sides can be captured in the right-hand sides, which
   begets the issue of the order of evaluation of those, because

     (1) it should not be observable through side-effects, so
           let rec x = ... and y = ...
         must be indistinguishable from
           let rec y = ... and x = ...
     (2) it must terminate, so
           let rec x = x
         must be rejected.

     A restriction on the syntax of left-hand sides and right-hand
   sides is strong enough to address this issue: we let the only valid
   left-hand sides to be variables and the right-hand sides to be
   either lambdas (that is, anonymous functions) or immediate node
   creations (that is, starting with the "node" keyword). This is a
   strong criterion, because "let rec x = 1 and y = x" is rejected,
   but it is acceptable in practice. In short, the only valid forms
   are:

     let rec x = node ...
     let rec f = fun  ...

   Note that we normalised the AST after parsing, so

     let f x = ...

   already have been rewritten into

     let f = fun x -> ...

   See [AST.norm].

   The implementation of [eval_let_rec_bindings] performs two passes
   on the bindings (divided into three phases).
*)

and eval_let_rec_bindings state = function
       RecFun bindings -> eval_let_rec_fun_bindings  state bindings
| RecNode (_,bindings) -> eval_let_rec_node_bindings state bindings


(* The function [eval_let_rec_fun_bindings] evaluates mutually
   recursive definitions of functions, after normalisation of the
   AST (see [AST.norm]):

     let rec x = fun  ... and ...
*)

and eval_let_rec_fun_bindings state bindings =
  (* Phase 1

     In this first phase, we initialise ghost recursive closures to
     the variable in the left-hand sides. The latter is the
     preparation for _back-patching_, which is used to evaluate
     mutually recursive functions: a reference to [None] is used
     instead of a normal closure (it is indeed simpler to insert the
     option type, as there is then no need for a fake closure waiting
     to be patched with the right environment: the value [None] will
     simply stand in its stead).
  *)

  let env   = state.State.env in
  let state = State.{state with env = Env.empty} in

  let state, ghost_closures =
    let apply (state, closures) (var, _, (reg, (_,param,_,body))) =
      if Env.mem var state.State.env then raise (Multiple_decl (state, var));
      let ghost = ref None in
      let value = Value.RecClos ghost in
      let   env = Env.add var value state.State.env in
      State.{state with env}, (reg,param,body,ghost)::closures
    in nsepseq_foldl apply (state,[]) bindings in

  let env   = Env.union (fun _ v -> Some v) env state.State.env in
  let state = State.{env; thread = state.thread} in

  (* Phase 2

     Back-patching recursive closures with the new environment

     All the bindings in the recursive definitions have been
     collected, the previous references are patched with the actual
     closures and the extended environment: this ties the loops
     through the environment.
  *)

  let () = let patch_back (reg, param, body, ghost) =
             ghost := Some (reg, Value.{param; body; env})
           in List.iter patch_back ghost_closures

  in state


(* The function [eval_let_rec_node_bindings] evaluate mutually
   recursive definitions of neurons, after normalisation of the AST
   (see [AST.norm]):

     let rec x = node ... and ...
*)

and eval_let_rec_node_bindings state bindings =

  (* Phase 1

     In this first phase, we create hidden, anonymous LHS neurons
     bound to the variable in the left-hand sides. *)

  let env   = state.State.env in
  let state = State.{state with env = Env.empty} in

  let state, neu_bind =
    let apply (state, neu_bind) (var, _, expr) =
      let zip = State.(state.thread.zip) in
      if Env.mem var state.State.env then raise (Multiple_decl (state, var));
      let spec = Neuron.{
        name   = "";
        input  = false;
        output = false;
        state  = None;
        debug  = None;
        min    = None
      } in
      let lhs, zip =
        Zipper.add_neuron ?log:State.(state.thread.log) spec zip in
      let env = Env.add var (Value.Neuron (fst var, lhs)) state.State.env
      in State.{env; thread = {state.thread with zip}},
        (var,lhs,expr)::neu_bind
    in nsepseq_foldl apply (state,[]) bindings in

  let env   = Env.union (fun _ v -> Some v) env state.State.env in
  let state = State.{env; thread = state.thread} in

  (* Phase 2

     Evaluation of the RHS neurons and fusing their LHS counterparts.
     Note: A rightward (thus tail-recursive) fold below would have
     been more efficient, but we chose a leftward fold to mimic the
     undocumented order of evaluation of the current OCaml
     compiler. Note also that there is no need here to catch any
     exception raised by [Zipper.fuse].
  *)

  let bind_neuron (var, lhs, expr) state =
    match eval_neu_expr state (Node expr) with
      State.{zip; log; _} as thread, (Value.Neuron (_,rhs) as dst) ->
        let env = Env.add var dst state.State.env
        and zip = Zipper.fuse ?log (lhs,rhs) zip
        in State.{env; thread = {thread with zip}}
    | thread, _ -> State.{state with thread}

  in List.fold_right bind_neuron neu_bind state


(* Evaluation of expressions *)

and eval_expr state = function
      LetExpr (_,e) -> eval_let_expr state e
|         CatExpr e -> eval_cat_expr state e
|  Tuple components -> eval_tuple state components
| Fun (r,(_,x,_,e)) ->
    state.State.thread,
    Value.(Closure (r, {param=x; body=e; env=state.State.env}))
| If (_,(_,cond,_,ifso,_,ifnot)) ->
    match eval_expr state cond with
      thread, Value.Bool (_, true) -> eval_expr State.{state with thread} ifso
    | thread, Value.Bool (_,false) -> eval_expr State.{state with thread} ifnot
    | _ -> raise (Type_error (state, __LOC__))

and eval_tuple state (region,exprs) =
  let apply expr (thread,values) =
    let thread, value = eval_cat_expr State.{state with thread} expr
    in thread, value::values in
  let thread, values =
    nsepseq_foldr apply exprs (state.State.thread,[])
  in thread, Value.Tuple (region,values)

and eval_expr_list state (region,(_,exprs,_)) =
  let apply expr (thread,values) =
    let thread, value = eval_expr State.{state with thread} expr
    in thread, value::values in
  let thread, values =
    sepseq_foldr apply exprs (state.State.thread,[])
  in thread, Value.List (region,values)

and eval_let_expr state = function
  LetIn (_,bindings,_,expr) ->
    eval_expr (eval_let_bindings state bindings) expr
| LetRecIn (_,_,bindings,_,expr) ->
    eval_expr (eval_let_rec_bindings state bindings) expr

and eval_cat_expr state = function
  Cat (reg,(arg1,_,arg2)) ->
    let thread, v1 = eval_cons_expr state arg1 in
    let thread, v2 = eval_cat_expr  State.{state with thread} arg2 in
    (match v1, v2 with
      Value.Str (_,s1), Value.Str (_,s2) -> thread, Value.Str (reg, s1 ^ s2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| ConsExpr e -> eval_cons_expr state e

and eval_cons_expr state = function
  DisjExpr e -> eval_disj_expr state e
| Cons (reg,(head,_,tail)) ->
    let thread, v2 = eval_cons_expr state tail in
    let thread, v1 = eval_disj_expr State.{state with thread} head in
    match v2 with
      Value.List (_,l2) -> thread, Value.List (reg, v1::l2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_disj_expr state = function
  ConjExpr e -> eval_conj_expr state e
| Or (reg,(e1,_,e2)) ->
    let thread, v1 = eval_disj_expr state e1 in
    let thread, v2 = eval_conj_expr State.{state with thread} e2 in
    let open Value in
    match v1, v2 with
      Bool (_,b1), Bool (_,b2) -> thread, Bool (reg, b1 || b2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_conj_expr state = function
  CompExpr e -> eval_comp_expr state e
| And (reg,(e1,_,e2)) ->
    let thread, v1 = eval_conj_expr state e1 in
    let thread, v2 = eval_comp_expr State.{state with thread} e2 in
    let open Value in
    match v1, v2 with
      Bool (_,b1), Bool (_,b2) -> thread, Bool (reg, b1 &&b2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_comp_expr state = function
  Lt (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2 in
    thread,
    let open Value in
    Bool (reg,
          match v1, v2 with
            Integer (_,z1), Integer (_,z2) -> Z.lt z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| LEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2 in
    thread,
    let open Value in
    Bool (reg,
          match v1, v2 with
            Integer (_,z1), Integer (_,z2) -> Z.leq z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| Gt (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2 in
    thread,
    let open Value in
    Bool (reg,
          match v1, v2 with
            Integer (_,z1), Integer (_,z2) -> Z.gt z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| GEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2 in
    thread,
    let open Value in
    Bool (reg,
          match v1, v2 with
            Integer (_,z1), Integer (_,z2) -> Z.geq z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| NEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2
    in thread, Value.(Bool (reg, not (eq thread v1 v2)))

| Eq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_comp_expr state e1 in
    let thread, v2 = eval_add_expr State.{state with thread} e2
    in thread, Value.(Bool (reg, eq thread v1 v2))

| AddExpr e -> eval_add_expr state e

and eval_add_expr state = function
  Add (reg,(e1,_,e2)) ->
    let thread, v1 = eval_add_expr state e1 in
    let thread, v2 = eval_mult_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
       Integer (_,z1), Integer (_,z2) -> Integer (reg, Z.(+) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| AddQ (reg,(e1,_,e2)) ->
    let thread, v1 = eval_add_expr state e1 in
    let thread, v2 = eval_mult_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
       Rational (_,r1), Rational (_,r2) -> Rational (reg, Q.(+) r1 r2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| Sub (reg,(e1,_,e2)) ->
    let thread, v1 = eval_add_expr state e1 in
    let thread, v2 = eval_mult_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
       Integer (_,z1), Integer (_,z2) -> Integer (reg, Z.(-) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| SubQ (reg,(e1,_,e2)) ->
    let thread, v1 = eval_add_expr state e1 in
    let thread, v2 = eval_mult_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
       Rational (_,r1), Rational (_,r2) -> Rational (reg, Q.(-) r1 r2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| MultExpr e -> eval_mult_expr state e

and eval_mult_expr state = function
  Mult (reg,(e1,_,e2)) ->
    let thread, v1 = eval_mult_expr state e1 in
    let thread, v2 = eval_unary_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
      Integer (_,z1), Integer (_,z2) -> Integer (reg, Z.( * ) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| MultQ (reg,(e1,_,e2)) ->
    let thread, v1 = eval_mult_expr state e1 in
    let thread, v2 = eval_unary_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
      Rational (_,r1), Rational (_,r2) -> Rational (reg, Q.( * ) r1 r2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| Div (reg,(e1,_,e2)) ->
    let thread, v1 = eval_mult_expr state e1 in
    let thread, v2 = eval_unary_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
      Integer (_,z1), Integer (_,z2) ->
        if Z.equal z2 Z.zero then
          raise (Div_by_zero (State.{state with thread}, region_of_unary_expr e2))
        else Integer (reg, Z.(/) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| DivQ (reg,(e1,_,e2)) ->
    let thread, v1 = eval_mult_expr state e1 in
    let thread, v2 = eval_unary_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
      Rational (_,r1), Rational (_,r2) ->
        if Q.equal r2 Q.zero then
          raise (Div_by_zero (State.{state with thread}, region_of_unary_expr e2))
        else Rational (reg, Q.(/) r1 r2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| Mod (reg,(e1,_,e2)) ->
    let thread, v1 = eval_mult_expr state e1 in
    let thread, v2 = eval_unary_expr State.{state with thread} e2 in
    thread,
    let open Value in
    (match v1, v2 with
      Integer (_,z1), Integer (_,z2) -> Integer (reg, Z.(mod) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

| UnaryExpr e -> eval_unary_expr state e

and eval_unary_expr state = function
  Primary e -> eval_primary_expr state e
| Not (r,(_,e)) ->
    let thread, v = eval_core_expr state e in
   (match v with
      Value.Bool (_,b) -> thread, Value.Bool (r, not b)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| NegQ (r,(_,e)) ->
    let thread, v = eval_core_expr state e in
   (match v with
      Value.Rational (_,q) -> thread, Value.Rational (r, Q.neg q)
   | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Neg (r,(_,e)) ->
    let thread, v = eval_core_expr state e in
    match v with
      Value.Integer (_,z) -> thread, Value.Integer (r, Z.neg z)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_primary_expr state = function
   GenExpr    e -> eval_gen_expr state e
| NeuExpr     e -> eval_neu_expr state e
| NetExpr (_,e) -> eval_net_expr state e

and eval_gen_expr state = function
  CoreExpr e -> eval_core_expr state e
| CallExpr e -> eval_call_expr state e

and eval_net_expr state (_,e1,e2) =
  let thread, v1 = eval_core_expr state e1 in
  let reg, name  = get_name State.{state with thread} v1
  and zip        = thread.State.zip in
  let msg        = Printf.sprintf "Creating subnet \"%s\".\n" name in
  let ()         = trace msg thread.State.log in
  let zip        = try Zipper.insert_and_go name zip with
                     Zipper.NetCollision _ ->
                       let state = State.{state with thread} in
                       raise (NetCollision (state,(reg,name))) in
  let state      = State.{state with thread = {thread with zip}} in
  let thread, v2 = eval_primary_expr state e2 in
  let zip        = Zipper.ascend thread.State.zip
  in State.{thread with zip}, v2

(* Note: We evaluate the arguments leftwards, as observed with the
   OCaml compiler (undocumented). *)

and eval_call_expr state (reg,(func,arg)) =
  let thread, arg_val = eval_core_expr state arg in
  let thread, fun_val = eval_gen_expr  State.{state with thread} func in
  let open Value in
  match fun_val with
    Closure (_, {param; body; env})
  | RecClos {contents = Some (_, {param; body; env})} ->
      let state = {State.thread; env = Env.add param arg_val env} in
      let thread, v =
        try eval_expr state body with
          Zipper.NotSource (neu,deg) ->
            raise (NotSource (state,(region_of_core_expr arg, neu),deg))
        | Zipper.TwoStates (src,dst) ->
            raise (TwoStates (state,src,(region_of_core_expr arg, dst)))
        | Zipper.Duplicate (src,dst,info) ->
            raise (Duplicate (state,src,(region_of_core_expr arg, dst), info))
      in thread, Value.set_region reg v
  | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and get_name state = function
  Value.Str s -> s
| _ -> raise (Type_error (state, __LOC__))

and mk_neu_state state = function
  Value.Tuple (_,
    Value.[Rational (_,init_v);  Rational (_,init_ge);
           Rational (_,init_gf); Bool (_,init_gate)]) ->
    Neuron.{init_v; init_ge; init_gf; init_gate}
| _ -> raise (Type_error (state, __LOC__))

and to_bool state = function
  Value.Bool (_,b) -> b
| _ -> raise (Type_error (state, __LOC__))

and to_rational state = function
  Value.Rational (_,r) -> r
| _ -> raise (Type_error (state, __LOC__))

and eval_annotations state =
  let apply (state, assoc as acc) (_, {label=_,(_,label); annot}) =
    match annot with
        None -> acc
    | Some e -> let thread, v = eval_core_expr state e in
               State.{state with thread}, (label, v)::assoc
  in List.fold_left apply (state,[])

and eval_neu_expr state expr =
  let zip        = State.(state.thread.zip) in
  let focus_addr = Zipper.addr zip in
  let toplevel   = Net.Addr.is_toplevel focus_addr in

  let neu_reg, annotations, name_expr, syn, input, output =
    match expr with
      Node (reg, (_, annot, name_expr, syn)) ->
        let state_reg, state_binding =
          List.find (fun (_,{label=_,(_,label);_}) -> label = "state") annot in
       (match state_binding.annot with
          Some _ when toplevel -> raise (Toplevel_state (state, state_reg))
        | _  -> reg, annot, name_expr, syn, false, false)

    | Output (reg, (_, annot, name_expr, syn)) ->
        let state_reg, state_binding =
          List.find (fun (_,{label=_,(_,label);_}) -> label = "state") annot in
       (match state_binding.annot with
          Some _ when toplevel -> raise (Toplevel_state (state, state_reg))
        | _  -> reg, annot, name_expr, syn, false, true)

    | Input (reg, (_, annot, name_expr)) ->
        let state_reg, state_binding =
          List.find (fun (_,{label=_,(_,label);_}) -> label = "state") annot in
       (match state_binding.annot with
          Some _ when toplevel -> raise (Toplevel_state (state, state_reg))
        | _ -> reg, annot, name_expr, None, true, false) in

  let state, rname, annot_bindings =
    let thread, v = eval_core_expr state name_expr in
    let state,  b = eval_annotations State.{state with thread} annotations
    in state, get_name state v, b in

  let neu_state =
    Utils.Option.apply (mk_neu_state state)
                       (List.assoc_opt "state" annot_bindings) in

  let debug =
    Utils.Option.apply (to_bool state)
                       (List.assoc_opt "debug" annot_bindings) in

  let min_weight =
    Utils.Option.apply (to_rational state)
                       (List.assoc_opt "min" annot_bindings) in

  (* Add here any other neuron annotations (uses of [annot_bindings]) *)

  let zip = State.(state.thread.zip) in

  let dst, zip =
    let name_reg, name = rname in
    let () =
      if name = "" then
        (if input  then raise (Nameless_input (state, name_reg));
         if output then raise (Nameless_output (state, name_reg))) in
    let spec = Neuron.{
      name;
      input;
      output;
      state = neu_state;
      debug;
      min = min_weight} in
    try Zipper.add_neuron ?log:State.(state.thread.log) spec zip with
      Net.Homograph (_,addr) ->
        let module Exc =
          struct
            exception Found of Region.t (* For flow control *)
          end in
        let apply _var value acc =
          match value with
            Value.Neuron (reg, neu)
              when name = Neuron.get_name neu -> raise (Exc.Found reg)
          | _ -> acc in
        let region =
          try Env.fold apply state.State.env (); ghost with
            Exc.Found region -> region in
        raise (Homograph (state, region, rname, addr)) in

  let state = State.{state with thread = {state.thread with zip}} in

  let thread, adj = eval_synapses state syn in

  let zip = thread.State.zip in

  let zip =
    let apply zip (src,lbl,syn_reg,_) =
      try Zipper.add_synapse Net.{src; lbl; dst} zip with
        Net.Multiple_syn _ ->
          let pred (src',lbl',_,_) =
            let src,  _ = Zipper.resolve ?log:thread.State.log src  zip
            and src', _ = Zipper.resolve ?log:thread.State.log src' zip
            in Neuron.eq src src' && Label.eq lbl lbl' in
          let _, _, syn_reg', _ = List.find pred adj
          and state = State.{state with thread} in
          raise (Multiple_syn (state, src, dst, syn_reg, syn_reg'))
    in List.fold_left apply zip adj

  in State.{thread with zip}, Value.Neuron (neu_reg, dst)

and check_delays state syn = function
  Value.Integer (_,d as n) :: delays ->
    if Z.lt d Z.zero then raise (NonPositiveDelay (state, n))
    else d :: check_delays state syn delays
| [] -> []
| _ -> raise (Type_error (state, __LOC__))

and check_rationals state syn = function
  Value.Rational (_,w) :: weights ->
    w :: check_rationals state syn weights
| [] -> []
| _ -> raise (Type_error (state, __LOC__))

and check_booleans state syn = function
  Value.Bool (_,b) :: weights ->
    b :: check_booleans state syn weights
| [] -> []
| _ -> raise (Type_error (state, __LOC__))

and eval_synapses state = function
          None -> state.State.thread, []
| Some (_,syn) ->
    let apply (thread,adj) (syn_reg,syn) =
      let thread, weights =
        eval_core_expr State.{state with thread} syn.weight in
      let thread, delays =
        eval_core_expr State.{state with thread} syn.delay in
      let state = State.{state with thread} in
      let delays =
        match delays with
          Value.List (_,delays) -> check_delays state syn delays
        |                 delay -> check_delays state syn [delay] in
      let labels =
        try
          match snd syn.kind with
            V ->
              let weights =
                match weights with
                  Value.List (_,weights) -> check_rationals state syn weights
                |                 weight -> check_rationals state syn [weight]
              in List.map2 Label.v weights delays
          | Ge ->
              let weights =
                match weights with
                  Value.List (_,weights) -> check_rationals state syn weights
                |                 weight -> check_rationals state syn [weight]
              in List.map2 Label.ge weights delays
          | Gf ->
              let weights =
                match weights with
                  Value.List (_,weights) -> check_rationals state syn weights
                |                 weight -> check_rationals state syn [weight]
              in List.map2 Label.gf weights delays
          | Gate ->
              let weights =
                match weights with
                  Value.List (_,weights) -> check_booleans state syn weights
                |                 weight -> check_booleans state syn [weight]
              in List.map2 Label.gate weights delays
        with Invalid_argument _ ->
          raise (Type_error (state, __LOC__)) in
      let thread, v = eval_gen_expr state syn.source in
      let sources =
        extract_neurons State.{state with thread} labels syn_reg adj
          (if snd syn.map then v else Value.List (Value.to_region v, [v]))
      in thread, sources in
    nseq_foldl apply (state.State.thread,[]) syn

and extract_neurons state labels syn_reg adj = function
  Value.List (_, values) ->
    let collect adj label = function
      Value.Neuron (reg, src_neu) -> (src_neu, label, syn_reg, reg)::adj
    | _ -> raise (Type_error (state, __LOC__)) in
    (try List.fold_left2 collect adj labels values with
       Invalid_argument _ -> raise (Type_error (state, __LOC__)))
| _ -> raise (Type_error (state, __LOC__))

and eval_core_expr state =
  let thread = state.State.thread in function
        Q (r,(_,q)) -> thread, Value.Rational (r,q)
  |             Z z -> thread, Value.Integer z
  |        String s -> thread, Value.Str s
  |      Unit (r,_) -> thread, Value.Unit r
  |        True reg -> thread, Value.Bool (reg, true)
  |       False reg -> thread, Value.Bool (reg, false)
  |  Var (r,_ as v) -> thread, Value.set_region r (Env.find v state.State.env)
  | Par (_,(_,e,_)) -> eval_expr state e
  |          List l -> eval_expr_list state l
  |        Extern e -> eval_external state e

and eval_external state = function
    Fuse e -> eval_fuse_expr    state e
|   Cast e -> eval_cast_expr    state e
|  Print e -> eval_print_expr   state e
|  Scanf e -> eval_scanf_expr   state e
|  QComp e -> eval_qcomp_expr   state e
| NodeEq e -> eval_node_eq_expr state e
| PolyEq e -> eval_polyeq_expr  state e
| Str2Ints e -> eval_str2ints   state e (* TEMPORARY *)

and eval_str2ints state var =
  let rec string_to_int_list i s =
    if i < String.length s
    then Char.code s.[i] :: string_to_int_list (i+1) s
    else [] in
  let string_to_int_list s = string_to_int_list 0 s in
  match Env.find (ghost, var) state.State.env with
    Value.Str (_,s) ->
      state.State.thread,
      Value.List (ghost,
                  List.map (fun n -> Value.Integer (ghost, Z.of_int n))
                           (string_to_int_list (Scanf.unescaped s)))
  | _ -> raise (Type_error (state, __LOC__))

and eval_polyeq_expr state (x,y) =
  let thread = state.State.thread in
  let x = Env.find (ghost,x) state.State.env
  and y = Env.find (ghost,y) state.State.env
  in thread, Value.(Bool (ghost, eq thread x y))

and eval_node_eq_expr state (x,y) =
  let thread = state.State.thread in
  let x = Env.find (ghost,x) state.State.env
  and y = Env.find (ghost,y) state.State.env in
  let open Value in
  match x, y with
    Neuron _, Neuron _ -> thread, Bool (ghost, eq thread x y)
  | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_qcomp_expr state =
  let thread = state.State.thread and env = state.State.env
  in function
   EqQ (x,y) -> (match Env.find (ghost,x) env, Env.find (ghost,y) env with
                 Value.Rational (_,q1), Value.Rational (_,q2) ->
                   thread, Value.Bool (ghost, Q.equal q1 q2)
                | _ -> raise (Type_error (state, __LOC__)))
|  LtQ (x,y) -> (match Env.find (ghost,x) env, Env.find (ghost,y) env with
                 Value.Rational (_,q1), Value.Rational (_,q2) ->
                   thread, Value.Bool (ghost, Q.lt q1 q2)
                | _ -> raise (Type_error (state, __LOC__)))
| LeqQ (x,y) -> (match Env.find (ghost,x) env, Env.find (ghost,y) env with
                 Value.Rational (_,q1), Value.Rational (_,q2) ->
                   thread, Value.Bool (ghost, Q.leq q1 q2)
                | _ -> raise (Type_error (state, __LOC__)))
|  GtQ (x,y) -> (match Env.find (ghost,x) env, Env.find (ghost,y) env with
                 Value.Rational (_,q1), Value.Rational (_,q2) ->
                   thread, Value.Bool (ghost, Q.gt q1 q2)
                | _ -> raise (Type_error (state, __LOC__)))
| GeqQ (x,y) -> (match Env.find (ghost,x) env, Env.find (ghost,y) env with
                 Value.Rational (_,q1), Value.Rational (_,q2) ->
                   thread, Value.Bool (ghost, Q.geq q1 q2)
                | _ -> raise (Type_error (state, __LOC__)))

and eval_print_expr state = function
  PrintString var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Str (r,s) ->
        print_string (Scanf.unescaped s); flush stdout;
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintInt var ->
   (match Env.find (ghost, var) state.State.env with
       Value.Integer (r,n) ->
         print_string (Z.to_string n); flush stdout;
         (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintBool var ->
   (match Env.find (ghost, var) state.State.env with
       Value.Bool (r,b) ->
         print_string (string_of_bool b); flush stdout;
         (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintWe var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,_) as v ->
        Printf.printf "%s *. we%!" (Value.to_string v);
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintSlope var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,_) as v ->
        Printf.printf "%s *. slope%!" (Value.to_string v);
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintGmul var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,_) as v ->
        Printf.printf "%s *. gmul%!" (Value.to_string v);
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintDec var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,_) as v ->
        Printf.printf "%s%!" (Value.to_string v);
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

and eval_scanf_expr state = function
  ScanfInt var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Unit r ->
        let z = Z.of_int Scanf.(bscanf Scanning.stdin "%d\n" (fun x -> x))
        in state.State.thread, Value.Integer (r,z)
    | _ -> raise (Type_error (state, __LOC__)))

| ScanfString var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Unit r ->
        state.State.thread,
        Value.Str (r, Scanf.(bscanf Scanning.stdin "%s\n" (fun x -> x)))
    | _ -> raise (Type_error (state, __LOC__)))

| ScanfBool var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Unit r ->
        state.State.thread,
        Value.Bool (r, Scanf.(bscanf Scanning.stdin "%B\n" (fun x -> x)))
    | _ -> raise (Type_error (state, __LOC__)))

| ScanfDec var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Unit r ->
        let s = Scanf.(bscanf Scanning.stdin "%s\n" (fun x -> x)) in
        state.State.thread, Value.Rational (r, Q.of_string s)
    | _ -> raise (Type_error (state, __LOC__)))

and eval_cast_expr state = function
  StringOfZ var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Integer (r,z) -> state.State.thread, Value.Str (r, Z.to_string z)
    | _ -> raise (Type_error (state, __LOC__)))

| StringOfBool var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Bool (r,b) -> state.State.thread, Value.Str (r, string_of_bool b)
    | _ -> raise (Type_error (state, __LOC__)))

| StringOfWe var | StringOfSlope var | StringOfGmul var | StringOfDec var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,q) -> state.State.thread, Value.Str (r, Q.to_string q)
    | _ -> raise (Type_error (state, __LOC__)))

| Ratio var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Integer (r,z) -> state.State.thread, Value.Rational (r, Q.of_bigint z)
    | _ -> raise (Type_error (state, __LOC__)))

| Trunc var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Rational (r,q) -> state.State.thread, Value.Integer (r, Q.to_bigint q)
    | _ -> raise (Type_error (state, __LOC__)))

| NameOfNode var ->
   let thread = state.State.thread in
  (match Env.find (ghost, var) state.State.env with
     Value.Neuron (r,neu) ->
       let neu, _ = Zipper.resolve ?log:thread.State.log neu thread.State.zip
       in thread, Value.Str (r, Neuron.get_name neu)
   | _ -> raise (Type_error (state, __LOC__)))

| AddrOfNode var ->
    let thread = state.State.thread in
   (match Env.find (ghost, var) state.State.env with
      Value.Neuron (r,neu) ->
        let neu, _ = Zipper.resolve ?log:thread.State.log neu thread.State.zip in
        let addr = List.rev (let hd, tail = Neuron.get_addr neu in hd::tail)
        in thread, Value.List (r, List.map (fun name -> Value.Str (r,name)) addr)
    | _ -> raise (Type_error (state, __LOC__)))

(* Note that any exception raised by [Zipper.fuse] is catched in
   [eval_core_expr], where more context is available to re-raise an
   exception with more information. *)

and eval_fuse_expr state (src,dst) =
  let thread = state.State.thread in
  let zip = thread.State.zip in
  match Env.find (ghost, src) state.State.env with
    Value.Neuron (_, src) ->
      (match Env.find (ghost, dst) state.State.env with
         Value.Neuron (_, dst) ->
           let zip = Zipper.fuse ?log:thread.State.log (src,dst) zip
           in State.{state.thread with zip}, Value.Unit ghost
       | _ -> raise (Type_error (state, __LOC__)))
  | _ -> raise (Type_error (state, __LOC__))


(* Initialising the interpreter's state *)

(* The value of [add_fuse state] is a copy of the state [state] whose
   environment has been extended with a binding to the external
   definition of the function "fuse". More precisely, what is added to
   the value environment is a binding from the string "fuse" to the
   AST of a function whose concrete syntax would be like "fun (src,
   dst) -> <AST of external definition of fuse>". That AST is never
   produced by the parser, for which "fuse" is just a variable (and a
   keyword), and is interpreted specially by [eval_fuse_expr].

   The same goes for the other functions below.
*)

let add_fuse state =
  let src, dst     = (ghost, "src"), (ghost, "dst")
  and arrow, comma = ghost, ghost in
  let ptuple       = Ptuple (ghost, (Pvar src, [comma, Pvar dst])) in
  let body         = core_to_expr (Extern (Fuse ("src", "dst"))) in
  let body         = norm (ptuple,[]) arrow body in
  let thread, fuse = eval_expr state (Fun body) in
  let env          = Env.add (ghost,"fuse") fuse state.State.env
  in State.{env; thread}

let add_casts state =
  let param = "x"
  and extern cast = core_to_expr (Extern (Cast cast)) in
  let string_of_int   = extern (StringOfZ param)
  and string_of_bool  = extern (StringOfBool param)
  and string_of_we    = extern (StringOfWe param)
  and string_of_slope = extern (StringOfSlope param)
  and string_of_gmul  = extern (StringOfGmul param)
  and string_of_dec   = extern (StringOfDec param)
  and int_of_ratio    = extern (Trunc param)
  and ratio_of_int    = extern (Ratio param)
  and name_of_node    = extern (NameOfNode param)
  and addr_of_node    = extern (AddrOfNode param) in
  let mk_clos body =
    Value.(Closure (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost,"string_of_int")   (mk_clos string_of_int)
          |> Env.add (ghost,"string_of_bool")  (mk_clos string_of_bool)
          |> Env.add (ghost,"string_of_we")    (mk_clos string_of_we)
          |> Env.add (ghost,"string_of_slope") (mk_clos string_of_slope)
          |> Env.add (ghost,"string_of_gmul")  (mk_clos string_of_gmul)
          |> Env.add (ghost,"string_of_dec")   (mk_clos string_of_dec)
          |> Env.add (ghost,"ratio")           (mk_clos ratio_of_int)
          |> Env.add (ghost,"trunc")           (mk_clos int_of_ratio)
          |> Env.add (ghost,"name_of_node")    (mk_clos name_of_node)
          |> Env.add (ghost,"addr_of_node")    (mk_clos addr_of_node)
  in State.{state with env}

let add_prints state =
  let param = "x"
  and extern print = core_to_expr (Extern (Print print)) in
  let print_string = extern (PrintString param)
  and print_int    = extern (PrintInt param)
  and print_bool   = extern (PrintBool param)
  and print_we     = extern (PrintWe param)
  and print_slope  = extern (PrintSlope param)
  and print_gmul   = extern (PrintGmul param)
  and print_dec    = extern (PrintDec param) in
  let mk_clos body =
    Value.(Closure (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost, "print_string") (mk_clos print_string)
          |> Env.add (ghost, "print_int")    (mk_clos print_int)
          |> Env.add (ghost, "print_bool")   (mk_clos print_bool)
          |> Env.add (ghost, "print_we")     (mk_clos print_we)
          |> Env.add (ghost, "print_slope")  (mk_clos print_slope)
          |> Env.add (ghost, "print_gmul")   (mk_clos print_gmul)
          |> Env.add (ghost, "print_dec")    (mk_clos print_dec)
  in State.{state with env}

let add_scanf state =
  let param = "x"
  and extern scanf = core_to_expr (Extern (Scanf scanf)) in
  let scanf_int    = extern (ScanfInt param)
  and scanf_string = extern (ScanfString param)
  and scanf_bool   = extern (ScanfBool param)
  and scanf_dec    = extern (ScanfDec param) in
  let mk_clos body =
    Value.(Closure (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost, "scanf_int")    (mk_clos scanf_int)
          |> Env.add (ghost, "scanf_string") (mk_clos scanf_string)
          |> Env.add (ghost, "scanf_bool")   (mk_clos scanf_bool)
          |> Env.add (ghost, "scanf_dec")    (mk_clos scanf_dec)
  in State.{state with env}

let add_weights state =
  let app env name =
    Env.add (ghost, name) (Value.Rational (ghost, Q.of_float 1.0)) env in
  let env = List.fold_left app state.State.env ["we"; "slope"; "gmul"]
  in State.{state with env}

let add_qcomp state =
  let ghost_fun = ghost and ghost_arrow = ghost in
  let extern comp =
    Fun (ghost, (ghost_fun, (ghost, "y"), ghost_arrow,
                 core_to_expr (Extern (QComp (comp ("x","y")))))) in
  let weight_eq  = extern (fun p -> EqQ  p)
  and weight_lt  = extern (fun p -> LtQ  p)
  and weight_leq = extern (fun p -> LeqQ p)
  and weight_gt  = extern (fun p -> GtQ  p)
  and weight_geq = extern (fun p -> GeqQ p) in
  let mk_clos body =
    Value.(Closure (ghost, {param=ghost,"x"; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost, "weight_eq")  (mk_clos weight_eq)
          |> Env.add (ghost, "weight_lt")  (mk_clos weight_lt)
          |> Env.add (ghost, "weight_leq") (mk_clos weight_leq)
          |> Env.add (ghost, "weight_gt")  (mk_clos weight_gt)
          |> Env.add (ghost, "weight_geq") (mk_clos weight_geq)
  in State.{state with env}

let add_equal state =
  let ghost_fun = ghost and ghost_arrow = ghost in
  let equal = Fun (ghost, (ghost_fun, (ghost, "y"), ghost_arrow,
                           core_to_expr (Extern (PolyEq ("x","y"))))) in
  let closure =
    Value.(Closure (ghost, {param=ghost,"x"; body=equal; env=Env.empty})) in
  let env = Env.add (ghost, "equal") closure state.State.env
  in State.{state with env}

(* TEMPORARY *)

let add_string_to_int_list state =
  let equal =  core_to_expr (Extern (Str2Ints "x")) in
  let closure =
    Value.(Closure (ghost, {param=ghost,"x"; body=equal; env=Env.empty})) in
  let env = Env.add (ghost, "string_to_int_list") closure state.State.env
  in State.{state with env}

let add_prelude =
   add_fuse <@ add_casts <@ add_prints <@ add_scanf <@ add_weights
<@ add_qcomp <@ add_equal <@ add_string_to_int_list


(* Main entry

   The main function exported by this module is [eval]. First, the
   initial state is built (if no evaluation environment is provided,
   the prelude is added --- see above). Then the statements in the AST
   are evaluated in sequence, and the zipper of the final state is
   made to apply to the whole network tree (using [Zipper.zip_up]).
*)

let eval ?log ?dot ?net ?stats ~top =
  let zip   = Zipper.init top in
  let state = State.{env=Env.empty; thread = {zip; log; dot; net; stats}} in
  fun ?env ast ->
    let state =
      match env with
            None -> add_prelude state
      | Some env -> State.{state with env} in
    let state = eval_statements state ast in
    let zip   = Zipper.zip_up State.(state.thread.zip)
    in State.{state with thread = {state.thread with zip}}
