(* The interpreter for Mini-ML *)

open Utils
open AST

let ghost = Region.ghost

(* Semantic values environment *)

module rec Env : sig
  type t = Value.t String.Map.t

  exception Unbound of rvar

  val empty     : t
  val add       : rvar -> Value.t -> t -> t
  val find      : rvar -> t -> Value.t
  val mem       : rvar -> t -> bool
  val union     : (Value.t -> Value.t -> Value.t option) -> t -> t -> t
  val fold      : (string -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_string : t -> string
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

and Value : sig
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Int     of Z.t reg
  | Str     of string reg
  | Bool    of bool reg
  | Unit    of Region.t
  | Tuple   of t list reg
  | List    of t list reg
  | Clos    of closure reg
  | RecClos of closure reg option ref

  val eq         : State.thread -> t -> t -> bool
  val to_string  : t -> string
  val to_region  : t -> Region.t
  val set_region : Region.t -> t -> t
end
=
struct
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Int     of Z.t reg
  | Str     of string reg
  | Bool    of bool reg
  | Unit    of Region.t
  | Tuple   of t list reg
  | List    of t list reg
  | Clos    of closure reg
  | RecClos of closure reg option ref

  let to_region : t -> Region.t = function
    Int (reg,_) | Str (reg,_) | Bool (reg,_)
  | Unit reg | Tuple (reg,_) | List (reg,_)
  | Clos (reg,_) | RecClos {contents = Some (reg,_)} -> reg
  | RecClos {contents=None} -> ghost

  let rec to_string : t -> string = function
    Int (_,z)        -> Z.to_string z
  | Str (_,s)        -> "\"" ^ s ^ "\""
  | Bool (_,b)       -> string_of_bool b
  | Unit _           -> "()"
  | Clos _
  | RecClos _        -> "a closure"
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

  let set_region r : t -> t = function
     Int  (_,z) -> Int (r,z)
  |  Str  (_,s) -> Str (r,s)
  | Bool  (_,b) -> Bool (r,b)
  |      Unit _ -> Unit r
  | Tuple (_,t) -> Tuple (r,t)
  |  List (_,l) -> List (r,l)
  |  Clos (_,c) -> Clos (r,c)
  | RecClos {contents = Some (_,c)}
                -> RecClos {contents = Some (r,c)}
  |   RecClos c -> RecClos c

  let rec eq thread (v1: t) (v2: t) =
    match v1, v2 with
      Int (_,z1),     Int (_,z2) -> Z.equal z1 z2
    | Str (_,s1),     Str (_,s2) -> s1 = s2
    | Bool (_,b1),   Bool (_,b2) -> b1 = b2
    | Unit _,             Unit _ -> true
    | Tuple (_,l1), Tuple (_,l2)
    | List (_,l1),   List (_,l2) ->
       (try List.for_all2 (eq thread) l1 l2 with Invalid_argument _ -> false)
    | _ -> false
end

(* State *)

and State : sig
  type thread = < >
  type t = {env: Env.t; thread: thread}
  type state = t
end = State

(* Errors expected to be caught by OCaml static typing *)

exception Type_error        of State.t * string
exception Nonlinear_pattern of State.t * rvar
exception Multiple_decl     of State.t * rvar
exception Incomplete_match  of State.t

(* Errors not caught by OCaml static typing *)

exception Div_by_zero of State.t * Region.t

(* Evaluation *)

(* The evaluation of top-level bindings (called statements below) is
   slightly different from other constructs as the evaluator state
   must be threaded in stead of only the proper thread. The reason is
   that the evaluation of a statement enriches the environment and
   that extended environment must then be used to evaluate the next
   statement, whereas in the case of subexpressions or local bindings,
   the environment only flows top-down (that is, local bindings cannot
   escape). *)

let rec eval_statements state (statements, _) =
  List.fold_left eval_statement state statements

and eval_statement state = function
  Let    (_,(_,  bindings)) -> eval_let_bindings     state bindings
| LetRec (_,(_,_,bindings)) -> eval_let_rec_bindings state bindings

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
   note that OCaml does not distinguish those two kinds of erroneous
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
    | Pint (_,m), Value.Int (_,n) when Z.equal m n -> acc
    | Ptrue _, Value.Bool (_, true) -> acc
    | Pfalse _, Value.Bool (_, false) -> acc
    | Pstr (_,s1), Value.Str (_,s2) when s1 = s2 -> acc
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
   lambdas, that is, anonymous functions. This is a strong criterion,
   because "let rec x = 1 and y = x" is rejected, even though it would
   be meaningful. In short, the only valid form is

     let rec f = fun  ...

   Note that we normalised the AST after parsing, so

     let f x = ...

   already have been rewritten into

     let f = fun x -> ...

   See [AST.norm].

   The implementation of [eval_let_rec_bindings] performs two passes
   on the bindings (divided into three phases).
 *)


(* The function [eval_let_rec_fun_bindings] evaluates mutually
   recursive definitions of functions, after normalisation of the
   AST (see [AST.norm]):

     let rec x = fun  ... and ...
*)

and eval_let_rec_bindings state bindings =
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

(* Evaluation of expressions *)

and eval_expr state = function
      LetExpr (_,e) -> eval_let_expr state e
|  Tuple components -> eval_tuple state components
|       Match (_,e) -> eval_match state e
| Fun (r,(_,x,_,e)) ->
    state.State.thread,
    Value.(Clos (r, {param=x; body=e; env=state.State.env}))
| If (_,(_, cond,_, if_true, _, otherwise)) ->
    let thread, value = eval_expr state cond in
    let eval = eval_expr State.{state with thread} in
    (match value with
       Value.Bool (_, true)  -> eval if_true
     | Value.Bool (_, false) -> eval otherwise
     | _ -> raise (Type_error (state, __LOC__)))
| Cat (reg,(arg1,_,arg2)) ->
    let thread, v1 = eval_expr state arg1 in
    let thread, v2 = eval_expr State.{state with thread} arg2 in
    (match v1, v2 with
      Value.Str (_,s1), Value.Str (_,s2) -> thread, Value.Str (reg, s1 ^ s2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Cons (reg,(head,_,tail)) ->
    let thread, v2 = eval_expr state tail in
    let thread, v1 = eval_expr State.{state with thread} head in
    (match v2 with
      Value.List (_,l2) -> thread, Value.List (reg, v1::l2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Or (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    let open Value in
    (match v1, v2 with
      Bool (_,b1), Bool (_,b2) -> thread, Bool (reg, b1 || b2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| And (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    let open Value in
    (match v1, v2 with
      Bool (_,b1), Bool (_,b2) -> thread, Bool (reg, b1 &&b2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
|  Lt (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    Value.Bool (reg,
          match v1, v2 with
            Value.Int (_,z1), Value.Int (_,z2) -> Z.lt z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| LEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    Value.Bool (reg,
          match v1, v2 with
            Value.Int (_,z1), Value.Int (_,z2) -> Z.leq z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Gt (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    Value.Bool (reg,
          match v1, v2 with
            Value.Int (_,z1), Value.Int (_,z2) -> Z.gt z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| GEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    Value.Bool (reg,
          match v1, v2 with
            Value.Int (_,z1), Value.Int (_,z2) -> Z.geq z1 z2
          | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| NEq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2
    in thread, Value.(Bool (reg, not (eq thread v1 v2)))
| Eq (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2
    in thread, Value.(Bool (reg, eq thread v1 v2))
| Add (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    (match v1, v2 with
       Value.Int (_,z1), Value.Int (_,z2) -> Value.Int (reg, Z.(+) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Sub (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    (match v1, v2 with
       Value.Int (_,z1), Value.Int (_,z2) -> Value.Int (reg, Z.(-) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Mult (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    (match v1, v2 with
      Value.Int (_,z1), Value.Int (_,z2) -> Value.Int (reg, Z.( * ) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Div (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    (match v1, v2 with
      Value.Int (_,z1), Value.Int (_,z2) ->
        if Z.equal z2 Z.zero then
          raise (Div_by_zero (State.{state with thread},
                              region_of_expr e2))
        else Value.Int (reg, Z.(/) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Mod (reg,(e1,_,e2)) ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr State.{state with thread} e2 in
    thread,
    (match v1, v2 with
      Value.Int (_,z1), Value.Int (_,z2) -> Value.Int (reg, Z.(mod) z1 z2)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Not (r,(_,e)) ->
    let thread, v = eval_expr state e in
    (match v with
      Value.Bool (_,b) -> thread, Value.Bool (r, not b)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))
| Neg (r,(_,e)) ->
    let thread, v = eval_expr state e in
    (match v with
      Value.Int (_,z) -> thread, Value.Int (r, Z.neg z)
    | _ -> raise (Type_error (State.{state with thread}, __LOC__)))

|          Call e -> eval_call_expr state e
|           Int z -> state.State.thread, Value.Int z
|           Str s -> state.State.thread, Value.Str s
|      Unit (r,_) -> state.State.thread, Value.Unit r
|        True reg -> state.State.thread, Value.Bool (reg, true)
|       False reg -> state.State.thread, Value.Bool (reg, false)
|  Var (r,_ as v) -> state.State.thread,
                    Value.set_region r (Env.find v state.State.env)
| Par (_,(_,e,_)) -> eval_expr state e
|          List l -> eval_expr_list state l
|        Extern e -> eval_external state e

and eval_match state (_, expr, _, cases) =
  let env = state.State.env in
  let thread, value = eval_expr state expr in
  let state = State.{thread; env = Env.empty}
  in eval_cases env state value cases

and eval_cases env state value (case, cases) =
  let pattern, _, expr = case in
  try
    let state = fst (filter state Env.empty [pattern] [value]) in
    let env = Env.union (fun _ v -> Some v) env state.State.env
    in eval_expr State.{state with env} expr
  with Type_error _ ->
    match cases with
              [] -> raise (Incomplete_match state)
    | (_,hd)::tl -> eval_cases env state value (hd,tl)

and eval_tuple state (region,exprs) =
  let apply expr (thread,values) =
    let thread, value = eval_expr State.{state with thread} expr
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

(* Note: We evaluate the arguments leftwards, as observed with the
   OCaml compiler (undocumented). *)

and eval_call_expr state (reg,(func,arg)) =
  let thread, arg_val = eval_expr state arg in
  let thread, fun_val = eval_expr State.{state with thread} func in
  let open Value in
  match fun_val with
    Value.Clos (_, {param; body; env})
  | RecClos {contents = Some (_, {param; body; env})} ->
      let state = {State.thread; env = Env.add param arg_val env} in
      let thread, v = eval_expr state body in
      thread, Value.set_region reg v
  | _ -> raise (Type_error (State.{state with thread}, __LOC__))

and eval_external state = function
    Cast e -> eval_cast_expr   state e
|  Print e -> eval_print_expr  state e
|  Scanf e -> eval_scanf_expr  state e
| PolyEq e -> eval_polyeq_expr state e

and eval_polyeq_expr state (x,y) =
  let thread = state.State.thread in
  let x = Env.find (ghost,x) state.State.env
  and y = Env.find (ghost,y) state.State.env
  in thread, Value.(Bool (ghost, eq thread x y))

and eval_print_expr state = function
  PrintString var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Str (r,s) ->
        print_string (Scanf.unescaped s); flush stdout;
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintInt var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Int (r,n) ->
        print_string (Z.to_string n); flush stdout;
        (state.State.thread, Value.Unit r)
    | _ -> raise (Type_error (state, __LOC__)))

and eval_scanf_expr state = function
  ScanfInt var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Unit r ->
        let z = Z.of_int Scanf.(bscanf Scanning.stdin "%d\n" (fun x -> x))
        in state.State.thread, Value.Int (r,z)
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

and eval_cast_expr state = function
  StringOfInt var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Int (r,z) -> state.State.thread, Value.Str (r, Z.to_string z)
    | _ -> raise (Type_error (state, __LOC__)))

| StringOfBool var ->
   (match Env.find (ghost, var) state.State.env with
      Value.Bool (r,b) -> state.State.thread, Value.Str (r, string_of_bool b)
    | _ -> raise (Type_error (state, __LOC__)))

(* Initialising the interpreter's state *)

let add_casts state =
  let param          = "x"
  and extern cast    = Extern (Cast cast) in
  let string_of_int  = extern (StringOfInt param)
  and string_of_bool = extern (StringOfBool param) in
  let mk_clos body =
    Value.(Clos (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost,"string_of_int")  (mk_clos string_of_int)
          |> Env.add (ghost,"string_of_bool") (mk_clos string_of_bool)
  in State.{state with env}

let add_prints state =
  let param        = "x"
  and extern print = Extern (Print print) in
  let print_string = extern (PrintString param)
  and print_int    = extern (PrintInt param) in
  let mk_clos body =
    Value.(Clos (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost, "print_string") (mk_clos print_string)
          |> Env.add (ghost, "print_int")    (mk_clos print_int)
  in State.{state with env}

let add_scanf state =
  let param        = "x"
  and extern scanf = Extern (Scanf scanf) in
  let scanf_int    = extern (ScanfInt param)
  and scanf_string = extern (ScanfString param)
  and scanf_bool   = extern (ScanfBool param) in
  let mk_clos body =
    Value.(Clos (ghost, {param=ghost,param; body; env=Env.empty})) in
  let env = state.State.env
          |> Env.add (ghost, "scanf_int")    (mk_clos scanf_int)
          |> Env.add (ghost, "scanf_string") (mk_clos scanf_string)
          |> Env.add (ghost, "scanf_bool")   (mk_clos scanf_bool)
  in State.{state with env}

let add_equal state =
  let ghost_fun = ghost and ghost_arrow = ghost in
  let equal = Fun (ghost, (ghost_fun, (ghost, "y"), ghost_arrow,
                           Extern (PolyEq ("x","y")))) in
  let closure =
    Value.(Clos (ghost, {param=ghost,"x"; body=equal; env=Env.empty})) in
  let env = Env.add (ghost, "equal") closure state.State.env
  in State.{state with env}

let add_prelude = add_casts <@ add_prints <@ add_scanf <@ add_equal


(* Main entry

   The main function exported by this module is [eval]. First, the
   initial state is built. Note that we do not have a use here for the
   thread, but we keep for future developments. If no evaluation
   environment is provided, the prelude is added. Then the statements
   in the AST are evaluated in sequence.  *)

let eval =
  let state = State.{env=Env.empty; thread = object end} in
  fun ?env ast ->
    let state =
      match env with
            None -> add_prelude state
      | Some env -> State.{state with env}
    in eval_statements state ast
