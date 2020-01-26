(* The interpreter for Mini-ML *)

open Utils
open AST

(* Semantic values environment *)

module rec Env :
  sig
    type t = Value.t String.Map.t

    exception Unbound of rvar

    val empty     : t
    val add       : rvar -> Value.t -> t -> t
    val add_id    : var -> Value.t -> t -> t
    val find      : rvar -> t -> Value.t
    val find_id   : var -> t -> Value.t
    val mem       : rvar -> t -> bool
    val union     : (Value.t -> Value.t -> Value.t option) -> t -> t -> t
    val fold      : (string -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a
    val to_string : t -> string
  end
= struct
    type t = Value.t String.Map.t

    exception Unbound of rvar

    let empty     = String.Map.empty
    let add_id v = String.Map.add v
    let add v = add_id v.Region.value
    let mem v = String.Map.mem v.Region.value
    let union f   = String.Map.union (lambda f)
    let fold      = String.Map.fold

    let find_id var env =
      try String.Map.find var env with
        Not_found -> raise (Unbound (Region.wrap_ghost var))

    let find var = find_id var.Region.value

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

  type value = t

  open! Region

  let to_region : value -> Region.t = function
    Int {region; _} | Str {region; _} | Bool {region; _}
  | Unit region | Tuple {region; _} | List {region; _}
  | Clos {region; _} | RecClos {contents = Some {region; _}} -> region
  | RecClos {contents=None} -> ghost

  let rec to_string : value -> string = function
    Int {value; _}        -> Z.to_string value
  | Str {value; _}        -> Printf.sprintf "\"%s\"" value
  | Bool {value; _}   -> string_of_bool value
  | Unit _           -> "()"
  | Clos _
  | RecClos _        -> "a closure"
  | Tuple {value; _} ->
      let apply e = function
        "" -> to_string e
      |  a -> to_string e ^ ", " ^ a
      in Printf.sprintf "(%s)" (List.fold_right apply value "")
  | List {value; _} ->
      let apply e = function
        "" -> to_string e
      |  a -> to_string e ^ "; " ^ a
      in Printf.sprintf "[%s]" (List.fold_right apply value "")

  let set_region region : value -> value = function
     Int  z -> Int {z with region}
  |  Str  s -> Str {s with region}
  | Bool  b -> Bool {b with region}
  |      Unit _ -> Unit region
  | Tuple t -> Tuple {t with region}
  |  List l -> List {l with region}
  |  Clos c -> Clos {c with region}
  | RecClos {contents = Some c}
                -> RecClos {contents = Some {c with region}}
  |   RecClos c -> RecClos c

  let rec eq thread (v1: value) (v2: value) =
    match v1, v2 with
      Int z1,     Int z2 -> Z.equal z1.value z2.value
    | Str s1,     Str s2 -> s1.value = s2.value
    | Bool b1,   Bool b2 -> b1.value = b2.value
    | Unit _,     Unit _ -> true
    | Tuple l1, Tuple l2
    | List l1,  List l2 ->
      (try List.for_all2 (eq thread) l1.value l2.value
       with Invalid_argument _ -> false)
    | _ -> false
end

(* State *)

and State :
  sig
    type thread = < >

    type t = <
      env        : Env.t;
      thread     : thread;
      set_env    : Env.t -> t;
      set_thread : thread -> t
    >

    type state = t

    val make : env:Env.t -> thread:thread -> t
  end
= struct
    type thread = < >

    type t = <
      env        : Env.t;
      thread     : thread;
      set_env    : Env.t -> t;
      set_thread : thread -> t
    >

    type state = t

    let make ~(env:Env.t) ~(thread:thread) =
      object
        val    env = env
        method env = env
        val thread = thread
        method thread = thread
        method set_env env = {< env = env >}
        method set_thread thread = {< thread = thread >}
      end
  end

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

open Region

let rec eval_statements state (statements, _) =
  List.fold_left eval_statement state statements

and eval_statement state = function
  Let    {value=_, bindings; _} -> eval_let_bindings     state bindings
| LetRec {value=_,_,bindings; _} -> eval_let_rec_bindings state bindings

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
  let env   = state#env in
  let state = state#set_env Env.empty in
  let state = nsepseq_foldl (eval_let_binding env) state bindings in
  state#set_env (Env.union (fun _ v -> Some v) env state#env)

and eval_let_binding env state (pattern,_,expr) =
  let thread, value = eval_expr (state#set_env env) expr in
  let state = state#set_thread thread in
  fst (filter state Env.empty [pattern] [value])

and filter state pat_env patterns values =
  let rec apply (state, pat_env as acc) pattern value =
    match pattern, value with
      Pvar var, _ ->
        if Env.mem var pat_env then raise (Nonlinear_pattern (state, var));
        if Env.mem var state#env then raise (Multiple_decl (state, var));
        state#set_env (Env.add var value state#env),
        Env.add var value pat_env
    | Pwild _, _ ->
        acc
    | Ppar {value=_,pattern,_; _}, _ ->
        apply acc pattern value
    | Punit _, Value.Unit _ -> acc
    | Pint {value=m; _}, Value.Int {value=n; _} when Z.equal m n -> acc
    | Ptrue _, Value.Bool {value=true; _} -> acc
    | Pfalse _, Value.Bool {value=false; _} -> acc
    | Pstr s1, Value.Str s2 when Region.(s1.value = s2.value) -> acc
    | Ptuple {value=patterns; _}, Value.Tuple {value; _} ->
        filter state pat_env (nsepseq_to_list patterns) value
    | Plist {value=_,patterns,_; _}, Value.List {value; _} ->
        filter state pat_env (sepseq_to_list patterns) value
    | Pcons {value=head,_,tail; _}, Value.List {value=head_val::tail_val; _} ->
        filter state pat_env [head;tail]
                     [head_val; Value.List (Region.wrap_ghost tail_val)]
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

  let env   = state#env in
  let state = state#set_env Env.empty in

  let state, ghost_closures =
    let apply (state, closures)
        (var, _, {region; value=_,param,_,body}) =
      if Env.mem var state#env then raise (Multiple_decl (state, var));
      let ghost_clos = ref None in
      let value = Value.RecClos ghost_clos in
      let   env = Env.add var value state#env in
      state#set_env env,
      {region; value=param,body,ghost_clos} :: closures
    in nsepseq_foldl apply (state,[]) bindings in

  let env   = Env.union (fun _ v -> Some v) env state#env in
  let state = state#set_env env in

  (* Phase 2

     Back-patching recursive closures with the new environment

     All the bindings in the recursive definitions have been
     collected, the previous references are patched with the actual
     closures and the extended environment: this ties the loops
     through the environment.
  *)

  let () =
    let patch_back {region; value=param, body, ghost_clos} =
      ghost_clos := Some {region; value=Value.{param; body; env}}
    in List.iter patch_back ghost_closures

  in state

(* Evaluation of expressions *)

and eval_expr state = function
  LetExpr {value; _} -> eval_let_expr state value
| Tuple components -> eval_tuple state components
| Match {value; _} -> eval_match state value
| If {value; _} -> eval_cond state value
| Fun {region; value=_,x,_,e} ->
    let value = Value.{param=x; body=e; env=state#env} in
    state#thread, Value.Clos {region; value}
| Cat {region; value=arg1,_,arg2} ->
    let thread, v1 = eval_expr state arg1 in
    let thread, v2 = eval_expr (state#set_thread thread) arg2 in
    (match v1, v2 with
       Value.Str s1, Value.Str s2 ->
         thread, Value.Str {region; value=s1.value ^ s2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Cons {region; value=head,_,tail} ->
    let thread, v2 = eval_expr state tail in
    let thread, v1 = eval_expr (state#set_thread thread) head in
    (match v2 with
      Value.List l2 ->
        thread, Value.List {region; value=v1::l2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Or {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let open Value in
    (match v1, v2 with
      Bool b1, Bool b2 ->
        thread, Bool {region; value = b1.value || b2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| And {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let open Value in
    (match v1, v2 with
      Bool b1, Bool b2 ->
        thread, Bool {region; value = b1.value && b2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
|  Lt {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let value =
      match v1, v2 with
        Value.Int z1, Value.Int z2 -> Z.lt z1.value z2.value
      | _ -> raise (Type_error (state#set_thread thread, __LOC__)) in
    thread, Value.Bool {region; value}
| LEq {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let value =
      match v1, v2 with
        Value.Int z1, Value.Int z2 -> Z.leq z1.value z2.value
      | _ -> raise (Type_error (state#set_thread thread, __LOC__))
    in thread, Value.Bool {region; value}
| Gt {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let value =
      match v1, v2 with
        Value.Int z1, Value.Int z2 -> Z.gt z1.value z2.value
      | _ -> raise (Type_error (state#set_thread thread, __LOC__))
    in thread, Value.Bool {region; value}
| GEq {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    let value =
      match v1, v2 with
        Value.Int z1, Value.Int z2 -> Z.geq z1.value z2.value
      | _ -> raise (Type_error (state#set_thread thread, __LOC__))
    in thread, Value.Bool {region; value}
| NEq {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2
    in thread, Value.(Bool {region; value = not (eq thread v1 v2)})
| Eq {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2
    in thread, Value.(Bool {region; value = eq thread v1 v2})
| Add {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    thread,
    (match v1, v2 with
       Value.Int z1, Value.Int z2 ->
         Value.Int {region; value = Z.(+) z1.value z2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Sub {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    thread,
    (match v1, v2 with
       Value.Int z1, Value.Int z2 ->
         Value.Int {region; value = Z.(-) z1.value z2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Mult {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    thread,
    (match v1, v2 with
      Value.Int z1, Value.Int z2 ->
        Value.Int {region; value = Z.( * ) z1.value z2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Div {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    thread,
    (match v1, v2 with
      Value.Int z1, Value.Int z2 ->
        if Z.equal z2.value Z.zero then
          raise (Div_by_zero (state#set_thread thread,
                              region_of_expr e2))
        else Value.Int {region; value = Z.(/) z1.value z2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Mod {region; value=e1,_,e2} ->
    let thread, v1 = eval_expr state e1 in
    let thread, v2 = eval_expr (state#set_thread thread) e2 in
    thread,
    (match v1, v2 with
      Value.Int z1, Value.Int z2 ->
        Value.Int {region; value = Z.(mod) z1.value z2.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Not {region; value=_,e} ->
    let thread, v = eval_expr state e in
    (match v with
      Value.Bool b -> thread, Value.Bool {region; value = not b.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))
| Neg {region; value=_,e} ->
    let thread, v = eval_expr state e in
    (match v with
      Value.Int z -> thread, Value.Int {region; value = Z.neg z.value}
    | _ -> raise (Type_error (state#set_thread thread, __LOC__)))

| Call e -> eval_call_expr state e
| Int z -> state#thread, Value.Int z
| Str s -> state#thread, Value.Str s
| Unit {region; _} -> state#thread, Value.Unit region
| True region -> state#thread, Value.Bool {region; value=true}
| False region -> state#thread, Value.Bool {region; value=false}
| Var v -> state#thread,
          Value.set_region v.region (Env.find v state#env)
| Par {value=_,e,_; _} -> eval_expr state e
| List l -> eval_expr_list state l
| Extern e -> eval_external state e

and eval_cond state = function
  IfThenElse (_, cond,_, if_true, _, otherwise) ->
    let thread, value = eval_expr state cond in
    let eval = eval_expr (state#set_thread thread) in
    (match value with
       Value.Bool {value=true;_}  -> eval if_true
     | Value.Bool {value=false;_} -> eval otherwise
     | _ -> raise (Type_error (state, __LOC__)))
| IfThen (_, cond,_, if_true) ->
    let thread, value = eval_expr state cond in
    let eval = eval_expr (state#set_thread thread) in
    (match value with
       Value.Bool {value=true; _}    -> eval if_true
     | Value.Bool {region; value=false} ->
         state#thread, Value.Unit region
     | _ -> raise (Type_error (state, __LOC__)))

and eval_match state (_, expr, _, cases) =
  let env = state#env in
  let thread, value = eval_expr state expr in
  let state = State.make ~thread ~env:Env.empty
  in eval_cases env state value cases

and eval_cases env state value (case, cases) =
  let pattern, _, expr = case in
  try
    let state = fst (filter state Env.empty [pattern] [value]) in
    let env = Env.union (fun _ v -> Some v) env state#env
    in eval_expr (state#set_env env) expr
  with Type_error _ ->
    match cases with
              [] -> raise (Incomplete_match state)
    | (_,hd)::tl -> eval_cases env state value (hd,tl)

and eval_tuple state components =
  let apply expr (thread,values) =
    let thread, value = eval_expr (state#set_thread thread) expr
    in thread, value::values in
  let thread, value =
    nsepseq_foldr apply components.value (state#thread,[])
  in thread, Value.Tuple {components with value}

and eval_expr_list state {region; value=_,exprs,_} =
  let apply expr (thread,values) =
    let thread, value = eval_expr (state#set_thread thread) expr
    in thread, value::values in
  let thread, value =
    sepseq_foldr apply exprs (state#thread,[])
  in thread, Value.List {region; value}

and eval_let_expr state = function
  LetIn (_,bindings,_,expr) ->
    eval_expr (eval_let_bindings state bindings) expr
| LetRecIn (_,_,bindings,_,expr) ->
    eval_expr (eval_let_rec_bindings state bindings) expr

(* Note: We evaluate the arguments leftwards, as observed with the
   OCaml compiler (undocumented). *)

and eval_call_expr state {region; value=func,arg} =
  let thread, arg_val = eval_expr state arg in
  let thread, fun_val = eval_expr (state#set_thread thread) func in
  let open Value in
  match fun_val with
    Value.Clos {value={param; body; env}; _}
  | RecClos {contents = Some {value={param; body; env}; _}} ->
    let state =
      State.make ~thread ~env:(Env.add param arg_val env) in
    let thread, v = eval_expr state body in
    thread, Value.set_region region v
  | _ -> raise (Type_error (state#set_thread thread, __LOC__))

and eval_external state = function
    Cast e -> eval_cast_expr   state e
|  Print e -> eval_print_expr  state e
|  Scanf e -> eval_scanf_expr  state e
| PolyEq e -> eval_polyeq_expr state e

and eval_polyeq_expr state (x,y) =
  let thread = state#thread in
  let x = Env.find_id x state#env
  and y = Env.find_id y state#env in
  thread, Value.(Bool (wrap_ghost (eq thread x y)))

and eval_print_expr state = function
  PrintString var ->
   (match Env.find_id var state#env with
      Value.Str {region; value} ->
        print_string (Scanf.unescaped value); flush stdout;
        (state#thread, Value.Unit region)
    | _ -> raise (Type_error (state, __LOC__)))

| PrintInt var ->
   (match Env.find_id var state#env with
      Value.Int {region; value} ->
        print_string (Z.to_string value); flush stdout;
        (state#thread, Value.Unit region)
    | _ -> raise (Type_error (state, __LOC__)))

and eval_scanf_expr state = function
  ScanfInt var ->
   (match Env.find_id var state#env with
      Value.Unit region ->
        let z = Z.of_int Scanf.(bscanf Scanning.stdin "%d\n" (fun x -> x))
        in state#thread, Value.Int {region; value=z}
    | _ -> raise (Type_error (state, __LOC__)))

| ScanfString var ->
   (match Env.find_id var state#env with
      Value.Unit region ->
        let value = Scanf.(bscanf Scanning.stdin "%s\n" (fun x -> x))
        in state#thread, Value.Str {region; value}
    | _ -> raise (Type_error (state, __LOC__)))

| ScanfBool var ->
   (match Env.find_id var state#env with
      Value.Unit region ->
        let value = Scanf.(bscanf Scanning.stdin "%B\n" (fun x -> x))
        in state#thread, Value.Bool {region; value}
    | _ -> raise (Type_error (state, __LOC__)))

and eval_cast_expr state = function
  StringOfInt var ->
    state#thread,
    (match Env.find_id var state#env with
      Value.Int z -> Value.Str {z with value = Z.to_string z.value}
     | _ -> raise (Type_error (state, __LOC__)))

| StringOfBool var ->
   state#thread,
   (match Env.find_id var state#env with
      Value.Bool b -> Value.Str {b with value = string_of_bool b.value}
    | _ -> raise (Type_error (state, __LOC__)))

(* Initialising the interpreter's state *)

let add_casts state =
  let param          = "x"
  and extern cast    = Extern (Cast cast) in
  let string_of_int  = extern (StringOfInt param)
  and string_of_bool = extern (StringOfBool param) in
  let mk_clos body =
    let clos = Value.{param = wrap_ghost param; body; env=Env.empty}
    in Value.Clos (wrap_ghost clos) in
  let env = state#env
            |> Env.add_id "string_of_int"  (mk_clos string_of_int)
            |> Env.add_id "string_of_bool" (mk_clos string_of_bool)
  in state#set_env env

let add_prints state =
  let param        = "x"
  and extern print = Extern (Print print) in
  let print_string = extern (PrintString param)
  and print_int    = extern (PrintInt param) in
  let mk_clos body =
    let clos = Value.{param = wrap_ghost param; body; env=Env.empty}
    in Value.Clos (wrap_ghost clos) in
  let env = state#env
          |> Env.add_id "print_string" (mk_clos print_string)
          |> Env.add_id "print_int"    (mk_clos print_int)
  in state#set_env env

let add_scanf state =
  let param        = "x"
  and extern scanf = Extern (Scanf scanf) in
  let scanf_int    = extern (ScanfInt param)
  and scanf_string = extern (ScanfString param)
  and scanf_bool   = extern (ScanfBool param) in
  let mk_clos body =
    let clos = Value.{param = wrap_ghost param; body; env=Env.empty}
    in Value.Clos (wrap_ghost clos) in
  let env = state#env
            |> Env.add_id "scanf_int"    (mk_clos scanf_int)
            |> Env.add_id "scanf_string" (mk_clos scanf_string)
            |> Env.add_id "scanf_bool"   (mk_clos scanf_bool)
  in state#set_env env

let add_equal state =
  let ghost_fun = ghost and ghost_arrow = ghost in
  let equal =
    Fun (wrap_ghost (ghost_fun, (wrap_ghost "y"), ghost_arrow,
                     Extern (PolyEq ("x","y")))) in
  let closure =
    Value.{param = wrap_ghost "x"; body=equal; env=Env.empty} in
  let closure = Value.Clos (wrap_ghost closure)
  in state#set_env (Env.add_id "equal" closure state#env)

let add_prelude = add_casts <@ add_prints <@ add_scanf <@ add_equal


(* Main entry

   The main function exported by this module is [eval]. First, the
   initial state is built. Note that we do not have a use here for the
   thread, but we keep for future developments. If no evaluation
   environment is provided, the prelude is added. Then the statements
   in the AST are evaluated in sequence.  *)

let eval =
  let state = State.make ~env:Env.empty ~thread:(object end) in
  fun ?env ast ->
    let state =
      match env with
            None -> add_prelude state
      | Some env -> state#set_env env
    in eval_statements state ast
