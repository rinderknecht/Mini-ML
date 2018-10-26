(* The interpreter for Mini-ML *)

(* Here, we define the interpreter as an iterator over the Abstract
   Syntax Tree (AST). At a given node in the AST, it computes a value for
   each subtree ("on the way down") and combines them ("on the way up")
   to make the value of the current node .

   The main function exported in the present module is [eval]. The call
   [eval ast] takes [ast] to be the AST, and the call evaluates into a
   _state_.

   A state of the interpreter is a pair made of a _value environment_
   and a _thread_. The environment is a map from variables to values and
   these variables define the current scope at any point in the program
   (equivalently, the scope could be defined as the region of the AST
   containing all the bindings available at a given node); the thread is
   the information that flows through all the constructs of a program,
   along the control flow. Contrary to the thread, the value environment
   only flows downwards in the AST (that is, only those declarations, or
   bindings, "up in the tree" are visible at any time). The thread could
   be used to model side-effects of Mini-ML, but it currently only
   contain the output channel ---As for the analogy: when sewing a piece
   of clothing, a thread is plucked before being threaded down.

   In general, the effect of each statement in the source file impacts
   both the environment (by adding a new binding, perhaps also shadowing
   an existing one) and the thread (by adding a new neuron to the current
   network, creating a new sub-network or fusing two neurons), hence the
   return type of [eval] is [State.t], as indicated above.

   Environments, values (sometimes called _semantic values_, to
   distinguish them from the syntactic values found in the AST) and
   states are implemented by OCaml modules. As mentioned above,
   environments depend on values, but the inverse also holds, due to
   functions capturing the environment at their point of
   declaration. Indeed, variables occuring freely in the function (that
   is, they are neither bound by parameters nor local declarations) have
   to be in the scope at the function call sites. The solution here
   consists in defining two mutually recursive modules, [Env] and
   [Value].

   Of the type [Value.t], the data constructors [Closure] and [RecClos]
   require some attention.

   The two first constructors depend on the type [Value.closure]. A
   _closure_ is the value of a function, and it is made of exactly one
   parameter (Note: in this document, a _parameter_ is a variable,
   whereas an "argument" is a value bound to a parameter at a call
   site.), one function body and the environment at the declaration
   point. The difference between [Closure] and [RecClos] lies in the
   latter holding a reference to an optional closure, and modelling
   _recursive closures_, that is, values of recursive functions. These
   closures must be handled with care because they capture the
   environment above their declarations, but _extended with the bindings
   in the declarations_. The problem with a purely functional data
   structure to represent the environment is that, once it is captured,
   it remains constant, and therefore cannot be extended to include the
   mutually recursive bindings. On the other hand, if we allow a
   reference to the environment in the closures, that reference can be
   changed after the recursive definitions have been processed to include
   the new bindings. It is actually simpler to have a reference to a full
   closure, rather than just the environment, and this is the rationale
   for the type of the data constructor [RecClos]. The technique is
   called _back-patching_ and works in two steps: first, a reference to
   [None] is used instead of a normal closure (it is indeed simpler to
   insert the option type, as there is then no need for a fake closure
   waiting to be patched with the right environment: the value [None]
   will simply stand in its stead); second, when all the bindings in the
   recursive definitions have been collected, the previous references are
   patched with the actual closures and the extended environment: this
   ties the loops through the environment.

   The module [Env] models the environments. Basically, it is a map
   from strings to values, and it reexports some the standard values from
   the [Map] module, specialised to the strings and value.s

   Finally, the module [State] defines the type [t] for states as a
   record containing an environment and a thread. The thread is only made
   of a zipper, as creating neurons and networks are the only side
   effects and the zipper interprets them.
*)

open Utils
open AST

(* Value environments *)

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
end

(* Semantic values *)

and Value: sig
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Int     of Z.t reg
  | Str     of string reg
  | Bool    of bool reg
  | Unit    of Region.t
  | Tuple   of t list reg
  | List    of t list reg
  | Clos    of closure reg
  | RecClos of closure reg option ref  (* Recursive closures *)

  val eq         : State.thread -> t -> t -> bool
  val to_string  : t -> string
  val to_region  : t -> Region.t
  val set_region : Region.t -> t -> t   (* Patching are region *)
end

(* State *)

and State : sig
  type thread = {
    out: out_channel option
  }
  type t = {env: Env.t; thread: thread}
  type state = t
end

(* Errors *)

(* The exceptional value [Type_Error (state, message)] means that a
   type error was encountered, and the interpreter state is [state]
   and the message is [message]. Note that if the Mini-ML program
   passed the type inference of OCaml, this exception will not be
   raised. *)

exception Type_error of State.t * string

(* Just like in OCaml non-linear patterns are forbidden, that is, a
   variable can occur only once in a given pattern. The exceptional
   value [Nonlinear_pattern (state, rvar)] refers to the second
   occurence of the variable, [rvar]. Note that if the Mini-ML program
   has passed the type inference of OCaml, this exception is not
   raised. *)

exception Nonlinear_pattern of State.t * rvar

(* The exceptional value [Multiple_decl (state, rvar)] denotes an
   invalid redeclaration of variable [rvar] (with its source
   region). This can only happen in a parallel let-expression ("let
   [rec] ... and ..."), as, otherwise, a redeclaration would simply
   shadow the previous one. Note that if the Mini-ML program has
   passed the type inference of OCaml, this exception is not
   raised. *)

exception Multiple_decl of State.t * rvar

(* Division by zero *)

exception Div_by_zero of State.t * Region.t

(* Evaluation *)

(* The Mini-ML interpreter is called by [eval ~out ~env ast], [out]
   the output channel, [env] the initial environment (see above) and
   [ast] the Abstract Syntax Tree of the Mini-ML program to be
   evaluated. *)

val eval : ?out:out_channel -> ?env:Env.t -> AST.t -> State.t

(* The function [eval_statements] evaluates an AST in a given state
   into a new state. *)

val eval_statements : State.t -> AST.t -> State.t
