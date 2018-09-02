(*
Design of the STICK language and the compiler

1 Background, motivation and technical choices

Most, if not all, popular solutions for programming neural networks
are libraries written in sundry programming languages, like Python,
which allow programmers to blend their networks with their own
specialised libraries for numerical computations, learning, data
visualisation etc. The APIs to these libraries often enable the
separate definition of neurons and their connections (called here
_synapses_, although _axons_ would be more accurate, but we follow the
terminology of the STICK paper).

Our approach is to propose a Domain-Specific Language (DSL) for
programming STICK networks.

The benefit is a tailored syntax and domain-specific error messages,
contrary to the library or embedded DSL approach. Of course, it is
then harder to entice customers to learn a new programming
language. As a way to ease the transition, we propose to mimic the
syntax and semantics of an existing, well-known programming language,
namely OCaml. In fact, the STICK language is an extension of a tiny
subset of OCaml, and, since most popular text editors feature editing
modes for OCaml, we already have (non-interactive) modes for our STICK
language. Moreover, there is an active community of researchers and
developers contributing the OCaml system, and great tutorials and
books are available. There is an active OCaml Consortium and an
international conference on industrial uses of OCaml, where
contributions of open source libraries are discussed as well.

OCaml is a functional language, that is, a programming language where
functions are the basic building blocks: they can be passed as
arguments to other functions and they can be returned by functions. As
far as the design of the STICK language is concerned, the subset of
OCaml we are interested in is _purely_ functional, that is, it only
features constructs that do not explicitly refer to the memory address
of values, therefore disallowing side-effects entirely. As a
consequence, all variables denote constant values and values cannot be
modified: only a new version of a value can be made, typically by
recursive function calls. An interesting consequence is that a purely
functional language de facto implies the _persistence_ of the
processed data (therefore versioning and local editing), hence its
safe _sharing_ between execution threads. For instance, in terms of
APIs, we obtain the "Undo" functionality free of charge. Purely
functional languages ease the definition of iterators (on stacks,
queues, trees etc.) and free the programmers of checking that they are
not accessing an array element out of bounds, because there are no
(writable) arrays and no loops: control is achieved exclusively by
means of function calls. A garbage collector is expected in the
runtime, liberating the programmer from freeing purportedly unused
memory chunks.

Note that OCaml itself is not purely functional, contrary to a
language like Haskell, with the (perhaps obvious) caveat that some
mechanism is always available to output the result of running a
programming to a terminal, a hard disk, a network etc. ---all typical
side-effects. In the case of our STICK language, the only side-effect
is the result of the evaluation, to wit, a STICK network, being output
to the terminal.


2. A language and compiler for flat networks

The first step towards designing a language to program STICK networks
was to have a tiny DSL, not Turing-complete, but allowing users to
define flat networks. Turing-completeness is the property of a
programming language that can express arbitrary functions, in
particular, functions whose termination is undecidable. Flatness means
that we do not care at first to use the features of the OCaml STICK
library that process hierarchical networks (networks of networks)
---see the report for February 2017.

A rough grammar for the language was made for a tiny subset of
OCaml. From the point of view of the semantics, the language is
_purely functional_, except for one added construct that creates one
neuron and its incoming synapses. The evaluation of that construct
yields a side-effect on the implicit, current network under
construction. Overall, the meaning of a STICK program is the STICK
network.

In terms of design, this choice differs in two ways from the library
approach described above. First, with the libraries, the network is a
value of the host language, whereas, with the STICK language, it is
not a value, but a side-effect of the interpretation. Second, with the
libraries, the construction of neurons and synapses are separate: in
order to add a synapse, the source and destination neurons must have
been created before. This brings a lot of flexibility in the order
with which the network is built, but, most importantly, it completely
avoids the issue of cycles in the networks, the simplest case being a
neuron with a loop synapse (that is, source and destination are one
and the same). On the other hand, some inconvenience may arise from
the great freedom in the order with which the network is built, as
well as the two syntactically separate operations to grow the network,
which can be spread out in a long source file. With the STICK
language, as noted above, we only have one atomic construct: the
creation of a neuron and its incoming synapses. Neurons are values of
the language, but synapses are not (since they cannot be added to
existing neurons after they have been created). This syntax forces the
locality of neuron and synapse creation, in contrast with the library
approach, where scattering may be an issue, but is also brings to the
fore a thorny design point: recursion. Indeed, to create, for example,
a single neuron with a loop in a single construct we would have to
reference the source neuron _while we are defining it_. More
generally, a cycle in the network corresponds to a group of mutually
recursive definitions of neuron and their incoming synapses. This
departs from how recursive definitions are handled in OCaml, where
(mostly) only functions are allowed to be mutually recursive, whereas
we want to allow neuron construction to be as well.

The first step of implementation was to write an interpreter for the
tiny subset of OCaml, we call MiniML. This way, the issues which are
independent of the STICK networks were identified and sorted out, the
boilerplate code for debugging was made, simple programs could be
tested against the behaviour of the OCaml compiler itself.

The second step was to add the single construct that adds a neuron and
its incoming synapses to the network, and the special support it
requires for recursion.

In order to enable debugging the compiler on reasonably large
examples, we had to rework the parser and the abstract syntax tree
(AST) so source locations are made available to the interpreter in
case of an error. It is actually quite difficult to accurately
pinpoint errors.


3. A conservative language extension for hierarchical networks

Hierarchy is of great help in understanding and debugging networks,
enabling abstraction (a given network can, on demand, be seen as a
black box) and composability (to wit, how two networks can be
connected, irrespectively of their actual contents). The OCaml library
designed earlier already supports network hierarchies, and it was
tested on examples taken from the seminal STICK paper, so we were
confident that it worked.

The shortest path to integrate our library into our compiler's backend
was to keep the syntax invariant and interpret differently function
calls: now, a function call will instantiate a sub-network, whose name
is that of the function, plus a globally unique index. For instance,
three calls to the function "memory" would create three sub-networks
"memory_0", "memory_1" and "memory_2". The hierarchy naturally stems
out of the control flow: if a function "memory" is called inside a
function "synchroniser", a sub-network "memory_0" is created below the
network "synchroniser_0" (assuming both functions were called for the
first time). The downside is that functions that do not add to the
network, like algorithmic, arithmetic or logic functions, yield empty
networks. These are not a problem for the consumers of the compiler
outputs, like the software simulator, because they only extract a flat
network anyway, and empty ones simply disappear.

For example, here is how we define a STICK network that stores the
constant 5:

(* The value of "we" is a weight and "t_syn" is a delay. *)

let cst = 5

let constant recall =
  node "output"
    V we         t_syn <- recall
    V we (t_syn + cst) <- recall

let recall = input "recall"
let stop = constant recall

Having functions and neurons provides a natural way to hide neurons:
those not returned by a function are lexically scoped to the body of
the function. For instance, the program defining the memory found in
the STICK paper is:

(* Note the hidden neurons "first", "last", "acc" and "acc2". *)

let wi = -we

let memory to_input to_recall =
  let in_ = node "input"
    V we 0 <- to_input in
  let recall = node "recall"
    V we 0 <- to_recall in
  let rec first = node "first"
    V we t_syn <- in_
    V wi t_syn <- first in
  let last = node "last"
    V (we/.2.0) t_syn <- in_ in
  let acc = node "acc"
    Ge wa t_syn <- first in
  let acc2 = node "acc2"
    Ge (-.wa) t_syn <- acc
    Ge wa t_syn <- recall
    Ge wa t_syn <- last in
  let out = node "output"
    V we t_syn <- acc2
    V we t_syn <- recall in
  let ready  = node "ready"
    V we t_syn <- acc
  in out, ready

let to_input = input "to_input"
let to_recall = input "to_recall"
let out, ready = memory to_input to_recall

In order to bring hierarchies to the STICK language, we first hooked
the flat network version into a zipper (the data structure of our
OCaml library that enables to randomly traverse a tree of networks
---see report of February 2017), that is, we constructed flat networks
at the root of a tree. The second step was to grow the tree of network
according to the function calls.

With hierarchy come the concepts of input and output neurons. These
concepts are not the same as those eponyms with respect to the
simulator (or "system interface"): here, a neuron is an input neuron
if, and only if, it is targetted by a synapse coming from a neuron in
a different network; similarly, a neuron is an output neuron if, and
only if, it is the source of a synapse to a neuron outside the
network; otherwise, a neuron is said to be hidden. These definitions
are consistent with the OCaml library we designed. We had to infer
these visibilities (input, output or hidden), because we did not want
to change the concrete syntax, and there was no rational to bring
these concepts explicitly to the toolbox of the programmer.

Another concept stemming our of hierarchy is that of network
address. A network is characterised by a unique address made of the
names of the networks making up the path from the root of the network
tree to the current node. Consequently, a neuron is now uniquely
qualified by the address of the network it belongs to, and its unique
identifier inside his network (as with flat networks). This is
actually the view offered by the zippers.

With hierarchy, a design requirement became clearer: the need for
mutually recursive definitions of neurons that are used in function
calls on the right-hand sides. Interpreting functions calls as network
creations, this means that we want to set a feedback loop. For
instance, consider

let synchronizer input_0 input_1 input_2 =
  let sync_weight = we/3 in
  let rec sync =
    node "sync"
      V sync_weight t_syn <- ready_0
      V sync_weight t_syn <- ready_1
      V sync_weight t_syn <- ready_2
  and output_0, ready_0 = memory input_0 sync
  and output_1, ready_1 = memory input_1 sync
  and output_2, ready_2 = memory input_2 sync
  in output_0, output_1, output_2

This function defines a synchronizer for three inputs. The neuron
"sync" has two synapses coming from neurons "ready_0", "ready_1" and
"ready_2", which belong to sub-networks "memory_0", "memory_1" and
"memory_2", creating synapses from "sync".

Of import are the calls to "memory": they lie on the right-hand side
of mutually recursive definitions (let rec), and this opens a whole
host of semantic difficulties.

Languages like OCaml forbid this kind of definitions because it opens
the way to unsound interactions between polymophism and imperative
effects, and because it blurs the control flow, that is, what and when
is a value computed. The case of mutually recursive functions is
relatively simple, though: each function evaluates in a pair made of
its AST and the environment at the current point (that is, the
identifiers and the values they are bound to), so the order of
evaluation is irrelevant.

Languages like Haskell allow a more general form of recursion (beyond
functions) because they are lazy, that is, only values that are needed
to build other necessary values are actually computed, in particular,
arguments to a function are not necessarily computed during a call.

One way to handle mutual recursion involving function calls (as in
function "synchronizer" above) would be to change the semantics of the
STICK language to become lazy, but this is a drastic departure from
OCaml, and, moreover, lazy languages yield programs that are hard to
debug because it is hard to know when an expression is evaluated
(depending on it being needed by another needed value).

Therefore we decided to keep the strict semantics of OCaml (as opposed
to the lazy semantics of Haskell) by using a trick: identifiers used
in mutual recursive definitions will be assumed to be neurons, and
only neurons (so no "let rec x = x + 1"), and a first pass on the
left-hand sides creates a neuron for each identifier involved in
recursion. Then, right-hand sides are evaluated, without a particular
order. The neurons created by the right-hand sides are finally fused
with the corresponding ones on the left-hand sides (by pattern
matching), creating feedback loops. We had then to extend the OCaml
library with a function fusing two neurons, possibly across network
boundaries, and use it when evaluating mutually recursive definitions.

Some technical difficulties brought by the fusion of neuron are: it is
an asymmetric operation, as the source neuron must not have incoming
synapses (except loops); the lexical scoping of a node may not
correspond to its network hierarchy, as a node may be fused with
another located in a sub-network; network growth is no longer
monotonic, as the number of nodes may decrease due to fusion, so a
remapping of neuron identifiers is required so the output net list
still contains contiguous identifiers; deleted nodes must remain
accessible through the name of the source neuron that was fused, so a
notion of neuron aliasing (and associated name resolution) is needed.

We also had to revisit the invariants we wanted to be enforced on the
synapses, to allow more kinds of network topologies, like a synapse
connecting a neuron to another which lies in two networks below or in
another branch of the network tree.

Additionally, the grammar was rid of LR(1) conflicts, so the parser
generated by Menhir became correct and complete in respect.

In order to create arbitrarily large networks, Slang features the
typical data structure of functional languages: lists. Together with
higher-order functions and polymorphism, any kind of iteration on
lists and combinations become available. For instance, here is the
program for building a synchonizer of [size] values (the function
[memory] is omitted):

let size = ...

let wi = -we

let memory to_recall to_input = net "memory" ...

let rec map f l =
  if equal l [] then [] else let head::tail = l in f head :: map f tail

let rec split l =
  if equal l [] then [],[]
  else let (head1, head2)::tail = l in
         let tail1, tail2 = split tail
         in head1::tail1, head2::tail2

let rec foldl f a l =
  if equal l [] then a else let head::tail = l in foldl f (f a head) tail

let len = foldl (fun n _ -> n + 1) 0

let rec mk_list n e = if n = 0 then [] else e :: mk_list (n-1) e

let synchronizer inputs =
  let n = len inputs in
  let weights = mk_list n (we/n)
  and delays = mk_list n t_syn
  and sync = node "sync" in
  let outputs, readys = split (map (memory sync) inputs) in
  let () = fuse (sync, node "sync_out" V weights delays <= readys)
  in outputs

let inputs =
  let rec mk_inputs acc i =
    if i < 0 then acc
    else mk_inputs (input ("input_" ^ string_of_ratio i) :: acc) (i-1)
  in mk_inputs [] (size-1)

let name = "synchroniser"

let _, outputs =
  let apply (i,out) mem_out =
    let neu = output ("output_" ^ string_of_ratio i) V we t_syn <- mem_out
    in i+1, neu::out
  in foldl apply (0,[]) (net name synchronizer inputs)


  4. Implementation of the interpreter for Slang

  Slang is a tiny, untyped, purely functional subset of OCaml,
extended with two constructs to create, by means of side-effects, a
STICK network. In particular, this means that the language semantics
is strict (eager evaluation), even though lazy functional languages
are usually used to better handle non-inductive data types like
graphs: we wanted to avoid lazy evaluation in order to provide a more
familiar mental model to the programmer, especially given the
difficulty to debug neural networks.

  A strict semantics means that the arguments to a function call are
always evaluated before the function is called, therefore, when
inspecting the source code of a function, the programmer can assume
that the arguments have been evaluated. This is useful to debug
programs and is important in the presence of side-effects: since those
depend on the order of evaluation, it is critical to understand well
when expressions are computed along the control flow. The reason for
building the network as a side-effect is that graphs are not, in
general, inductive data structures, because of the presence of
cycles. On the other hand, a lazy semantics means that expressions are
evaluated only when and if their values are needed. In that framework,
graphs can be (potentially infinite) values of the language, and they
do no need to be the result of side-effects.

  Here, we define the interpreter as an iterator over the Abstract
Syntax Tree (AST). At a given node in the AST, it computes a value for
each subtree ("on the way down") and combines them ("on the way up")
to make the value of the current node .

  The main function exported in the present module is [eval]. The call
[eval ~top ~conf ast] takes [top] to be the name of the top-level
network, [conf] is the name of the configuration file, and [ast] is
the AST, and the call evaluates into a _state_.

  A state of the interpreter is a pair made of a _value environment_
and a _thread_. The environment is a map from variables to values and
these variables define the current scope at any point in the program
(equivalently, the scope could be defined as the region of the AST
containing all the bindings available at a given node); the thread is
the information that flows through all the constructs of a program,
along the control flow. Contrary to the thread, the value environment
only flows downwards in the AST (that is, only those declarations, or
bindings, "up in the tree" are visible at any time). The thread is
used to model the only side-effect of Slang, namely, a tree of
networks handled by a zipper (see module [Network.Zipper]) ---As for
the analogy: when sewing a piece of clothing, a thread is plucked
before being threaded down.

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

  Of the type [Value.t], the data constructors [Closure], [RecClos]
and [Neuron] require some attention.

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

  The [Neuron] data constructor models the value of a neuron and
contains a neuron as defined by the [Network] module, and a digest
from the AST denoting the neuron. The latter holds some information
useful for error reporting, like the neuron name and its number of
incoming synapses.

  The module [Env] models the environments. Basically, it is a map
from strings to values, and it reexports some the standard values from
the [Map] module, specialised to the strings and values.

  Finally, the module [State] defines the type [t] for states as a
record containing an environment and a thread. The thread is only made
of a zipper, as creating neurons and networks are the only side
effects and the zipper interprets them.
*)

open Utils
open Network
open AST

(* Value environments *)

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
end

(* Semantic values *)

and Value: sig
  type closure = {param: rvar; body: expr; env: Env.t}

  type t =
    Rational of Q.t reg
  | Integer  of Z.t reg
  | Str      of string reg              (* Strings *)
  | Bool     of bool reg
  | Unit     of Region.t
  | Tuple    of t list reg
  | List     of t list reg
  | Closure  of closure reg
  | RecClos  of closure reg option ref  (* Recursive closures *)
  | Neuron   of Neuron.t reg

  val eq: State.thread -> t -> t -> bool

  val to_string: t -> string
  val to_region: t -> Region.t

  val set_region: Region.t -> t -> t   (* Patching are region *)
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
end

(* Errors *)

(* Errors are signalled by means of exceptions. These are divided here
   in two groups: the ones that cannot be raised if the Slang program
   has been typechecked by the OCaml compiler, and the ones that can.
   This distinction follows the seam between static and dynamic
   semantics, but also mirrors the contrast between OCaml and
   STICK-specific errors.

     In the following exceptions, the state [state] is meant to be
   used for dumping the network up to the error (if it is in a
   coherent state), and accessing other relevant informations, like
   the environment at the point of error.
*)

(* The exceptional value [Type_Error (state, message)] means that a
   type error was encountered, and the interpreter state is [state]
   and the message is [message]. Note that if the Slang program passed
   the type inference of OCaml, this exception will not be raised. *)

exception Type_error of State.t * string

(* Just like in OCaml non-linear patterns are forbidden, that is, a
   variable can occur only once in a given pattern. The exceptional
   value [Nonlinear_pattern (state, rvar)] refers to the second
   occurence of the variable, [rvar]. Note that if the Slang program
   has passed the type inference of OCaml, this exception is not
   raised. *)

exception Nonlinear_pattern of State.t * rvar

(* The exceptional value [Multiple_decl (state, rvar)] denotes an
   invalid redeclaration of variable [rvar] (with its source
   region). This can only happen in a parallel let-expression ("let
   [rec] ... and ..."), as, otherwise, a redeclaration would simply
   shadow the previous one. Note that if the Slang program has passed
   the type inference of OCaml, this exception is not raised. *)

exception Multiple_decl of State.t * rvar

(* STICK-specific errors (dynamic errors for the Slang semantics) *)

(* TO DO *)

exception Div_by_zero of State.t * Region.t

(* The exceptional value [Nameless_input (state, reg)] signals that an
   input neuron (distinguished by the "input" keyword) is being
   associated with an empty name (string), which is invalid. *)

exception Nameless_input of State.t * Region.t

(* Dual of [Nameless_input] for output neurons. *)

exception Nameless_output of State.t * Region.t

(* The exceptional value [Homograph (state,reg,neu,addr)] denotes the
   situation where the neuron [neu] (with its source region) bears the
   same name as another in the network at the address [addr]. The
   region [reg] refers (if not a ghost) to the homograph neuron. *)

exception Homograph of State.t * Region.t * Neuron.name reg * Net.Addr.t

(* The exceptional value [Multiple_syn (state,src,dst,reg1,reg2)]
   means that there are two identical synapses from the neuron [src]
   to [dst], located at regions [reg1] and [reg2]. *)

exception Multiple_syn of State.t * Neuron.t * Neuron.t * Region.t * Region.t

(* The exceptional value [NotSource (state,neu,deg)] is raised when
   attempting to fuse a neuron [neu] with another and the former is
   not a source, that is, it has incoming synapses ([deg] is their
   number). *)

exception NotSource of State.t * Neuron.t reg * int

(* The exceptional value [TwoStates (state,src,(reg,dst))] refers to
   the invalid attempt at fusing two neurons, [src] and [dst] (whose
   source code region is [reg]), which are in different states. *)

exception TwoStates of State.t * Neuron.t * Neuron.t reg

(* The exceptional value [Duplicate (state,src,dst,(lbl,neu))] denotes
   the invalid attempt at fusing two neurons, [src] and [dst], each of
   which have an identical outgoing synapse, labelled [lbl, to the
   same neuron [neu]. *)

exception Duplicate of State.t * Neuron.t * Neuron.t reg * (Label.t * Neuron.t)

(* The exceptional value [Toplevel_state (state,reg)] is raised if the
   creation of a top-level neuron specifies an explicit state at the
   region [reg]. *)

exception Toplevel_state of State.t * Region.t

(* The exceptional value [NonPositiveDelay (state,d)] is raised when
   attempting to create a synapse with the nonpositive delay [d] (the
   source region is included in [d]). *)

exception NonPositiveDelay of State.t * Delay.t reg

(* The exceptional value [NetCollision (state,name)] is used when
   attempting to create a network named [name], which already has a
   sibling bearing the same name. (Neurons and networks must have
   unique names in within their network.) *)

exception NetCollision of State.t * Net.name reg


(* Evaluation *)

(* The Slang interpreter is called by [eval ~log ~dot ~net ~top ~env
   ast], where [log] is the log channel, [dot] the dot channel, [net]
   the net-list channel, [top] the name of the top-level network,
   [env] the initial environment (see above) and [ast] the Abstract
   Syntax Tree of the Slang program to be evaluated.
*)

val eval: ?log:out_channel -> ?dot:out_channel -> ?net:out_channel
          -> ?stats:out_channel
          -> top:Net.name -> ?env:Env.t -> AST.t -> State.t

(* The function [eval_statements] evaluates an AST in a given state
   into a new state. *)

val eval_statements: State.t -> AST.t -> State.t
