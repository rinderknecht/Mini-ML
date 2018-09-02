(* Abstract Syntax Tree (AST) for Slang, the STICK language *)

(* The AST is stratified in such a way that it is (almost) isomorphic
   to the grammar (see module [Parser]. This enables to write
   straightforward semantic actions in the parser, at the cost of
   deeper abstract trees being built (for example, arithmetic
   operators are binary). In order to ease the matching on those
   trees, some specific functions are provided here. *)

open Utils

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

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

type one_syn  = Region.t                                              (* "<-" *)
type many_syn = Region.t                                              (* "<=" *)
type arrow    = Region.t                                              (* "->" *)
type cons     = Region.t                                              (* "::" *)
type cat      = Region.t                                              (* "^"  *)

(* Arithmetic operators *)

type minus  = Region.t                                                 (* "-" *)
type plus   = Region.t                                                 (* "+" *)
type div    = Region.t                                                 (* "/" *)
type mult   = Region.t                                                 (* "*" *)

type minusQ  = Region.t                                               (* "-." *)
type plusQ   = Region.t                                               (* "+." *)
type divQ    = Region.t                                               (* "/." *)
type multQ   = Region.t                                               (* "*." *)

(* Boolean operators *)

type bool_or  = Region.t                                              (* "||" *)
type bool_and = Region.t                                              (* "&&" *)

(* Comparisons *)

type eq = Region.t                                                    (* "="  *)
type ne = Region.t                                                    (* "<>" *)
type lt = Region.t                                                    (* "<"  *)
type gt = Region.t                                                    (* ">"  *)
type le = Region.t                                                    (* "=<" *)
type ge = Region.t                                                    (* ">=" *)

(* Compounds *)

type lpar = Region.t                                                   (* "(" *)
type rpar = Region.t                                                   (* ")" *)
type lbra = Region.t                                                   (* "[" *)
type rbra = Region.t                                                   (* "]" *)

(* Separators *)

type comma = Region.t                                                  (* "," *)
type semi  = Region.t                                                  (* ";" *)

(* Wildcard *)

type wild = Region.t                                                   (* "_" *)

(* Literals *)

type var = string
type label = string                                            (* "~<label>:" *)

(* Comma-separated non-empty lists *)

type 'a csv = ('a, comma) nsepseq

(* Semicolon-separated lists *)

type 'a ssv = ('a, semi) sepseq

(* Parentheses *)

type 'a par = lpar * 'a * rpar

type unit__ = lpar * rpar

(* Brackets compounds *)

type 'a bra = lbra * 'a * rbra


(* The Abstract Syntax Tree (finally) *)

type t  = statement list * eof
and ast = t

and eof = Region.t

and statement =
                                       (* let p1 = e1 and p2 = e2 and ...     *)
  Let     of (kwd_let * let_bindings) reg
                                       (* let rec p1 = e1 and p2 = e2 and ... *)
| LetRec  of (kwd_let * kwd_rec * let_rec_bindings) reg
| Include of Token.file                                     (* #include "foo" *)

and let_bindings = (let_binding, kwd_and) nsepseq  (* p1 = e1 and p2 = e2 ... *)

and let_binding = pattern * eq * expr                                (* p = e *)

and let_rec_bindings =
  RecFun  of (let_rec_fun_binding,  kwd_and) nsepseq
| RecNode of (let_rec_node_binding, kwd_and) nsepseq reg

and let_rec_fun_binding  = var reg * eq * fun_expr
and let_rec_node_binding = var reg * eq * node_expr

and pattern =
  Ptuple of pattern csv reg                                  (* p1, p2, ...   *)
| Plist  of pattern ssv bra reg                              (* [p1; p2; ...] *)
| Pvar   of var reg                                          (*             x *)
| Punit  of unit__ reg                                       (*            () *)
| Pwild  of wild                                             (*             _ *)
| Pcons  of (pattern * cons * pattern) reg                   (*      p1 :: p2 *)
| Ppar   of pattern par reg                                  (*           (p) *)

and expr =
  LetExpr of let_expr reg       (* let [rec] p1 = e1 and p2 = e2 and ... in e *)
| Fun     of fun_expr           (* fun x -> e                                 *)
| If      of conditional reg    (* if e1 then e2 else e3                      *)
| Tuple   of cat_expr csv reg   (* e1, e2, ...                                *)
| CatExpr of cat_expr

and let_expr =
  LetIn    of kwd_let           * let_bindings     * kwd_in * expr
| LetRecIn of kwd_let * kwd_rec * let_rec_bindings * kwd_in * expr

and fun_expr = (kwd_fun * var reg * arrow * expr) reg

and conditional = kwd_if * expr * kwd_then * expr * kwd_else * expr

and cat_expr =
  Cat      of (cons_expr * cat * cat_expr) reg                     (* e1 ^ e2 *)
| ConsExpr of cons_expr

and cons_expr =
  Cons     of (disj_expr * cons * cons_expr) reg                  (* e1 :: e2 *)
| DisjExpr of disj_expr

and disj_expr =
  Or       of (disj_expr * bool_or * conj_expr) reg               (* e1 || e2 *)
| ConjExpr of conj_expr

and conj_expr =
  And      of (conj_expr * bool_and * comp_expr) reg              (* e1 && e2 *)
| CompExpr of comp_expr

(* Comparisons *)

and comp_expr =
  Lt      of (comp_expr * lt * add_expr) reg                      (* e1  < e2 *)
| LEq     of (comp_expr * le * add_expr) reg                      (* e1 =< e2 *)
| Gt      of (comp_expr * gt * add_expr) reg                      (* e1  > e2 *)
| GEq     of (comp_expr * ge * add_expr) reg                      (* e1 >= e2 *)
| NEq     of (comp_expr * ne * add_expr) reg                      (* e1 <> e2 *)
| Eq      of (comp_expr * eq * add_expr) reg                      (* e1  = e2 *)
| AddExpr of add_expr

and add_expr =
  Add      of (add_expr * plus   * mult_expr) reg                 (* e1 +  e2 *)
| AddQ     of (add_expr * plusQ  * mult_expr) reg                 (* e2 +. e2 *)
| Sub      of (add_expr * minus  * mult_expr) reg                 (* e1 -  e2 *)
| SubQ     of (add_expr * minusQ * mult_expr) reg                 (* e1 -. e2 *)
| MultExpr of mult_expr

and mult_expr =
  Mult      of (mult_expr * mult    * unary_expr) reg            (* e1  *  e2 *)
| MultQ     of (mult_expr * multQ   * unary_expr) reg            (* e1  *. e2 *)
| Div       of (mult_expr * div     * unary_expr) reg            (* e1  /  e2 *)
| DivQ      of (mult_expr * divQ    * unary_expr) reg            (* e1  /. e2 *)
| Mod       of (mult_expr * kwd_mod * unary_expr) reg            (* e1 mod e2 *)
| UnaryExpr of unary_expr

and unary_expr =
  Neg     of (minus   * core_expr) reg                               (*    -e *)
| NegQ    of (minusQ  * core_expr) reg                               (*   -.e *)
| Not     of (kwd_not * core_expr) reg                               (* not e *)
| Primary of primary_expr

and primary_expr =
  GenExpr of gen_expr
| NeuExpr of neuron_expr
| NetExpr of net_expr reg

and net_expr = kwd_net * core_expr * primary_expr            (* net name expr *)

and gen_expr =
  CallExpr of (gen_expr * core_expr) reg                               (* f x *)
| CoreExpr of core_expr

and core_expr =
  Q      of (string * Q.t) reg                               (* 123/45 1.2345 *)
| Z      of Z.t reg                                          (* 12345         *)
| Var    of var reg                                          (* x             *)
| String of string reg                                       (* "foo"         *)
| Unit   of unit__ reg                                       (* ()            *)
| True   of kwd_true                                         (* true          *)
| False  of kwd_false                                        (* false         *)
| Par    of expr par reg                                     (* (e)           *)
| List   of expr ssv bra reg                                 (* [e1; e2; ...] *)
| Extern of extern

and extern =
  Fuse     of (var * var)                                       (* fuse (x,y) *)
| Cast     of cast_expr
| Print    of print_expr
| Scanf    of scanf_expr
| QComp    of qcomp_expr                          (* comparisons on rationals *)
| NodeEq   of (var * var)                              (* equality on neurons *)
| PolyEq   of (var * var)                             (* polymorphic equality *)
| Str2Ints of var                            (* TEMPORARY: string_to_int_list *)

and qcomp_expr =
  EqQ  of (var * var)
| LtQ  of (var * var)
| LeqQ of (var * var)
| GtQ  of (var * var)
| GeqQ of (var * var)

and cast_expr =
  StringOfZ     of var                                   (* string_of_int   x *)
| StringOfBool  of var                                   (* string_of_bool  x *)
| StringOfWe    of var                                   (* string_of_we    x *)
| StringOfSlope of var                                   (* string_of_slope x *)
| StringOfGmul  of var                                   (* string_of_gmul  x *)
| StringOfDec   of var                                   (* string_of_dec   x *)
| Ratio         of var                                   (* ratio           x *)
| Trunc         of var                                   (* trunc           x *)
| NameOfNode    of var                                   (* name_of_node    x *)
| AddrOfNode    of var                                   (* addr_of_node    x *)

and print_expr =
  PrintString of var                                        (* print_string x *)
| PrintInt    of var                                        (* print_int    x *)
| PrintBool   of var                                        (* print_bool   x *)
| PrintWe     of var                                        (* print_we     x *)
| PrintSlope  of var                                        (* print_slope  x *)
| PrintGmul   of var                                        (* print_gmul   x *)
| PrintDec    of var                                        (* print_dec    x *)

and scanf_expr =
  ScanfInt    of var                                        (* scanf_int    x *)
| ScanfString of var                                        (* scanf_string x *)
| ScanfBool   of var                                        (* scanf_bool   x *)
| ScanfDec    of var                                        (* scanf_dec    x *)

and neuron_expr =
                   (* node ~init:state "name" V weight  delay  <- source  ... *)
                   (* node ~init:state "name" V weights delays <= sources     *)
                   (* node             "name" V weight  delay  <- source  ... *)
                   (* node             "name" V weights delays <= sources     *)
  Node   of node_expr
| Output of (kwd_output * annotations * neuron_name * synapses) reg
                                                 (* output ~init:state "name" *)
                                                 (* output             "name" *)
| Input  of (kwd_input  * annotations * neuron_name) reg

and node_expr = (kwd_node * annotations * neuron_name * synapses) reg

and annotations = label_assoc reg list

and label_assoc = {
  label: label reg reg;                (* The outer region is for "~<label>:" *)
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

(* Normalising nodes of the AST so the interpreter is more uniform and
   no source regions are lost in order to enable all manner of
   source-to-source transformations from the rewritten AST and the
   initial source.

   The first kind of expressions to be normalised is lambdas, like:

     fun a -> fun b -> a
     fun a b -> a
     fun a (b,c) -> a

   to become

     fun a -> fun b -> a
     fun a -> fun b -> a
     fun a -> fun x -> let (b,c) = x in a

   The second kind is let-bindings introducing functions without the
   "fun" keyword, like

     let g a b = a
     let h a (b,c) = a

   which become

     let g = fun a -> fun b -> a
     let h = fun a -> fun x -> let (b,c) = x in a

   The former is actually a subcase of the latter. Indeed, the general
   shape of the former is

     fun <patterns> -> <expr>

   and the latter is

     let <ident> <patterns> = <expr>

   The isomorphic parts are "<patterns> -> <expr>" and "<patterns> =
   <expr>".

     The call [norm patterns sep expr], where [sep] is a region either
   of an "->" or a "=", evaluates in a function expression (lambda),
   as expected. In order to get the regions right in the case of
   lambdas, additional regions are passed: [norm ~reg:(total,kwd_fun)
   patterns sep expr], where [total] is the region for the whole
   lambda (even if the resulting lambda is actually longer: we want to
   keep the region of the original), and the region of the original
   "fun" keyword.
*)

type sep = Region.t
val norm: ?reg:(Region.t * kwd_fun) -> pattern nseq -> sep -> expr -> fun_expr

(* Undoing the above rewritings (for debugging by comparison with the
   lexer, and to feed the source-to-source transformations with only
   tokens that originated from the original input.

     Unparsing is performed on an expression which is expected to be a
   series "fun ... -> fun ... -> ...". Either this expression is the
   right-hand side of a let, or it is not. These two cases are
   distinguished by the function [unparse], depending on the first
   keyword "fun" being concrete or ghostly (virtual). In the former
   case, we are unparsing an expression which was originally starting
   with "fun"; in the latter, we are unparsing an expression that was
   parsed on the right-hand side of a let construct. In other words,
   in the former case, we expect to reconstruct

                    let f p_1 ... p_n = e

   whereas, in the second case, we want to obtain

                    fun p_1 ... p_n -> e

     In any case, the heart of the unparsing is the same, and this is
   why the data constructors [`Fun] and [`Let] of the type [unparsed]
   share a common type: [pattern * Region.t * expr], the region can
   either actually denote the alias type [arrow] or [eq]. Let us
   assume a value of this triple [patterns, separator_region,
   expression]. Then the context (handled by [unparse]) decides if
   [separator_region] is the region of a "=" sign or "->".

   There are two forms to be unparsed:

     fun x_1 -> let p_1 = x_1 in ... fun x_n -> let p_n = x_n in e

   or

     fun p_1 -> ... fun p_n -> e

   in the first case, the rightmost "=" becomes [separator_region]
   above, whereas, in the second case, it is the rightmost "->".

   Here are some example covering all cases:

   let rec f = fun a -> fun b -> a
   let rec g = fun a b -> a
   let rec h = fun a (b,c) -> a
   let rec fst = fun (x,_) -> x

   let rec g a b = a
   let rec h (b,c) a (d,e) = a
   let len = (fun n _ -> n)
   let f l = let n = l in n
*)

type unparsed = [
  `Fun  of (kwd_fun * (pattern nseq * arrow * expr))
| `Let  of (pattern nseq * eq * expr)
| `Idem of expr
]

val unparse: expr -> unparsed

(* Promotion of variables and expressions *)

val var_to_unary_expr: var reg -> unary_expr
val var_to_mult_expr:  var reg -> mult_expr
val var_to_add_expr:   var reg -> add_expr
val var_to_comp_expr:  var reg -> comp_expr
val var_to_conj_expr:  var reg -> conj_expr
val var_to_disj_expr:  var reg -> disj_expr
val var_to_cons_expr:  var reg -> cons_expr
val var_to_cat_expr:   var reg -> cat_expr
val var_to_expr:       var reg -> expr

val core_to_expr:      core_expr    -> expr
val primary_to_expr:   primary_expr -> expr
val gen_to_expr:       gen_expr     -> expr

(* Conversions to type [string] *)

val to_string:         t -> string
val pattern_to_string: pattern -> string

(* Printing the tokens reconstructed from the AST. This is very useful
   for debugging, as the output of [print_token ast] can be textually
   compared to that of [Lexer.trace] (see module [LexerMain]). The
   optional parameter [undo] is bound to [true] if the caller wants
   the AST to be unparsed before printing (those nodes that have been
   normalised with function [norm_let] and [norm_fun]). *)

val print_tokens: ?undo:bool -> ast -> unit

(* Projecting regions from sundry nodes of the AST. See the first
   comment at the beginning of this file. *)

val region_of_pattern:           pattern -> Region.t
val region_of_neuron_expr:   neuron_expr -> Region.t
val region_of_expr:                 expr -> Region.t
val region_of_primary_expr: primary_expr -> Region.t
val region_of_gen_expr:         gen_expr -> Region.t
val region_of_core_expr:       core_expr -> Region.t
val region_of_unary_expr:     unary_expr -> Region.t
val region_of_mult_expr:       mult_expr -> Region.t
val region_of_add_expr:         add_expr -> Region.t
val region_of_comp_expr:       comp_expr -> Region.t
val region_of_conj_expr:       conj_expr -> Region.t
val region_of_disj_expr:       disj_expr -> Region.t
val region_of_cons_expr:       cons_expr -> Region.t
val region_of_cat_expr:         cat_expr -> Region.t

(* Removing all outermost parentheses from a given expression *)

val rm_par: expr -> expr

(* Predicates on nodes *)

val is_var:    expr -> bool
val is_neuron: expr -> bool
val is_call:   expr -> bool
val is_fun:    expr -> bool

(* Variables *)

type rvar = var reg

module Vars:     Set.S with type elt = string
module FreeVars: Set.S with type elt = rvar

(* The value of the call [vars t] is a pair of sets: the first is the
   set variables in the scope at the end of the program corresponding
   to the AST [t], the second is the set of free variables in the same
   AST.

   Computing free variables is useful because we do not want to escape a
   Slang variable that is a predefined variable in OCaml, when we
   translate the Slang program to OCaml: this way, we make sure that
   an unbound Slang variable is caught before the translation (where
   it would be wrongly captured by the OCaml compiler).

   Dually, computing bound variables is useful when compiling to OCaml.
*)

val vars: t -> Vars.t * FreeVars.t

(* Checking and normalising the AST *)

exception Duplicate_label of rvar
exception Invalid_label   of rvar

val normalise: t -> t
