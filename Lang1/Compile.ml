(* Transformations of Mini-ML programs to OCaml *)

open! Utils
open AST
open Printf

module Trans =
  struct
    type t = Id
    type trans = t

    let compare = compare

    let equal = (=)

    let to_string = function
      Id -> "Id"
  end

module TEdit = Edit.Make (Trans)

type state = {
  trans : Trans.t;
  input : string;
}

type filter = state * TEdit.filter

type editor = filter -> filter

let insert ?loc text (state, edit) =
  state, TEdit.insert state.trans loc text edit

and overwrite loc text (state, edit) =
  state, TEdit.overwrite state.trans loc text edit

and patch ?start ~stop text (state, edit) =
  state, TEdit.patch state.trans start stop text edit

and delete ?start ~stop (state, edit) =
  state, TEdit.delete state.trans start stop edit

and discard loc (state, edit) =
  state, TEdit.discard state.trans loc edit

and copy_to loc (state, edit) =
  state, TEdit.copy_to state.trans loc edit

and skip_to char (state, edit) =
  state, TEdit.skip_to state.trans char edit

and skip_to_end (state, edit) =
  state, TEdit.skip_to_end state.trans edit

and append text (state, edit) =
  state, TEdit.append state.trans text edit

and copy_to_end (state, _) =
  state, TEdit.copy_to_end state.trans

and stop (state, _) = state, TEdit.stop

let patch_region reg =
  let start, stop = Region.locs reg in patch ~start ~stop

let delete_region reg =
  let start, stop = Region.locs reg in delete ~start ~stop

(* Getters and setters for the filters *)

let set_trans trans (state, edit) = {state with trans}, edit

and get_input (state, _) = state.input

and get_state, get_edit = fst, snd

(* Editing *)

(*let mk_pad reg = String.make (Loc.offset (Region.start_loc reg)) ' '*)

let rec edit_ast ~debug (ast,_) =
  (if Utils.String.Set.is_empty debug
   ||  Utils.String.Set.mem "compiler" debug
   then Utils.id
   else insert "let () = Printexc.record_backtrace true\n")
<@ edit_statements ast
<@ insert "\n\n"
<@ skip_to_end

and edit_statements ast filter =
  List.fold_right edit_statement ast filter

and edit_statement = function
  Let (reg, (_, bindings)) ->
    edit_binding (edit_let_bindings bindings) reg
| LetRec (reg, (_, _, bindings)) ->
    edit_binding (edit_let_rec_bindings bindings) reg

and edit_let_rec_bindings bindings =
  edit_let_rec_fun_bindings bindings

and add_line loc filter =
  let text = sprintf "\n# %d \"%s\"\n%s"
               (Loc.line loc) (get_input filter)
               (String.make (Loc.offset loc) ' ')
  in discard loc @@ insert text filter
and edit_binding edit reg filter =
  if Region.file reg = get_input filter then
    add_line (Region.start_loc reg)
  @@ edit
  @@ discard (Region.stop_loc reg) filter
  else filter

and edit_let_bindings bindings = edit_and edit_let_binding bindings

and edit_let_binding (_,_,expr) = edit_expr expr

and edit_let_rec_fun_bindings bindings =
  edit_and edit_let_rec_fun_binding bindings

and edit_let_rec_fun_binding (_,_,expr) = edit_expr (Fun expr)

and edit_and: 'a.(('a -> editor) -> ('a, kwd_and) nsepseq -> editor) =
  fun edit (binding, bindings) filter ->
    let apply (kwd_and, binding) =
      add_line (Region.start_loc kwd_and) <@ edit binding
    in edit binding
    @@ List.fold_right apply bindings filter

and edit_expr expr =
  match unparse expr with
   `Let (_,_,e) | `Fun (_,(_,_,e)) | `Idem e ->
     (match e with
           LetExpr (_,e) -> edit_let_expr    e
     | Fun (_,(_,_,_,e)) -> edit_expr        e
     |         CatExpr e -> edit_cat_expr    e
     |          If (_,c) -> edit_conditional c
     |       Tuple (_,t) -> edit_components  t)

and edit_let_in_bindings (edit, kwd_in, expr) =
   edit
<@ copy_to (Region.stop_loc kwd_in)
<@ add_line (Region.start_loc (region_of_expr expr))
<@ edit_expr expr

and edit_let_expr expr =
  edit_let_in_bindings
   (match expr with
      LetIn (_, bindings, kwd_in, expr) ->
        edit_let_bindings bindings, kwd_in, expr
    | LetRecIn (_, _, bindings, kwd_in, expr) ->
        edit_let_rec_bindings bindings, kwd_in, expr)

and edit_conditional (_,cond,_,ifso,_,ifnot) =
  let edit = edit_expr in edit cond <@ edit ifso <@ edit ifnot

and edit_components comp = nsepseq_foldr edit_cat_expr comp

and edit_cat_expr = function
  Cat (_,(e1,_,e2)) -> edit_cons_expr e1 <@ edit_cat_expr e2
|        ConsExpr e -> edit_cons_expr e

and edit_cons_expr = function
  Cons (_,(e1,_,e2)) -> edit_disj_expr e1 <@ edit_cons_expr e2
|         DisjExpr e -> edit_disj_expr e

and edit_disj_expr = function
  Or (_,(e1,_,e2)) -> edit_disj_expr e1 <@ edit_conj_expr e2
|       ConjExpr e -> edit_conj_expr e

and edit_conj_expr = function
  And (_,(e1,_,e2)) -> edit_conj_expr e1 <@ edit_comp_expr e2
|        CompExpr e -> edit_comp_expr e

and edit_comp_expr = function
  Lt (_,(e1,_,e2)) | Gt  (_,(e1,_,e2)) | GEq (_,(e1,_,e2))
| Eq (_,(e1,_,e2)) | NEq (_,(e1,_,e2)) | LEq (_,(e1,_,e2)) ->
    edit_comp_expr e1 <@ edit_add_expr e2
| AddExpr e -> edit_add_expr e

and edit_add_expr = function
  Add (_,(e1,_,e2)) | Sub (_,(e1,_,e2)) ->
    edit_add_expr e1 <@ edit_mult_expr e2
| MultExpr e -> edit_mult_expr e

and edit_mult_expr = function
  Mult (_,(e1,_,e2)) | Div  (_,(e1,_,e2)) | Mod (_,(e1,_,e2)) ->
    edit_mult_expr e1 <@ edit_unary_expr e2
| UnaryExpr e -> edit_unary_expr e

and edit_unary_expr = function
  Neg (_,(_,e)) | Not (_,(_,e)) -> edit_core_expr    e
|                     Primary e -> edit_primary_expr e

and edit_primary_expr = function
  CallExpr (_,e) -> edit_call_expr e
|     CoreExpr e -> edit_core_expr e

and edit_call_expr (func,arg) = edit_primary_expr func <@ edit_core_expr arg

and edit_core_expr = function
  Par (_,(_,e,rpar)) ->
    edit_expr e <@ copy_to (Region.stop_loc rpar)
| List (_,(_,e,rbra)) ->
    edit_list_expr e <@ copy_to (Region.stop_loc rbra)
| Int _ | Var _ | Str _ | Unit _ | True _ | False _ | Extern _ as e ->
    region_of_core_expr e |> Region.stop_loc |> copy_to

and edit_list_expr expr filter =
  sepseq_foldr edit_list_item expr filter

and edit_list_item expr =
  let start_expr = Region.start_loc (region_of_expr expr) in
     copy_to start_expr
  <@ insert "("
  <@ add_line start_expr
  <@ edit_expr expr
  <@ insert ")"

(* Adding the "open Prologue" at the beginning of the OCaml module *)

(* Adding the opening of the functor *)

let add_functor = insert "module Run () = struct\n"

(* Adding the "end" keyword *)

let add_end = skip_to_end <@ insert "end\n"

(* Generating the runtime environment (RTE) *)

let add_error_printing =
  insert (sprintf
   "(* Error printing *)\n\n\
    let runtime text region =\n\
    %s  let msg = Lexer.error_to_string ~kind:\"\\nRuntime\" text region\n\
    %s  in Utils.highlight msg\n\n\
    let static text region =\n\
    %s  let msg = Lexer.error_to_string ~kind:\"\\nStatic\" text region\n\
    %s  in Utils.highlight msg\n\n\
    let internal text =\n\
    %s  Utils.highlight (Printf.sprintf \"Internal error: %%s\" text); exit 1\n\n\
    let external_ text =\n\
    %s  Utils.highlight (Printf.sprintf \"External error: %%s\" text); exit 2\n\n"
    "" "" "" "" "" "")

(**)

let add_functor_call filter =
  let mod_name =
    get_input filter |> Filename.remove_extension |> String.capitalize_ascii
  in insert (sprintf
        "(* Running the application and catching errors *)\n\n\
         let () =\n\
         %s  try\n" "")
   @@ insert (sprintf
        "%s    let module M = %s.Run () in\n"
        "" mod_name)
   @@ insert (sprintf
       "%s    ()\n\
        %s  with\n"
        "" "")
     filter

let add_error_handling =
  insert (sprintf
    "    Net.Multiple_syn Net.{src; lbl; dst} ->\n\
     %s      Printf.eprintf \"Duplicate synapse %%s \
                            from %%s (%%s) to %%s (%%s).\\n\"\n\
     %s        (Label.to_string lbl)\n\
     %s        (Neuron.get_name src) \
               (Neuron.get_addr src |> Net.Addr.to_string)\n\
     %s        (Neuron.get_name dst) \
               (Neuron.get_addr dst |> Net.Addr.to_string)\n\
     %s  | Eval.Nameless_input (state, region) ->\n\
     %s      runtime \"Nameless input neuron.\" region;\n\
     %s      try_dot state; exit 7\n\
     %s  | Eval.Nameless_output (state, region) ->\n\
     %s      runtime \"Nameless output neuron.\" region;\n\
     %s      try_dot state; exit 7\n\
     %s  | Eval.Toplevel_state (state, region) ->\n\
     %s      runtime \"Top-level neuron with an explicit state.\" region;\n\
     %s      try_dot state; exit 7\n\
     %s  | Eval.Homograph (state, _, (name_reg, name), addr) ->\n\
     %s      let msg = Printf.sprintf\n\
     %s        \"Duplicate neuron named \\\"%%s\\\" in network \\\"%%s\\\"\"\n\
     %s        name (Network.Net.Addr.to_string addr)\n\
     %s      in runtime msg name_reg; try_dot state; exit 7\n"

    "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "  " ""

)
