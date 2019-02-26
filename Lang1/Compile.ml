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

let rec edit_ast ~verb (ast,_) =
  (if   Utils.String.Set.is_empty verb
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
   add_line (Region.start_loc reg)
@@ edit
@@ discard (Region.stop_loc reg) filter

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
            LetExpr (_,e)  -> edit_let_expr    e
     |  Fun (_,(_,_,_,e))  -> edit_expr        e
     |           If (_,c)  -> edit_conditional c
     |        Tuple (_,t)  -> edit_components  t
     |        Match (_,e)  -> edit_match       e
     |  Cat (_,(e1,_,e2))
     | Cons (_,(e1,_,e2))
     |   Or (_,(e1,_,e2))
     |  And (_,(e1,_,e2))
     |   Lt (_,(e1,_,e2))
     |  Gt  (_,(e1,_,e2))
     |  GEq (_,(e1,_,e2))
     |   Eq (_,(e1,_,e2))
     |  NEq (_,(e1,_,e2))
     |  LEq (_,(e1,_,e2))
     |  Add (_,(e1,_,e2))
     |  Sub (_,(e1,_,e2))
     | Mult (_,(e1,_,e2))
     |  Div (_,(e1,_,e2))
     |  Mod (_,(e1,_,e2))
     |   Call (_,(e1,e2))  -> edit_expr e1 <@ edit_expr e2
     |      Neg (_,(_,e))
     |      Not (_,(_,e))  -> edit_expr e
     | Par (_,(_,e,rpar))  -> edit_expr e <@ copy_to (Region.stop_loc rpar)
     | List (_,(_,e,rbra)) -> edit_list_expr e <@ copy_to (Region.stop_loc rbra)
     | Int _ | Var _
     | Str _ | Unit _
     | True _ | False _
     | Extern _            -> region_of_expr e |> Region.stop_loc |> copy_to
     )

and edit_match (kwd_match, expr, kwd_with, cases, _kwd_end) =
  copy_to (Region.start_loc kwd_match)
<@ insert "("
<@ edit_expr expr
<@ copy_to (Region.stop_loc kwd_with)
<@ edit_cases cases
<@ insert ")"

and edit_cases cases = nsepseq_foldr edit_case cases

and edit_case (_, _, expr) = edit_expr expr

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
  edit_expr cond <@ edit_expr ifso <@ edit_expr ifnot

and edit_components comp = nsepseq_foldr edit_expr comp

and edit_list_expr expr filter =
  sepseq_foldr edit_list_item expr filter

and edit_list_item expr =
  let start_expr = Region.start_loc (region_of_expr expr) in
     copy_to start_expr
  <@ insert "("
  <@ add_line start_expr
  <@ edit_expr expr
  <@ insert ")"

(* Generating the runtime environment (RTE) *)
(*
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
*)
