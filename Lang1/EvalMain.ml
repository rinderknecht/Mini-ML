(* Driver for the compiler and interpreter of Mini-ML *)

let sprintf = Printf.sprintf

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let runtime text region =
  let msg = Lexer.format_error ~kind:"\nRuntime" (text, region)
  in Utils.highlight msg

let static text region =
  let msg = Lexer.format_error ~kind:"\nStatic" (text, region)
  in Utils.highlight msg

let internal text =
  Utils.highlight (sprintf "Internal error: %s" text); exit 1

let external_ text =
  Utils.highlight (sprintf "External error: %s" text); exit 2


(* Path to the Mini-ML standard library *)

let lib_path =
  match EvalOpt.libs with
      [] -> ""
  | libs -> let mk_I dir path = sprintf " -I %s%s" dir path
           in List.fold_right mk_I libs ""


(* Opening the input channel and setting the lexing engine *)

let cin, reset =
  match EvalOpt.input with
    None | Some "-" -> stdin, fun ?(line=1) _buffer -> ignore line
  |       Some file -> open_in file, Lexer.reset ~file

let buffer = Lexing.from_channel cin
let     () = reset buffer

(* Tokeniser *)

let tokeniser =
  if Utils.String.Set.mem "lexer" EvalOpt.verbose then
    Lexer.get_token ~log:(stdout, Lexer.output_token buffer)
  else Lexer.get_token ?log:None

(* Out-channel opening and closing *)

let open_out_chan = function
       None -> None
| Some file -> Some (if file = "-" then stdout else open_out file)

and close_out_opt = function
       None -> ()
| Some chan -> try close_out_noerr chan with Sys_error _ -> ()

(* Main *)

let () =
  try
    (* Parsing the input program *)

    let ast = Parser.program tokeniser buffer in

    (* Optional pretty-printing of tokens after parsing *)

    let () = if Utils.String.Set.mem "parser" EvalOpt.verbose then
               if Utils.String.Set.mem "unparsing" EvalOpt.verbose then
                 AST.print_tokens ~undo:true ast
               else AST.print_tokens ast in

    (* Checking for bound and free variables *)

    let _bv, fv = AST.vars ast in (* bv: bound vars; fv: free vars *)
    let () =
      let apply (region, free_var) =
        let msg = sprintf "Unbound variable \"%s\"." free_var
        in static msg region
      in AST.FreeVars.iter apply fv in
    let () = if not (AST.FreeVars.is_empty fv) then exit 3 in

    (* Interpreting the Mini-ML source code *)

    let () =
      if EvalOpt.eval then
        (* Interpreting the Abstract Syntax Tree *)

        ignore (Eval.eval ast) in

    (* Compiling Mini-ML to OCaml *)

    let () =
      match EvalOpt.compile with
             None -> ()
      | Some file ->
         let open Compile in
         let input =
           match EvalOpt.input with
             Some file_path -> file_path
           | None -> assert false in (* "-c" implies an input file ([EvalOpt]) *)
          let state = {
            trans = Trans.Id;
            input = Filename.basename input
          } in

          let edit =
             TEdit.compile
          @@ get_edit
          @@ edit_ast ~verb:EvalOpt.verbose ast
            (state, TEdit.stop) in

          (* I/O maps *)

          let io = TEdit.init_io Trans.to_string
                   |> TEdit.add Trans.Id ~in_:input ~out:file in

          let edits = [edit] in (* TEMPORARY *)

          (* Optionally adding the runtime environment (RTE) edits *)
(*
          let io, edits =
            if EvalOpt.rte then
              let ml_rte =
                let open Filename in
                  sprintf "%s%s%sRTE.ml"
                   (dirname file) dir_sep (remove_extension file |> basename) in

              Edit.add Compile.RTE ~in_:EvalOpt.input ~out:ml_rte io,
              let open Compile in
              let state = {state with trans = RTE} in
              let edit_rte =
                (if EvalOpt.tco then Edit.compile_cps else Edit.compile)
              @@ get_edit
              @@ add_out_channels
              @@ add_error_printing
              @@ add_error_handling
                (state, Edit.stop)
              in [edit; edit_rte]
            else io, [edit] in
*)
          (* Checking all edits *)

          let () = if Utils.String.Set.mem "editor" EvalOpt.verbose then
                    (print_endline "\nEDITS"; TEdit.show io edits) in

          let check edit =
            try TEdit.check edit with (* The following are internal errors. *)
              TEdit.Invalid (loc1, loc2) ->
                let msg = sprintf "Decreasing locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg
            | Loc.Incomparable (loc1, loc2) ->
                let msg = sprintf "Incomparable locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg in

          let () = List.iter check edits in

          (* Optimising the edits *)

          let desc, edits =
            TEdit.build ~opt:(not EvalOpt.raw_edits) io edits in

          let () = if Utils.String.Set.mem "editor" EvalOpt.verbose
                   && not EvalOpt.raw_edits
                   then (print_endline "\nOPTIMISED EDITS";
                         TEdit.show io edits) in

          (* Applying the edits *)

          List.iter (TEdit.apply desc) edits;
          TEdit.close_out_desc io desc in
    ()
  with
  (* Lexing errors *)

    Lexer.Error diag ->
      close_in cin; Lexer.prerr ~kind:"Lexical" diag; exit 5

  (* Parsing errors *)

  | Parser.Error ->
      Lexer.prerr ~kind:"Syntactical"
        ("Parse error.",
         Region.make ~start:(Lexing.lexeme_start_p buffer)
                     ~stop:(Lexing.lexeme_end_p buffer));
      exit 6

  (* Evaluation errors *)

  | Eval.Div_by_zero (_state, reg) ->
      let msg = "Division by zero."
      in runtime msg reg; exit 7

  | Eval.Nonlinear_pattern (_state,(region,x)) ->
      let msg = sprintf "Repeated variable \"%s\" in pattern." x
      in runtime msg region; exit 7

  | Eval.Multiple_decl (_state,(region,x)) ->
      let msg = sprintf "Variable \"%s\" is bound several times \
                                in this matching." x
      in runtime msg region; exit 7

  | Eval.Type_error (_state, info) ->
      let msg = sprintf "Runtime error: Type error.\n%s\n" info
      in Utils.highlight msg; exit 7

  (* Internal errors *)

  | Eval.Env.Unbound (region,x) ->
      let msg = sprintf "Unbound variable \"%s\" (%s)."
                   x (Region.to_string region)
      in internal msg

  (* System errors *)

  | Sys_error msg ->
      Utils.highlight (sprintf "System error: %s." msg); exit 8


(* Closing the input channel (Mini-ML source code) *)

let () = close_in_noerr cin
