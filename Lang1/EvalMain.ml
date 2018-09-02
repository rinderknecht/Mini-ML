module Foo = PolyMap (* TEMPORARY *)

(* Pilot for the compiler/interpreter of Slang *)

let id = Utils.id

open! EvalOpt (* Reads the command-line options: Effectful! *)


(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let runtime text region =
  let msg = Lexer.error_to_string ~kind:"\nRuntime" text region
  in Utils.highlight msg

let static text region =
  let msg = Lexer.error_to_string ~kind:"\nStatic" text region
  in Utils.highlight msg

let internal text =
  Utils.highlight (Printf.sprintf "Internal error: %s" text); exit 1

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 2


(* Path to the Slang standard library *)

let lib_path =
  match EvalOpt.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
           in List.fold_right mk_I libs ""


(* Preprocessing the input source and opening the input channels *)

let prefix =
  if EvalOpt.input = "-" then "temp"
  else Filename.(EvalOpt.input |> basename |> remove_extension)

let suffix = ".pp.sl"

let pp_input =
  if Utils.String.Set.mem "cpp" EvalOpt.debug
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  Printf.sprintf "cpp -traditional-cpp%s %s%s %s"
    lib_path
    (match cpp with None -> "" | Some symb -> "-D" ^ symb ^ " ")
    input pp_input

let () =
  if Utils.String.Set.mem "cpp" EvalOpt.debug
  then Printf.eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (Printf.sprintf "the command \"%s\" failed." cpp_cmd)

let pp_chan = open_in pp_input


(* Lexing buffer and tokeniser *)

let base_input = Filename.basename input
let buffer = Lexing.from_channel pp_chan
let () = Lexer.reset ~file:base_input buffer

let tokeniser =
  if Utils.String.Set.mem "lexer" EvalOpt.debug then
    Lexer.get_token ~log:(stdout, Lexer.output_token buffer)
  else Lexer.get_token ?log:None


(* Out-channel opening and closing *)

let open_out_chan = function
       None -> None
| Some file -> Some (if file = "-" then stdout else open_out file)

and close_out_opt = function
       None -> ()
| Some chan -> try close_out_noerr chan with Sys_error _ -> ()


(* GraphViZ Dot out-channel *)

let maybe_dot ?labels zip = function
       None -> ()
| Some chan -> Network.Zipper.to_dot_chan ?labels chan zip

let try_dot ?labels state =
  match Eval.State.(state.thread.dot) with
         None -> ()
  | Some chan ->
      let zip = Eval.State.(state.thread.zip) in
      try Network.Zipper.to_dot_chan ?labels chan zip with _ -> ()


(* Main *)

let () =
  try
    (* Parsing the input program *)

    let ast = Parser.program tokeniser buffer in

    (* Optional pretty-printing of tokens after parsing *)

    let () = if Utils.String.Set.mem "parser" EvalOpt.debug then
               if Utils.String.Set.mem "unparsing" EvalOpt.debug then
                 AST.print_tokens ~undo:true ast
               else AST.print_tokens ast in

    (* Checking for duplicate and invalid neuron labels *)

    let ast =
      try AST.normalise ast with
        AST.Duplicate_label (region, label) ->
          let msg =
            Printf.sprintf "Duplicate neuron label \"%s\"." label
          in runtime msg region; exit 7
      | AST.Invalid_label (region, label) ->
          let msg =
            Printf.sprintf "Invalid neuron label \"%s\"." label
          in runtime msg region; exit 7 in

    (* Optional pretty-printing of tokens after AST normalisation *)

    let () = if Utils.String.Set.mem "norm" EvalOpt.debug then
               AST.print_tokens ast in

    (* Checking for bound and free variables *)

    let bv, fv = AST.vars ast in (* bv: bound vars; fv: free vars *)
    let () =
      let apply (region, free_var) =
        let msg = Printf.sprintf "Unbound variable \"%s\"." free_var
        in static msg region
      in AST.FreeVars.iter apply fv in
    let () = if not (AST.FreeVars.is_empty fv) then exit 3 in

    (* Generating OCaml code for type-checking *)

    let () =
      match EvalOpt.infer with
             None -> ()
      | Some file ->
          let state = Typing.{
            trans = Typing;
            input = Filename.basename EvalOpt.input;
            tco   = EvalOpt.tco;
          } in

          let edit =
            (if EvalOpt.tco then Edit.compile_cps else Edit.compile)
          @@ Typing.get_edit
          @@ (if EvalOpt.no_stdlib then id else Typing.add_prelude)
          @@ Typing.edit_ast ast
            (state, Edit.stop) in

          (* I/O maps *)

          let ml  = if Utils.String.Set.mem "infer" EvalOpt.debug
                    then file
                    else let open Filename in
                         get_temp_dir_name () ^ dir_sep ^ basename file in

          let io  = Edit.init_IO Typing.to_string
                  |> Edit.add Typing.Typing ~in_:EvalOpt.input ~out:ml in

          (* Checking all edits *)

          let () = if Utils.String.Set.mem "editor" EvalOpt.debug then
                    (print_endline "\nEDITS"; Edit.show io [edit]) in

          let () =
            try Edit.check edit with
              Edit.Invalid (loc1, loc2) ->
                let msg = Printf.sprintf "Invalid locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg
            | Loc.Incomparable (loc1, loc2) ->
                let msg = Printf.sprintf "Incomparable locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg in

          (* Optimising the edits *)

          let desc, edits =
            Edit.build ~opt:(not EvalOpt.raw_edits) io [edit] in

          let () =
            if Utils.String.Set.mem "editor" EvalOpt.debug && not EvalOpt.raw_edits
            then (print_endline "\nOPTIMISED EDITS"; Edit.show io edits) in

          (* Printing the edits as they are applied to the source file *)

          let apply =
            if Utils.String.Set.mem "editor" EvalOpt.debug
            then (print_endline "\nAPPLIED EDITS";
                  Edit.apply ~io desc)
            else Edit.apply desc in

          let () = List.iter apply edits in
          let () = Edit.close_out_desc io desc in
      () in

    let () =
      if EvalOpt.eval then
        (* Output channels *)

        let net_chan   = open_out_chan EvalOpt.net
        and stats_chan = open_out_chan EvalOpt.stats
        and dot_chan   = open_out_chan EvalOpt.dot
        and log_chan   = open_out_chan EvalOpt.log in

        let close_all () =
          try
            List.iter close_out_opt [net_chan; stats_chan; dot_chan; log_chan]
          with Sys_error _ -> () in

        (* Interpreting the Abstract Syntax Tree *)

        let state =
          Eval.eval ?log:log_chan ?dot:dot_chan ?net:net_chan ~top:"top" ast in

        (* Exporting the results of the interpretation or debugging it *)

        if net_chan <> None || dot_chan <> None || stats_chan <> None
           || Utils.String.Set.mem "eval" EvalOpt.debug
        then
          let zip = Network.Zipper.normalise Eval.State.(state.thread.zip) in
          let delays =
            let open Eval in
            let env = state.State.env in
            let t_neu =
              "t_neu",
              match Env.find (Region.ghost, "t_neu") env with
                    Value.Integer (_,z) -> `Int (Z.to_int z)
              |                       _ -> `Null
              | exception Env.Unbound _ -> `Null
            and t_syn =
              "t_syn",
              match Env.find (Region.ghost, "t_syn") env with
                    Value.Integer (_,z) -> `Int (Z.to_int z)
              |                       _ -> `Null
              | exception Env.Unbound _ -> `Null
            and t_min =
              "t_min",
              match Env.find (Region.ghost, "t_min") env with
                    Value.Integer (_,z) -> `Int (Z.to_int z)
              |                       _ -> `Null
              | exception Env.Unbound _ -> `Null
            and t_cod =
              "t_cod",
              match Env.find (Region.ghost, "t_cod") env with
                    Value.Integer (_,z) -> `Int (Z.to_int z)
              |                       _ -> `Null
              | exception Env.Unbound _ -> `Null
            in `Assoc [t_neu; t_syn; t_min; t_cod] in

          let () =
            match net_chan with
                   None -> ()
            | Some chan -> Network.Zipper.to_json_chan
                            ~pretty ~delays chan zip in
          let () = maybe_dot ~labels:EvalOpt.labels zip dot_chan in

          let () =
            match stats_chan with
                   None -> ()
            | Some chan -> Network.Zipper.Stats.to_json_chan
                            ~pretty chan zip
          in close_all () in

    let () =
      match EvalOpt.compile with
             None -> ()
      | Some file ->
          (* Compiling Slang to OCaml *)

          let state = Compile.{
            trans = Pass1;
            input = Filename.basename EvalOpt.input;
            tco   = EvalOpt.tco;
            log   = EvalOpt.log;
            stats = EvalOpt.stats
          } in

          let edit =
            let open Compile in
            (if EvalOpt.tco then Edit.compile_cps else Edit.compile)
          @@ get_edit
          @@ add_functor
          @@ (if EvalOpt.no_stdlib then id else add_prologue)
          @@ edit_ast ~debug:EvalOpt.debug ast
          @@ add_end
            (state, Edit.stop) in

          (* I/O maps *)

          let ml = if Utils.String.Set.mem "compiler" EvalOpt.debug
                   then file
                   else let open Filename in
                        get_temp_dir_name () ^ dir_sep ^ basename file in

          let io =  Edit.init_IO Compile.to_string
                 |> Edit.add Compile.Pass2 ~in_:EvalOpt.input ~out:ml
                 |> Edit.add Compile.Pass1 ~in_:EvalOpt.input ~out:ml in

         (* Optionally adding the runtime environment (RTE) edits *)

          let io, edits =
            if EvalOpt.rte then
              let ml_rte =
                let open Filename in
                if Utils.String.Set.mem "compiler" EvalOpt.debug
                then Printf.sprintf "%s%s%sRTE.ml"
                      (dirname file) dir_sep (remove_extension file |> basename)
                else get_temp_dir_name () ^ dir_sep ^ basename file in

              Edit.add Compile.RTE ~in_:EvalOpt.input ~out:ml_rte io,
              let open Compile in
              let state = {state with trans = RTE} in
              let edit_rte =
                (if EvalOpt.tco then Edit.compile_cps else Edit.compile)
              @@ get_edit
              @@ add_prologue
              @@ add_out_channels
              @@ add_error_printing
              @@ add_try_dot
              @@ add_functor_call ~bv ~pretty ~labels
              @@ add_error_handling
                (state, Edit.stop)
              in [edit; edit_rte]
            else io, [edit] in

          (* Checking all edits *)

          let () = if Utils.String.Set.mem "editor" EvalOpt.debug then
                    (print_endline "\nEDITS"; Edit.show io edits) in

          let check edit =
            try Edit.check edit with (* The following are internal errors. *)
              Edit.Invalid (loc1, loc2) ->
                let msg = Printf.sprintf "Decreasing locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg
            | Loc.Incomparable (loc1, loc2) ->
                let msg = Printf.sprintf "Incomparable locations %s and %s.\n"
                           (Loc.to_string loc1) (Loc.to_string loc2)
                in internal msg in

          let () = List.iter check edits in

          (* Optimising the edits *)

          let desc, edits =
            Edit.build ~opt:(not EvalOpt.raw_edits) io edits in

          let () = if Utils.String.Set.mem "editor" EvalOpt.debug
                   && not EvalOpt.raw_edits
                   then (print_endline "\nOPTIMISED EDITS";
                         Edit.show io edits) in

          (* Applying the edits *)

          List.iter (Edit.apply desc) edits;
          Edit.close_out_desc io desc in

    ()
  with
  (* Lexing errors *)

    Lexer.Error diag ->
      close_in pp_chan; Lexer.prerr ~kind:"Lexical" diag; exit 5

  (* Parsing errors *)

  | Parser.Error ->
      Lexer.prerr ~kind:"Syntactical"
        ("Parse error.",
         Region.make ~start:(Lexing.lexeme_start_p buffer)
                     ~stop:(Lexing.lexeme_end_p buffer));
      exit 6

  (* Evaluation errors *)

  | Eval.Div_by_zero (state, reg) ->
      let msg = "Division by zero."
      in runtime msg reg; try_dot state; exit 7

  | Eval.Homograph (state, neu_reg, (name_reg, name), addr) ->
      let reg_str =
        if Region.is_ghost neu_reg then ""
        else Printf.sprintf "See also %s." (Region.to_string neu_reg) in
      let msg =
        Printf.sprintf "Duplicate neuron named \"%s\" in network \"%s\".\n%s"
          name (Network.Net.Addr.to_string addr)
          (if reg_str = "" then "" else reg_str ^ " ")
      in runtime msg name_reg; try_dot state; exit 7

  | Eval.Nonlinear_pattern (state,(region,x)) ->
      let msg = Printf.sprintf "Repeated variable \"%s\" in pattern." x
      in runtime msg region; try_dot state; exit 7

  | Eval.Multiple_decl (state,(region,x)) ->
      let msg = Printf.sprintf "Variable \"%s\" is bound several times \
                                in this matching." x
      in runtime msg region; try_dot state; exit 7

  | Eval.Type_error (state, info) ->
      let msg = Printf.sprintf "Runtime error: Type error.\n%s\n" info
      in Utils.highlight msg; try_dot state; exit 7

  | Eval.Nameless_input (state, region) ->
      let msg = "Nameless input neuron."
      in runtime msg region; try_dot state; exit 7

  | Eval.Nameless_output (state, region) ->
      let msg = "Nameless output neuron."
      in runtime msg region; try_dot state; exit 7

  | Eval.Multiple_syn (state, src, dst, syn_reg, syn_reg') ->
      let msg =
        Printf.sprintf "Duplicate synapse (see %s) from neuron \"%s\" \
                        to neuron \"%s\"."
          (Region.to_string ~file:false syn_reg)
          (Network.Neuron.get_name src)
          (Network.Neuron.get_name dst)
      in runtime msg syn_reg'; try_dot state; exit 7

  | Eval.NotSource (state, (reg, neu), in_deg) ->
      let msg =
        Printf.sprintf
          "The neuron \"%s\" in network \"%s\" cannot be fused because \
           it has %d incoming synapse%s"
          (Network.Neuron.get_name neu)
          Network.(Neuron.get_addr neu |> Net.Addr.to_string)
          in_deg
          (if in_deg = 1 then "." else "s.")
      in runtime msg reg; try_dot state; exit 7

  | Eval.TwoStates (state, src, (dst_reg, dst)) ->
      let msg =
        Printf.sprintf
          "Neuron \"%s\" in network \"%s\"\nand \"%s\" in \"%s\"\ncannot be fused \
           for they are in different states."
          (Network.Neuron.get_name src)
          Network.(Neuron.get_addr src |> Net.Addr.to_string)
          (Network.Neuron.get_name dst)
          Network.(Neuron.get_addr dst |> Net.Addr.to_string)
      in runtime msg dst_reg; try_dot state; exit 7

  | Eval.Duplicate (state, src, (dst_reg, dst), (label, neu)) ->
      let msg =
        Printf.sprintf
          "Neuron \"%s\" in network \"%s\"\nand \"%s\" in \"%s\"\ncannot be fused \
           for they are connected to neuron \"%s\" in \"%s\" with a synapse %s."
         (Network.Neuron.get_name dst)
          Network.(Neuron.get_addr dst |> Net.Addr.to_string)
         (Network.Neuron.get_name src)
          Network.(Neuron.get_addr src |> Net.Addr.to_string)
         (Network.Neuron.get_name neu)
          Network.(Neuron.get_addr neu |> Net.Addr.to_string)
         (Label.to_string label)
      in runtime msg dst_reg; try_dot state; exit 7

  | Eval.Toplevel_state (state, region) ->
      let msg =
        Printf.sprintf "Top-level neuron with an explicit state."
      in runtime msg region; try_dot state; exit 7

  | Eval.NonPositiveDelay (state, (reg,delay)) ->
      let msg = Printf.sprintf "Non-positive delay %s." (Delay.to_string delay)
      in runtime msg reg; try_dot state; exit 7

  | Eval.NetCollision (state, (reg, name)) ->
      let msg = Printf.sprintf "Network \"%s\" already exists." name
      in runtime msg reg; try_dot state; exit 7

  (* Internal errors *)

  | Eval.Env.Unbound (region,x) ->
      let msg = Printf.sprintf "Unbound variable \"%s\" (%s)."
                   x (Region.to_string region)
      in internal msg

  | Network.Net.Addr.Heterogeneous (a1,a2) ->
      let msg =
        Printf.sprintf "Addresses %s and %s not of the same network."
          (Network.Net.Addr.to_string a1)
          (Network.Net.Addr.to_string a2)
      in internal msg

  | Network.Zipper.Invalid_access (name, addr) ->
      let msg =
        Printf.sprintf "Invalid access to child network \"%s\" from \"%s\"."
          name (Network.Net.Addr.to_string addr)
      in internal msg

  | Network.Net.Homograph (name, addr) ->
      let msg =
        Printf.sprintf "Homograph \"%s\" in network %s."
          name
          (Network.Net.Addr.to_string addr)
      in internal msg

  | Network.Net.Missing (neuron,net) ->
      let msg =
        Printf.sprintf "Missing neuron (%s) in network \"%s\"."
          (Network.Neuron.to_string neuron)
          Network.Net.(net |> addr |> Addr.to_string)
      in internal msg

  (* System errors *)

  | Sys_error msg ->
      Utils.highlight (Printf.sprintf "System error: %s." msg); exit 8


(* Closing the input channel (Slang source code) *)

let () = close_in_noerr pp_chan
