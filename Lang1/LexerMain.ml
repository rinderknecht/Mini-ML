(* Pilot for the lexer of Slang *)

open! EvalOpt (* Reads the command-line options: Effectful! *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

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

let () = Lexer.trace pp_input
