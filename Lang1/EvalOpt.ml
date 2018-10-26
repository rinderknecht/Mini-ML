(* Parsing the command-line option for the Mini-ML compiler/interpreter *)

let abort msg =
  Utils.highlight (Printf.sprintf "Command-line error: %s" msg); exit 1

(* Help *)

let help () =
  let file = Filename.basename Sys.argv.(0) in
  Printf.printf "Usage: %s [<option> ...] <input>.ml\n" file;
  print_endline "where <input>.ml is the Mini-ML source file,";
  print_endline "and each <option> (if any) is one of the following:";
  print_endline "  -I <paths>                Library paths (colon-separated)";
  print_endline "  -o, --out=<michel>.son    Output Michelson (default \
                                             <input>.son)";
  print_endline "  -c <file>.ml              Compile to Michelson";
  print_endline "  -e, --eval                Evaluate <input>.ml";
  print_endline "  -d, --debug=<target>      cmdline, lexer, compiler, ";
  print_endline "                            parser, unparsing, norm, eval";
  print_endline "  -v, --version             Print the version number on \
                                             stdout and exit";
  print_endline "  -h, --help                This help";
  exit 0

(* Version *)

let version () = Printf.printf "%s\n" Version.version; exit 0

(* Specifying the command-line options a la GNU *)

let input     = ref None
and eval      = ref false
and compile   = ref None
and output    = ref None
and debug     = ref Utils.String.Set.empty
and libs      = ref []

let set_opt var err =
  Some (fun x -> if !var = None then var := Some x else raise (Getopt.Error err))

let split_at_colon = Str.(split (regexp ":"))

let add_path p = libs := !libs @ split_at_colon p

let add_debug d =
  debug := List.fold_left (Utils.swap Utils.String.Set.add)
                         !debug
                         (split_at_colon d)

let specs =
  let open! Getopt in [
    'I',    nolong, None, Some add_path;
    'o',     "out", set output (Some ""),
                    set_opt output "Multiple source files";
    'c',    nolong, set compile (Some ""),
                    set_opt compile "Multiple OCaml outputs";
    'e',    "eval", set eval true, None;
    'd',   "debug",         None, Some add_debug;
    'h',    "help",    Some help, None;
    'v', "version", Some version, None;
  ]
;;

(* Handler of anonymous arguments *)

let anonymous arg =
  if !input = None then
    if Sys.file_exists arg then input := Some arg
    else abort (Printf.sprintf "Input file %s does not exist." arg)
  else abort "Multiple source files."
;;

(* Parsing the command-line options *)

try Getopt.parse_cmdline specs anonymous with
  Getopt.Error msg -> abort msg
;;

(* Checking options *)

let string_of convert = function
    None -> "None"
| Some s -> Printf.sprintf "Some %s" (convert s)

let string_of_path p =
  let apply s a = if a = "" then s else s ^ ":" ^ a
  in List.fold_right apply p ""

let quote s = Printf.sprintf "\"%s\"" s

let debug_str =
  let apply e a =
    if a <> "" then Printf.sprintf "%s, %s" e a else e
  in Utils.String.Set.fold apply !debug ""

let print_opt () =
  Printf.printf "COMMAND LINE\n";
  Printf.printf "input     = %s\n" (string_of quote !input);
  Printf.printf "out       = %s\n" (string_of quote !output);
  Printf.printf "compile   = %s\n" (string_of quote !compile);
  Printf.printf "eval      = %B\n" !eval;
  Printf.printf "debug     = %s\n" debug_str;
  Printf.printf "libs      = %s\n" (string_of_path !libs)
;;

if Utils.String.Set.mem "cmdline" !debug then print_opt ();;

let input =
  match !input with
    None | Some "-" -> abort "Source filename missing."
  | Some input -> if   Filename.check_suffix input ".ml"
                 then input
                 else abort "Source file lacks extension .ml."

let compile =
  match !compile with
    None | Some "-" -> !compile
  | Some "" ->
      if   input = "-"
      then abort "OCaml filename (for compilation) missing."
      else Some (Filename.remove_extension input ^ ".ml")
  | Some compile' ->
      if   Filename.check_suffix compile' ".ml"
      then !compile
      else abort "Extension of the OCaml file is not .ml"
;;

match compile with
    None -> ()
| Some _ -> if !eval then abort "Options -e and -c are mutually exclusive."
;;

let out =
  match !output with
    None | Some "-" -> !output
  | Some "" -> Some (if input = "-" then "-"
                    else Filename.remove_extension input ^ ".son")
  | Some out' -> if   Filename.check_suffix out' ".son"
                then !output
                else abort "Extension of output is not .son."
;;

if out <> None && not !eval && compile = None then
  abort "Option --eval or -c required to produce a Michelson file.";;

(* Exporting remaining options as non-mutable values *)

let eval      = !eval
and debug     = !debug
and libs      = !libs
;;

if Utils.String.Set.mem "cmdline" debug then
begin
  Printf.printf "\nEXPORTED COMMAND LINE\n";
  Printf.printf "input     = \"%s\"\n" input;
  Printf.printf "out       = \"%s\"\n" (string_of quote out);
  Printf.printf "compile   = %s\n"     (string_of quote compile);
  Printf.printf "eval      = %B\n"     eval;
  Printf.printf "debug     = %s\n"     debug_str;
  Printf.printf "I         = %s\n"     (string_of_path libs)
end
;;
