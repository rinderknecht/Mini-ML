(* Parsing the command-line options for the STICK compiler *)

let abort msg =
  Utils.highlight (Printf.sprintf "Command-line error: %s" msg); exit 1

(* Help *)

let help () =
  let file = Filename.basename Sys.argv.(0) in
  Printf.printf "Usage: %s [<option> ...] <input>.sl\n" file;
  print_endline "where <input>.sl is the Slang source file,";
  print_endline "and each <option> (if any) is one of the following:";
  print_endline "  -I <paths>                Library paths (colon-separated)";
  print_endline "  -o, --net=<netlist>.json  Output netlist (default \
                                             <input>.json)";
  print_endline "  -s, --stats=<stats>.json  Network statistics (default \
                                             <input>.stats.json)";
  print_endline "  -D, --dot=<graph>.dot     Network in GraphViZ Dot";
  print_endline "  -i, --infer=<file>.ml     OCaml for static type inference";
  print_endline "  -c <file>.ml              Compile to OCaml";
  print_endline "  -e, --eval                Evaluate <input>.sl";
  print_endline "      --no-stdlib           If -i or -c, do not add `open Prelude' to \
                                             the OCaml file";
  print_endline "      --cpp=<def>           -D<def> option for Cpp";
  print_endline "      --pretty              Pretty-print of JSON outputs";
  print_endline "      --no-labels           No labels in GraphViZ Dot";
  print_endline "  -l, --log=<file>          Log of the network build (stdout \
                                             if '-')";
  print_endline "      --rte                 If -c <file>.ml, generate a runtime <input>RTE.ml in the same directory, else nothing";
  print_endline "      --tco                 More tail-call optimisations";
  print_endline "  -r, --raw-edits           No optimisation of edits";
  print_endline "  -d, --debug=<target>      cmdline, cpp, lexer, infer, \
                                             compiler, editor,";
  print_endline "                            parser, unparsing, norm, eval";
  print_endline "  -v, --version             Print the version number on \
                                             stdout and exit";
  print_endline "  -h, --help                This help";
  exit 0

(* Version *)

let version () = Printf.printf "%s\n" Version.version; exit 0

(* Specifying the command-line options a la GNU *)

let input     = ref None
and net       = ref None
and stats     = ref None
and dot       = ref None
and log       = ref None
and infer     = ref None
and cpp       = ref None
and compile   = ref None
and debug     = ref Utils.String.Set.empty
and eval      = ref false
and raw_edits = ref false
and pretty    = ref false
and no_stdlib = ref false
and labels    = ref true
and rte       = ref false
and tco       = ref false
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
    'I',     nolong,      None, Some add_path;
    'o',     "net",       set net (Some ""),
                          set_opt net "Multiple source files";
    's',     "stats",     set stats (Some ""),
                          set_opt stats "Multiple statistics";
    'D',     "dot",       set dot (Some ""),
                          set_opt dot "Multiple dot files";
    'l',     "log",       set log (Some ""),
                          set_opt log "Multiple log files";
    'i',     "infer",     set infer (Some ""),
                          set_opt infer "Multiple inference files";
    'c',     nolong,      set compile (Some ""),
                          set_opt compile "Multiple OCaml outputs";
    noshort, "cpp",       None,
                          set_opt cpp "Multiple cpp definitions";
        'e', "eval",      set eval true, None;
    noshort, "pretty",    set pretty true, None;
    noshort, "rte",       set rte true, None;
    noshort, "tco",       set tco true, None;
    noshort, "no-stdlib", set no_stdlib true, None;
    noshort, "no-labels", set labels false, None;
    'd',     "debug",     None, Some add_debug;
    'r',     "raw-edits", set raw_edits true, None;
    'h',     "help",      Some help, None;
    'v',     "version",   Some version, None;
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
  Printf.printf "net       = %s\n" (string_of quote !net);
  Printf.printf "stats     = %s\n" (string_of quote !stats);
  Printf.printf "dot       = %s\n" (string_of quote !dot);
  Printf.printf "log       = %s\n" (string_of quote !log);
  Printf.printf "infer     = %s\n" (string_of quote !infer);
  Printf.printf "compile   = %s\n" (string_of quote !compile);
  Printf.printf "eval      = %B\n" !eval;
  Printf.printf "debug     = %s\n" debug_str;
  Printf.printf "raw_edits = %b\n" !raw_edits;
  Printf.printf "rte       = %B\n" !rte;
  Printf.printf "tco       = %B\n" !tco;
  Printf.printf "no-labels = %B\n" (not !labels);
  Printf.printf "libs      = %s\n" (string_of_path !libs)
;;

if Utils.String.Set.mem "cmdline" !debug then print_opt ();;

let input =
  match !input with
    None | Some "-" -> abort "Source filename missing."
  | Some input -> if Filename.check_suffix input ".sl"
                 then input
                 else abort "Source file lacks extension .sl."

let check_output_name name =
  let modname =
    String.capitalize_ascii (Filename.remove_extension name) in
  match modname with
    "Net" | "Zipper" | "Neuron" | "JSON" -> abort "Invalid output OCaml filename."
  | _ -> name

let compile =
  match !compile with
    None | Some "-" ->
      let _ = check_output_name Filename.(remove_extension (basename input))
      in !compile
  | Some "" ->
      if   input = "-"
      then abort "OCaml filename (for compilation) missing."
      else Some (Filename.remove_extension input ^ ".ml")
  | Some compile ->
      if   Filename.check_suffix compile ".ml"
      then Some (check_output_name compile)
      else abort "Extension of the OCaml file is not .ml"
;;

match compile with
    None -> ()
| Some _ -> if !eval then abort "Options -e and -c are mutually exclusive."
;;

let infer =
  match !infer with
    None | Some "-" -> !infer
  | Some "" ->
      if   input = "-"
      then abort "OCaml filename (for type inference) missing."
      else Some (Filename.remove_extension input ^ ".ml")
  | Some infer ->
      if   Filename.check_suffix infer ".ml"
      then Some infer
      else abort "Extension of the inference file is not .ml"
;;

match infer with
    None -> ()
| Some _ -> if compile <> None then
           abort "Options --infer and -c are mutually exclusive."
;;

let net =
  match !net with
    None | Some "-"  -> !net
  | Some "" -> Some (if input = "-" then "-"
                    else Filename.remove_extension input ^ ".json")
  | Some net' -> if   Filename.check_suffix net' ".json"
                then Some net'
                else abort "Extension of netlist is not .json."
;;

if net <> None && not !eval && compile = None then
  abort "Option --eval or -c required to produce a net list.";;

let stats =
  match !stats with
    None | Some "-" -> !stats
  | Some "" ->
      if   input = "-"
      then abort "Statistics filename missing."
      else Some (Filename.remove_extension input ^ ".stats.json")
  | Some stats' ->
      if   Filename.check_suffix stats' ".json"
      then Some stats'
      else abort "Extension of stats is not .json."
;;

match stats with
    None -> ()
| Some _ -> if not !eval && compile = None
            && infer = None (* TEMPORARY *) then
           abort "Option --eval or -c required to produce statistics."
;;

let dot =
  match !dot with
    None | Some "-" -> !dot
  | Some "" ->
      if   input = "-"
      then abort "Dot filename missing."
      else Some (Filename.remove_extension input ^ ".dot")
  | Some dot' ->
      if Filename.check_suffix dot' ".dot"
      then Some dot'
      else abort "Extension of the dot file is not .dot."
;;

match dot with
    None -> ()
| Some _ -> if not !eval && compile = None then
           abort "Option --eval or -c required to produce a .dot file."
;;

let log =
  match !log with
    None | Some "-" -> !log
  | Some "" ->
      if   input = "-"
      then abort "Log filename missing."
      else Some (Filename.remove_extension input ^ ".log")
  | Some log' -> Some log'

(* Exporting options as non-mutable values *)

let eval      = !eval
and cpp       = !cpp

and debug     = !debug
and raw_edits = !raw_edits
and pretty    = !pretty
and rte       = !rte
and tco       = !tco
and labels    = !labels
and no_stdlib = !no_stdlib

and libs      = !libs
;;

if Utils.String.Set.mem "cmdline" debug then
begin
  Printf.printf "\nEXPORTED COMMAND LINE\n";
  Printf.printf "input     = \"%s\"\n" input;
  Printf.printf "net       = \"%s\"\n" (string_of quote net);
  Printf.printf "stats     = %s\n"     (string_of quote stats);
  Printf.printf "dot       = %s\n"     (string_of quote dot);
  Printf.printf "log       = %s\n"     (string_of quote log);
  Printf.printf "infer     = %s\n"     (string_of quote infer);
  Printf.printf "compile   = %s\n"     (string_of quote compile);
  Printf.printf "cpp       = %s\n"     (string_of quote cpp);
  Printf.printf "eval      = %B\n"     eval;
  Printf.printf "debug     = %s\n"     debug_str;
  Printf.printf "pretty    = %B\n"     pretty;
  Printf.printf "raw_edits = %B\n"     raw_edits;
  Printf.printf "rte       = %B\n"     rte;
  Printf.printf "tco       = %B\n"     tco;
  Printf.printf "no-labels = %B\n"     (not labels);
  Printf.printf "no_stdlib = %B\n"     no_stdlib;
  Printf.printf "I         = %s\n"     (string_of_path libs)
end;;
