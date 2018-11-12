let rec all_ones l =
  match l with
    [] -> true
  | "one"::l -> all_ones l
  | _::_ -> false
  end

let l = ["one"; "one"; "zero"; "one"]

let () = print_string (string_of_bool (all_ones l) ^ "\n")
