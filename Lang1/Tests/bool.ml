let rec all_true l =
  match l with
    [] -> true
  | true::l -> all_true l
  | false::_ -> false
  end

let b = [true; false; true]

let () = print_string (string_of_bool (all_true b) ^ "\n")
