let rec cat l1 l2 =
  match l1 with
      [] -> l2
  | h::t -> h :: cat t l2
  end

let a = cat [1;2] [3;4;5]

let rec print_list l =
  match l with
    [] -> ()
  | [n] -> print_int n
  | n::l -> let () = print_int n in
             let () = print_string "; "
             in print_list l
  end

let () = print_list a
let () = print_string "\n"

