let mult x y = x * y
let rec fact n = if n = 0 then 1 else mult n (fact (n-1))
let n = (fun x -> x + 1) (fact 5)
let () = print_int n

let () = print_string "\n"

let rec fact' n =
  match n with
    0 -> 1
  | p -> match p with
          1 -> 1
        | _ -> p * fact' (p-1)
        end
  end

let () = print_int (fact' 5)
let () = print_string "\n"
