let mult x y = x * y
let rec fact n = if n = 0 then 1 else mult n (fact (n-1))
let n = (fun x -> x + 1) (fact 5)
let () = print_int n
