let mult x y = x * y in
  let rec fact n = if n = 0 then 1 else mult n (fact (n-1))
  in (fun x -> x + 1) (fact 5)
