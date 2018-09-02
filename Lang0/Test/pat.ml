let foo, mult =
  (fun (x,(y,z)) -> x + y * z), (fun (x,y) -> x * y)
in foo (1, (mult (2,3), 4))
