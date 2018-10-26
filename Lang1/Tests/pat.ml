let foo, mult = (fun (x,(y,z)) -> x + y * z), (fun (x,y) -> x * y)
let n = foo (1, (mult (2,3), 4))
let () = print_int n
