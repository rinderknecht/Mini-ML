let gen_sym =
  let counter = ref 0 in
  fun () -> incr counter; "v" ^ string_of_int !counter

let swap f x y = f y x
