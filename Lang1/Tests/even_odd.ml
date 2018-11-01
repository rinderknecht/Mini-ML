let rec even n = if n = 0 then true else odd (n-1)
and odd = fun n -> if n = 0 then false else even (n-1)

let () = print_string (string_of_bool (even 13))
