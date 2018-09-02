let rec even n = if n = 0 then true else odd (n-1)
and odd = fun n -> if n = 0 then false else even (n-1)
in even 13

let x = 4 in x + 1
