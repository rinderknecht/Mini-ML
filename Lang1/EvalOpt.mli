(* Command-line options for the Mini-ML compiler/interpreter *)

val input:     string
val eval:      bool
val compile:   string option
val out:       string option

val debug:     Utils.String.Set.t
val libs:      string list
