(* Command-line options for the STICK compiler *)

val input:     string
val net:       string option
val stats:     string option
val dot:       string option
val log:       string option
val infer:     string option
val cpp:       string option
val compile:   string option

val debug:     Utils.String.Set.t
val eval:      bool
val raw_edits: bool
val pretty:    bool
val labels:    bool
val no_stdlib: bool
val rte:       bool
val tco:       bool

val libs: string list
