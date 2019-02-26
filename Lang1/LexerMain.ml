(* Driver for the lexer of Mini-ML *)

(* Error printing and exception tracing *)

Printexc.record_backtrace true;;

(* Running the lexer on the source *)

Lexer.trace EvalOpt.input;;
