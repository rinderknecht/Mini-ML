#!/bin/sh
set -x
ocamllex.opt Lexer.mll
menhir -la 1 --explain --external-tokens Token --base Parser ParToken.mly Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Token.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Lexer.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c LexerMain.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Token.ml
ocamlfind ocamlc -o LexerMain.byte Token.cmo Lexer.cmo LexerMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c AST.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Utils.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Eval.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Parser.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c EvalMain.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c AST.ml
camlcmd="ocamlfind ocamlc -I _x86_64 -strict-sequence -w +A-48-4   "
menhir --infer --ocamlc="$camlcmd" --explain --external-tokens Token --base Parser ParToken.mly Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Parser.ml
ocamlfind ocamlc -o EvalMain.byte AST.cmo Eval.cmo Token.cmo Lexer.cmo Utils.cmo Parser.cmo EvalMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c ParserMain.ml
ocamlfind ocamlc -o ParserMain.byte AST.cmo Token.cmo Lexer.cmo Utils.cmo Parser.cmo ParserMain.cmo
