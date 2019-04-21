#!/bin/sh
set -x
ocamllex.opt Edit.mll
ocamllex.opt Lexer.mll
menhir -la 1 --explain --external-tokens Token --base Parser ParToken.mly Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Loc.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Pos.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Utils.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Edit.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c Token.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Region.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c EvalOpt.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Lexer.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c AST.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c Eval.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c Parser.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Compile.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -w -58 -c EvalMain.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Loc.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Partition.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Utils.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Pos.ml
ocamlfind ocamlc -I /home/rinderkn/.opam/4.07.0/lib/ocaml -c stubs.c 
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Region.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -w -4 -package zarith -c AST.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c Token.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Version.ml
camlcmd="ocamlfind ocamlc -I _x86_64 -strict-sequence -w +A-48-4 -w -4  -package zarith"
ocamlfind ocamlc -strict-sequence -w +A-48-4 -w -42 -package zarith -c Eval.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Compile.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package zarith -c Lexer.ml
menhir --infer --ocamlc="$camlcmd" --explain --external-tokens Token --base Parser ParToken.mly Parser.mly
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Partition3.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -package getopt,str -c EvalOpt.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Edit.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -w -4 -package zarith -c Parser.ml
ocamlfind ocamlc -package getopt,str,zarith -linkpkg -o EvalMain.byte Loc.cmo Pos.cmo Region.cmo Utils.cmo AST.cmo Partition3.cmo Edit.cmo Compile.cmo Eval.cmo Version.cmo EvalOpt.cmo Token.cmo Lexer.cmo Parser.cmo EvalMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c LexerMain.ml
ocamlfind ocamlc -g -custom -cclib stubs.o -package getopt,str,zarith -linkpkg -o LexerMain.byte Utils.cmo Version.cmo EvalOpt.cmo Pos.cmo Loc.cmo Region.cmo Token.cmo Lexer.cmo LexerMain.cmo
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c ParserMain.ml
ocamlfind ocamlc -g -custom -cclib stubs.o -package getopt,str,zarith -linkpkg -o ParserMain.byte Loc.cmo Pos.cmo Region.cmo Utils.cmo AST.cmo Version.cmo EvalOpt.cmo Token.cmo Lexer.cmo Parser.cmo ParserMain.cmo
