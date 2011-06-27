#!/bin/bash


ocamllex lexer.mll
ocamlyacc parser.mly

ocamlc -c syntaxe.ml

ocamlc -c parser.mli

ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c eval.ml

ocamlc -c interprete.ml

ocamlc -o caml lexer.cmo parser.cmo eval.cmo interprete.cmo

mv lexer.ml build
mv parser.ml build
mv parser.mli build
mv *.cmi build
mv *.cmo build