#!/bin/bash

rm debug/*.ml
sed -e 's/\(^open .*\)/(* \1 *)/' interprete.ml > debug/interprete.ml
sed -e 's/\(^open .*\)/(* \1 *)/' eval.ml > debug/eval.ml
sed -e 's/\(^open .*\)/(* \1 *)/' syntaxe.ml > debug/syntaxe.ml
cd debug/
ocaml -init ../.ocamlinit