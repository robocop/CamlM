#!/bin/bash
make
ocamlmktop -o toplevel build/parser.cmo build/eval.cmo
