all:
	make caml

top:
	ocamlmktop -o toplevel build/syntaxe.cmo

caml: lexer.mll parser.mly syntaxe.ml eval.ml interprete.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c syntaxe.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c eval.ml
	ocamlc -c interprete.ml
	ocamlc -o caml syntaxe.cmo lexer.cmo parser.cmo eval.cmo interprete.cmo

	mv lexer.ml build
	mv parser.ml build
	mv parser.mli build
	mv -f *.cmi build
	mv -f *.cmo build
