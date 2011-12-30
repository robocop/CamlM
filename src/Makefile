BUILD=../build
EXEC=../
CC=ocamlc -I $(BUILD)

all: caml

caml: lexer.mll parser.mly syntaxe.ml  eval.ml interprete.ml
	$(CC) -o $(BUILD)/syntaxe.cmo -c syntaxe.ml
	ocamllex -o $(BUILD)/lexer.ml lexer.mll 
	menhir --ocamlc "$(CC)" --infer --base $(BUILD)/parser parser.mly
	cd $(BUILD); $(CC) -o parser.cmi -c parser.mli
	cd $(BUILD); $(CC) -o lexer.cmo -c lexer.ml
	cd $(BUILD); $(CC) -o parser.cmo -c parser.ml
	$(CC) -o $(BUILD)/eval.cmo -c eval.ml
	$(CC) -o $(BUILD)/interprete.cmo -c interprete.ml
	cd $(BUILD); $(CC) -o caml syntaxe.cmo lexer.cmo parser.cmo eval.cmo interprete.cmo
	mv $(BUILD)/caml $(EXEC)

clean:
	rm -f $(BUILD)/* $(EXEC)/caml
