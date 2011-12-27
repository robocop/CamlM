BUILD=../build
EXEC=../
CC=ocamlc -I $(BUILD)

all: caml

caml: lexer.mll parser.mly syntaxe.ml valeur.ml eval_helper.ml eval.ml formel.ml interprete.ml
	ocamllex -o $(BUILD)/lexer.ml lexer.mll 
	menhir --base $(BUILD)/parser parser.mly
	$(CC) -o $(BUILD)/syntaxe.cmo -c syntaxe.ml
	cd $(BUILD); $(CC) -o parser.cmi -c parser.mli
	cd $(BUILD); $(CC) -o lexer.cmo -c lexer.ml
	cd $(BUILD); $(CC) -o parser.cmo -c parser.ml
	$(CC) -o $(BUILD)/valeur.cmo -c valeur.ml
	$(CC) -o $(BUILD)/eval_helper.cmo -c eval_helper.ml
	$(CC) -o $(BUILD)/eval.cmo -c eval.ml
	$(CC) -o $(BUILD)/formel.cmo -c formel.ml
	$(CC) -o $(BUILD)/interprete.cmo -c interprete.ml
	cd $(BUILD); $(CC) -o caml syntaxe.cmo lexer.cmo parser.cmo valeur.cmo eval_helper.cmo eval.cmo formel.cmo interprete.cmo
	mv $(BUILD)/caml $(EXEC)

clean:
	rm -f $(BUILD)/* $(EXEC)/caml
