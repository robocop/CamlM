BUILD=../build
EXEC=../
CC=ocamlc -I $(BUILD)

all: caml

caml: lexer.mll parser.mly syntax.ml helper.ml error.ml modules.ml typing.ml show.ml lambda_repl.ml eval.ml interpreter.ml 
	$(CC) -o $(BUILD)/syntax.cmo -c syntax.ml
	ocamllex -o $(BUILD)/lexer.ml lexer.mll 
	menhir --ocamlc "$(CC)" --infer --base $(BUILD)/parser parser.mly
	cd $(BUILD); $(CC) -o parser.cmi -c parser.mli
	cd $(BUILD); $(CC) -o lexer.cmo -c lexer.ml
	cd $(BUILD); $(CC) -o parser.cmo -c parser.ml
	$(CC) -o $(BUILD)/error.cmo -c error.ml
	$(CC) -o $(BUILD)/helper.cmo -c helper.ml
	$(CC) -o $(BUILD)/show.cmo -c show.ml
	$(CC) -o $(BUILD)/modules.cmo -c modules.ml
	$(CC) -o $(BUILD)/typing.cmo -c typing.ml
	$(CC) -o $(BUILD)/lambda_repl.cmo -c lambda_repl.ml
	$(CC) -o $(BUILD)/eval.cmo -c eval.ml
	$(CC) -o $(BUILD)/builtin.cmo -c builtin.ml
	$(CC) -o $(BUILD)/interpreter.cmo -c interpreter.ml
	cd $(BUILD); $(CC) -o caml syntax.cmo lexer.cmo parser.cmo error.cmo helper.cmo show.cmo modules.cmo typing.cmo lambda_repl.cmo eval.cmo builtin.cmo interpreter.cmo
	mv $(BUILD)/caml $(EXEC)

clean:
	rm -f $(BUILD)/* $(EXEC)/caml
