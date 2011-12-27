TESTS=test_parser.ml

test: 
	make test_suite
	./test_suite
	@echo "\n"

test_suite: $(TESTS) test.ml
	ocamlfind ocamlc -I ../build -o test_suite -package oUnit -linkpkg \
		../build/lexer.ml ../build/parser.ml $(TESTS) test.ml

clean:
	rm -f *.cmi *.cmo test_suite
