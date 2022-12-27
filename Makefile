MAIN	  = main
TESTS_DIR = tests
OUTPUT    = output

TESTS     = $(wildcard $(TESTS_DIR)/*.test)

INDENT    = 38

main:
	ocamlbuild -use-menhir -no-hygiene $(MAIN).byte

test: main
test: $(TESTS)
	@for f in $(TESTS); do \
		./$(MAIN).byte $$f 1> /dev/null 2> $(OUTPUT); \
		if [ -s $(OUTPUT) ]; then \
			printf "%$(INDENT)s : ❌ => " $$f; \
			cat $(OUTPUT); \
		else \
			printf "%$(INDENT)s : 🎉\n" $$f; \
		fi; \
	done; \
	rm $(OUTPUT)

clean:
	rm -r *.cmi $(MAIN).byte _build/ *.conflicts parser.ml parser.mli

archive:
	tar czf "IC.tar.gz" *.ml *.mly *.mll Makefile $(TESTS_DIR) README.md

symlinks:
	ln -s _build/*.cmi .

menhir-debug:
	menhir parser.mly --explain --infer
