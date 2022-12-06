MAIN	  = main
TESTS_DIR = tests
OUTPUT    = output

TESTS     = $(wildcard $(TESTS_DIR)/*.test)

main:
	ocamlbuild -use-menhir -no-hygiene $(MAIN).byte

test: main
test: $(TESTS)
	@for f in $(TESTS); do \
		./$(MAIN).byte $$f 1> /dev/null 2> $(OUTPUT); \
		if [ -s $(OUTPUT) ]; then \
			printf "%s : ❌ => " $$f; \
			cat $(OUTPUT); \
		else \
			printf "%s : 🎉\n" $$f; \
		fi; \
	done; \
	rm $(OUTPUT)

clean:
	rm -r *.cmi $(MAIN).byte _build/

archive:
	tar czf "IC.tar.gz" *.ml *.mly *.mll Makefile $(TESTS_DIR)

symlinks:
	ln -s _build/*.cmi .
