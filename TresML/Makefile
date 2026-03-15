# To compile the project with the native compiler, the three following lines should be:
# 	CC = ocamlfind ocamlopt # To explictly execute command within the opam switch, you can also replace by: CC = opam exec -- ocamlfind ocamlopt
# 	OBJ_EXT = cmx
# 	CFLAGS = -opaque
# Otherwise, to compile with the bytecode compiler:
# 	CC = ocamlfind ocamlc # To explictly execute command within the opam switch, you can also replace by: CC = opam exec -- ocamlfind ocamlc
# 	OBJ_EXT = cmo
# 	CFLAGS =
CC = ocamlfind ocamlc
OBJ_EXT = cmo
CFLAGS =
EXE_EXT = x
LDFLAGS = -package sqlite3 -package str -linkpkg

all: produce_page.$(EXE_EXT) test.$(EXE_EXT)

.PHONY: clean
# See if ocaml-deps can reduce this horrible makefile
produce_page.$(EXE_EXT): produce_page.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) symbols.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o produce_page.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) symbols.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT) produce_page.$(OBJ_EXT)

produce_page.cmi: produce_page.$(OBJ_EXT)

produce_page.$(OBJ_EXT): produce_page.ml hierarchic.cmi utils.cmi symbols.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi linker.cmi typechecker.cmi typeSyntax.cmi value.cmi interpreter.cmi parse_url_dictionary.cmi 
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o produce_page.$(OBJ_EXT) produce_page.ml

parse_url_dictionary.$(EXE_EXT): hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o parse_url_dictionary.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT)

parse_url_dictionary.cmi: parse_url_dictionary.$(OBJ_EXT)

parse_url_dictionary.$(OBJ_EXT): hierarchic.cmi utils.cmi syntax.cmi value.cmi parse_url_dictionary.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parse_url_dictionary.$(OBJ_EXT) parse_url_dictionary.ml

test.$(EXE_EXT): test.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) symbols.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o test.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) symbols.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) test.$(OBJ_EXT)

test.cmi: test.$(OBJ_EXT)

test.$(OBJ_EXT): test.ml hierarchic.cmi utils.cmi symbols.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi linker.cmi typechecker.cmi typeSyntax.cmi value.cmi interpreter.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o test.$(OBJ_EXT) test.ml

interpreter.$(EXE_EXT): interpreter.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o interpreter.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT)

interpreter.cmi: interpreter.$(OBJ_EXT)

interpreter.$(OBJ_EXT): interpreter.ml hierarchic.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi parser.cmi typechecker.cmi syntax.cmi typeSyntax.cmi value.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o interpreter.$(OBJ_EXT) interpreter.ml

value.$(EXE_EXT): value.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o value.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT)

value.cmi: value.$(OBJ_EXT)

value.$(OBJ_EXT): value.ml syntax.cmi hierarchic.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o value.$(OBJ_EXT) value.ml

typechecker.$(EXE_EXT): typechecker.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o typechecker.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT)

typechecker.cmi: typechecker.$(OBJ_EXT)

typechecker.$(OBJ_EXT): typechecker.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi hierarchic.cmi typeSyntax.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typechecker.$(OBJ_EXT) typechecker.ml

typeSyntax.$(EXE_EXT): lexic.$(OBJ_EXT) hierarchic.$(OBJ_EXT) typeSyntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o typeSyntax.$(EXE_EXT) lexic.$(OBJ_EXT) hierarchic.$(OBJ_EXT) typeSyntax.$(OBJ_EXT)

typeSyntax.cmi: lexic.cmi typeSyntax.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.cmi typeSyntax.mli

typeSyntax.$(OBJ_EXT): lexic.cmi hierarchic.cmi typeSyntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.$(OBJ_EXT) typeSyntax.ml

linker.$(EXE_EXT): hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o linker.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) linker.$(OBJ_EXT)

linker.cmi: linker.$(OBJ_EXT)

linker.$(OBJ_EXT): hierarchic.cmi utils.cmi trie.cmi lexic.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi linker.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o linker.$(OBJ_EXT) linker.ml

parser.$(EXE_EXT): parser.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o parser.$(EXE_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT)

parser.cmi: parser.$(OBJ_EXT)

parser.$(OBJ_EXT): parser.ml lexic.cmi prelexer.cmi lexer.cmi trie.cmi utils.cmi syntax.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parser.$(OBJ_EXT) parser.ml

syntax.$(EXE_EXT): syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o syntax.$(EXE_EXT) syntax.$(OBJ_EXT)

syntax.cmi: syntax.$(OBJ_EXT)

syntax.$(OBJ_EXT): syntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o syntax.$(OBJ_EXT) syntax.ml

lexer.$(EXE_EXT): lexer.$(OBJ_EXT) utils.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) trie.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexer.$(EXE_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT)

lexer.cmi: lexer.$(OBJ_EXT)

lexer.$(OBJ_EXT): lexer.ml lexic.cmi prelexer.cmi trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexer.$(OBJ_EXT) lexer.ml

prelexer.$(EXE_EXT): prelexer.$(OBJ_EXT) utils.$(OBJ_EXT) lexic.$(OBJ_EXT) trie.$(OBJ_EXT) symbols.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o prelexer.$(EXE_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) symbols.$(OBJ_EXT) prelexer.$(OBJ_EXT)

prelexer.cmi: prelexer.$(OBJ_EXT)

prelexer.$(OBJ_EXT): prelexer.ml utils.cmi lexic.cmi trie.cmi symbols.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o prelexer.$(OBJ_EXT) prelexer.ml

lexic.$(EXE_EXT): lexic.$(OBJ_EXT) lexic.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexic.$(EXE_EXT) lexic.$(OBJ_EXT)

lexic.cmi: lexic.$(OBJ_EXT)

lexic.$(OBJ_EXT): lexic.ml trie.cmi utils.cmi symbols.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexic.$(OBJ_EXT) lexic.ml

symbols.$(EXE_EXT): utils.$(OBJ_EXT) symbols.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o symbols.$(EXE_EXT) utils.$(OBJ_EXT) symbols.$(OBJ_EXT)

symbols.cmi: symbols.$(OBJ_EXT)

symbols.$(OBJ_EXT): symbols.ml utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o symbols.$(OBJ_EXT) symbols.ml

trie.$(EXE_EXT): trie.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o trie.$(EXE_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT)

trie.$(OBJ_EXT): trie.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.$(OBJ_EXT) trie.ml

trie.cmi: trie.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.cmi trie.mli

utils.$(EXE_EXT): hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o utils.$(EXE_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT)

utils.cmi: utils.$(OBJ_EXT)

utils.$(OBJ_EXT): hierarchic.cmi utils.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o utils.$(OBJ_EXT) utils.ml

hierarchic.$(OBJ_EXT): hierarchic.cmi hierarchic.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o hierarchic.$(OBJ_EXT) hierarchic.ml

hierarchic.cmi: hierarchic.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o hierarchic.cmi hierarchic.mli
clean:
	rm -rf *.$(OBJ_EXT) *.o *.cmi *.$(EXE_EXT) 
