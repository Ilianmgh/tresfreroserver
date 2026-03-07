# To compile the project with the native compiler, the two following lines should be:
# 	CC = ocamlfind ocamlopt
# 	OBJ_EXT = cmx
# Otherwise, to compile with the bytecode compiler:
# 	CC = ocamlfind ocamlc
# 	OBJ_EXT = cmo
CC = ocamlfind ocamlc
OBJ_EXT = cmo
CFLAGS =
LDFLAGS = -package sqlite3 -package str -linkpkg

all: produce_page.x parse_url_dictionary.x test.x

.PHONY: clean
# See if ocaml-deps can reduce this horrible makefile
produce_page.x: produce_page.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT) test.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o produce_page.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) test.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT) produce_page.$(OBJ_EXT)

produce_page.cmi: produce_page.$(OBJ_EXT)

produce_page.$(OBJ_EXT): produce_page.ml hierarchic.cmi utils.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi typechecker.cmi typeSyntax.cmi value.cmi interpreter.cmi parse_url_dictionary.cmi test.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o produce_page.$(OBJ_EXT) produce_page.ml

parse_url_dictionary.x: hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o parse_url_dictionary.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT) parse_url_dictionary.$(OBJ_EXT)

parse_url_dictionary.cmi: parse_url_dictionary.$(OBJ_EXT)

parse_url_dictionary.$(OBJ_EXT): hierarchic.cmi utils.cmi syntax.cmi value.cmi parse_url_dictionary.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parse_url_dictionary.$(OBJ_EXT) parse_url_dictionary.ml

test.x: test.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o test.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT) test.$(OBJ_EXT)

test.cmi: test.$(OBJ_EXT)

test.$(OBJ_EXT): test.ml hierarchic.cmi utils.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi typechecker.cmi typeSyntax.cmi value.cmi interpreter.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o test.$(OBJ_EXT) test.ml

interpreter.x: interpreter.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o interpreter.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT) value.$(OBJ_EXT) interpreter.$(OBJ_EXT)

interpreter.cmi: interpreter.$(OBJ_EXT)

interpreter.$(OBJ_EXT): interpreter.ml hierarchic.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi parser.cmi typechecker.cmi syntax.cmi typeSyntax.cmi value.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o interpreter.$(OBJ_EXT) interpreter.ml

value.x: value.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o value.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT) value.$(OBJ_EXT)

value.cmi: value.$(OBJ_EXT)

value.$(OBJ_EXT): value.ml syntax.cmi hierarchic.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o value.$(OBJ_EXT) value.ml

typechecker.x: typechecker.$(OBJ_EXT) hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o typechecker.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT) typeSyntax.$(OBJ_EXT) typechecker.$(OBJ_EXT)

typechecker.cmi: typechecker.$(OBJ_EXT)

typechecker.$(OBJ_EXT): typechecker.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi hierarchic.cmi typeSyntax.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typechecker.$(OBJ_EXT) typechecker.ml

typeSyntax.x: lexic.$(OBJ_EXT) hierarchic.$(OBJ_EXT) typeSyntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o typeSyntax.x lexic.$(OBJ_EXT) hierarchic.$(OBJ_EXT) typeSyntax.$(OBJ_EXT)

typeSyntax.cmi: lexic.cmi typeSyntax.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.cmi typeSyntax.mli

typeSyntax.$(OBJ_EXT): lexic.cmi hierarchic.cmi typeSyntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.$(OBJ_EXT) typeSyntax.ml

parser.x: parser.$(OBJ_EXT) utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) utils.$(OBJ_EXT) syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o parser.x utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT) syntax.$(OBJ_EXT) parser.$(OBJ_EXT)

parser.cmi: parser.$(OBJ_EXT)

parser.$(OBJ_EXT): parser.ml lexic.cmi prelexer.cmi lexer.cmi trie.cmi utils.cmi syntax.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parser.$(OBJ_EXT) parser.ml

syntax.x: syntax.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o syntax.x syntax.$(OBJ_EXT)

syntax.cmi: syntax.$(OBJ_EXT)

syntax.$(OBJ_EXT): syntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o syntax.$(OBJ_EXT) syntax.ml

lexer.x: lexer.$(OBJ_EXT) utils.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) trie.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexer.x utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT) lexer.$(OBJ_EXT)

lexer.cmi: lexer.$(OBJ_EXT)

lexer.$(OBJ_EXT): lexer.ml lexic.cmi prelexer.cmi trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexer.$(OBJ_EXT) lexer.ml

prelexer.x: prelexer.$(OBJ_EXT) utils.$(OBJ_EXT) lexic.$(OBJ_EXT) trie.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o prelexer.x utils.$(OBJ_EXT) trie.$(OBJ_EXT) lexic.$(OBJ_EXT) prelexer.$(OBJ_EXT)

prelexer.cmi: prelexer.$(OBJ_EXT)

prelexer.$(OBJ_EXT): prelexer.ml utils.cmi lexic.cmi trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o prelexer.$(OBJ_EXT) prelexer.ml

lexic.x: lexic.$(OBJ_EXT) lexic.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexic.x lexic.$(OBJ_EXT)

lexic.cmi: lexic.$(OBJ_EXT)

lexic.$(OBJ_EXT): lexic.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexic.$(OBJ_EXT) lexic.ml

trie.x: trie.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o trie.x utils.$(OBJ_EXT) trie.$(OBJ_EXT)

trie.$(OBJ_EXT): trie.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.$(OBJ_EXT) trie.ml

trie.cmi: trie.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.cmi trie.mli

utils.x: hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o utils.x hierarchic.$(OBJ_EXT) utils.$(OBJ_EXT)

utils.cmi: utils.$(OBJ_EXT)

utils.$(OBJ_EXT): hierarchic.cmi utils.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o utils.$(OBJ_EXT) utils.ml

hierarchic.$(OBJ_EXT): hierarchic.cmi hierarchic.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o hierarchic.$(OBJ_EXT) hierarchic.ml

hierarchic.cmi: hierarchic.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o hierarchic.cmi hierarchic.mli
clean:
	rm -rf *.$(OBJ_EXT) *.cmi *.x 