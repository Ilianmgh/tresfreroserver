CC = ocamlfind ocamlc
CFLAGS =
LDFLAGS = -package sqlite3 -linkpkg

all: produce_page.x parse_url_dictionary.x test.x

.PHONY: clean
# Voir ocaml-deps
produce_page.x: produce_page.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo typechecker.cmo interpreter.cmo parse_url_dictionary.cmo test.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o produce_page.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo typechecker.cmo interpreter.cmo test.cmo parse_url_dictionary.cmo produce_page.cmo

produce_page.cmi: produce_page.cmo

produce_page.cmo: produce_page.ml utils.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi typechecker.cmi typeSyntax.cmi interpreter.cmi parse_url_dictionary.cmi test.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o produce_page.cmo produce_page.ml

parse_url_dictionary.x: utils.cmo parse_url_dictionary.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o parse_url_dictionary.x utils.cmo parse_url_dictionary.cmo

parse_url_dictionary.cmi: parse_url_dictionary.cmo

parse_url_dictionary.cmo: utils.cmi parse_url_dictionary.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parse_url_dictionary.cmo parse_url_dictionary.ml

test.x: test.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo typechecker.cmo interpreter.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o test.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo typechecker.cmo interpreter.cmo test.cmo

test.cmi: test.cmo

test.cmo: test.ml utils.cmi lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi typechecker.cmi typeSyntax.cmi interpreter.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o test.cmo test.ml

interpreter.x: interpreter.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo typeSyntax.cmo typechecker.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o interpreter.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo typeSyntax.cmo typechecker.cmo interpreter.cmo

interpreter.cmi: interpreter.cmo

interpreter.cmo: interpreter.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi parser.cmi typechecker.cmi syntax.cmi typeSyntax.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o interpreter.cmo interpreter.ml

typechecker.x: typechecker.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o typechecker.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo typeSyntax.cmo typechecker.cmo

typechecker.cmi: typechecker.cmo

typechecker.cmo: typechecker.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi syntax.cmi parser.cmi typeSyntax.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typechecker.cmo typechecker.ml

typeSyntax.x: lexic.cmo typeSyntax.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o typeSyntax.x lexic.cmo typeSyntax.cmo

typeSyntax.cmi: lexic.cmi typeSyntax.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.cmi typeSyntax.mli

typeSyntax.cmo: lexic.cmi typeSyntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typeSyntax.cmo typeSyntax.ml

parser.x: parser.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo utils.cmo syntax.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o parser.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo syntax.cmo parser.cmo

parser.cmi: parser.cmo

parser.cmo: parser.ml lexic.cmi prelexer.cmi lexer.cmi trie.cmi utils.cmi syntax.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o parser.cmo parser.ml

syntax.x: syntax.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o syntax.x syntax.cmo

syntax.cmi: syntax.cmo

syntax.cmo: syntax.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o syntax.cmo syntax.ml

lexer.x: lexer.cmo utils.cmo lexic.cmo prelexer.cmo trie.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexer.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo

lexer.cmi: lexer.cmo

lexer.cmo: lexer.ml lexic.cmi prelexer.cmi trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexer.cmo lexer.ml

prelexer.x: prelexer.cmo utils.cmo lexic.cmo trie.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o prelexer.x utils.cmo trie.cmo lexic.cmo prelexer.cmo

prelexer.cmi: prelexer.cmo

prelexer.cmo: prelexer.ml utils.cmi lexic.cmi trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o prelexer.cmo prelexer.ml

lexic.x: lexic.cmo lexic.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexic.x lexic.cmo

lexic.cmi: lexic.cmo

lexic.cmo: lexic.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexic.cmo lexic.ml

trie.x: trie.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o trie.x utils.cmo trie.cmo

trie.cmo: trie.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.cmo trie.ml

trie.cmi: trie.mli
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o trie.cmi trie.mli

utils.x: utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o utils.x utils.cmo

utils.cmi: utils.cmo

utils.cmo: utils.ml
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o utils.cmo utils.ml

clean:
	rm -rf *.cmo *.cmi *.x 