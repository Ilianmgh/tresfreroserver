CC = ocamlc
CFLAGS =
LDFLAGS =

all: parser.x

.PHONY: clean

interpreter.x: interpreter.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo parser.cmo typechecker.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o interpreter.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo interpreter.cmo

interpreter.cmi: interpreter.cmo

interpreter.cmo: interpreter.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi parser.cmi typechecker.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o interpreter.cmo interpreter.ml

typechecker.x: typechecker.cmo utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo parser.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o typechecker.x utils.cmo trie.cmo lexic.cmo prelexer.cmo lexer.cmo typechecker.cmo

typechecker.cmi: typechecker.cmo

typechecker.cmo: typechecker.ml lexic.cmi trie.cmi prelexer.cmi lexer.cmi parser.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o typechecker.cmo typechecker.ml

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