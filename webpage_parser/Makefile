CC = ocamlc
CFLAGS =
LDFLAGS =

all: lexer.x prelexer.x

.PHONY: clean

lexer.x: lexer.cmo trie.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o lexer.x utils.cmo trie.cmo lexer.cmo

lexer.cmo: lexer.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o lexer.cmo lexer.ml

prelexer.x: prelexer.cmo trie.cmo utils.cmo
	$(CC) $(CFLAGS) $(LDFLAGS) -o prelexer.x utils.cmo trie.cmo prelexer.cmo

prelexer.cmo: prelexer.ml trie.cmi utils.cmi
	$(CC) $(CFLAGS) $(LDFLAGS) -c -o prelexer.cmo prelexer.ml

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