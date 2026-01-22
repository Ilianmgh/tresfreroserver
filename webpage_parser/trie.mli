type trie

val empty : trie

val add_word : trie -> string -> trie

val is_final : trie -> bool

val eat_letter : trie -> char -> trie

val eat_letter_opt : trie -> char -> trie option