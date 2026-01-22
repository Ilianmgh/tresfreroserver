open Utils

module CharMap = Map.Make(Char)

type trie = {final : bool ; next : trie CharMap.t}

let empty = {final = false ; next = CharMap.empty}

let add_word (tr : trie) (w : string) : trie =
  let rec add_word_aux (tr : trie) (w : char list) : trie = match w with
    | [] -> {final = true; next = tr.next}
    | h :: t ->
      let current_next_h = unpack_some (CharMap.find_opt h tr.next) {final = false ; next = CharMap.empty} in
      {final = tr.final; next = CharMap.add h (add_word_aux current_next_h t) tr.next}
  in
  add_word_aux tr (list_of_string w)

let is_final (tr : trie) : bool = tr.final

let eat_letter_opt (tr : trie) (c : char) : trie option = CharMap.find_opt c tr.next

let eat_letter (tr : trie) (c : char) : trie = match CharMap.find_opt c tr.next with
  | Some tr' -> tr'
  | None -> raise (Invalid_argument "Character is not defined in this trie")