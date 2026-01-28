open Utils

type url_parsing_state = ParsingKey | ParsingValue

(** [parse_url_dictionary url_data] Converts a url-encoded dictionary to a [StringMap].
  Example: [parse_url_dictionary "key1=val1&key2=val2&key3=val3"] returns a map associating [keyi] to [vali] *)
let parse_url_dictionary (url_data : string) : string StringMap.t =
  let (_, _, _, final_map) = String.fold_left begin fun (state, key_acc, value_acc, map_acc) c -> match state with
      | ParsingValue -> if c = '&' then
          let key = string_of_char_list (List.rev key_acc) in
          let value = string_of_char_list (List.rev value_acc) in
          (ParsingKey, [], [], StringMap.add key value map_acc)
        else
          (ParsingValue, key_acc, c :: value_acc, map_acc)
      | ParsingKey -> if c = '=' then
          (ParsingValue, key_acc, value_acc, map_acc)
        else
          (ParsingKey, c :: key_acc, value_acc, map_acc)
    end (ParsingKey, [], [], StringMap.empty) (url_data ^ "&") (* FIXME this could be better *)
  in
  final_map