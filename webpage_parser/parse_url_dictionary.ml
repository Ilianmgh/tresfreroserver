open Utils

type url_parsing_state = ParsingKey | ParsingValue

(** [convert_url_data s] converts string encoded in the url as a proper string.
  E.g. [convert_url_data "a+multiple+word+value" "a multiple word value"]
  TODO see all the other modifications to apply. 
*)
let convert_url_data (s : string) : string = String.map (fun c -> if c = '+' then ' ' else c) s

(** [first_of_split_string s sep] returns [(prefix, postfix)] if [s = prefix ^ "`c`" ^ postfix] where `c` is the first such occurence of [c] in [s]. *)
let first_of_split_string (s : string) (sep : char) : string * string = let split = String.split_on_char sep s in (List.hd split, String.concat "&" (List.tl split)) (* FIXME we can do better with a fold_left *)

(** [parse_url_dictionary url_data] Converts a url-encoded dictionary to a [StringMap].
  Example: [parse_url_dictionary "METHOD&key1=val1&key2=val2&key3=val3"] returns a map associating [method_keyi] to [vali] *)
let parse_url_dictionary (url_data : string) : string StringMap.t =
  let method_of_data, data = first_of_split_string url_data '&' in
  let lower_method_of_data = String.lowercase_ascii method_of_data in
  let (_, _, _, final_map) = String.fold_left begin fun (state, key_acc, value_acc, map_acc) c -> match state with
      | ParsingValue -> if c = '&' then
          let key = Printf.sprintf "%s_%s" lower_method_of_data (string_of_char_list (List.rev key_acc)) in
          let value = convert_url_data (string_of_char_list (List.rev value_acc)) in (* TODO lowercased for some reason *)
          (ParsingKey, [], [], StringMap.add key value map_acc)
        else
          (ParsingValue, key_acc, c :: value_acc, map_acc)
      | ParsingKey -> if c = '=' then
          (ParsingValue, key_acc, value_acc, map_acc)
        else
          (ParsingKey, c :: key_acc, value_acc, map_acc)
    end (ParsingKey, [], [], StringMap.empty) (data ^ "&") (* FIXME this ugly & appended at the end could be better *)
  in
  final_map