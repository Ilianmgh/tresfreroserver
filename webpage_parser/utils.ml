module StringMap = Map.Make(String)

module Environment = Hierarchic.Make(StringMap)

exception UnsupportedError of string

let unpack_some (x : 'a option) (default : 'a) : 'a = match x with
  | Some x -> x
  | None -> default

let unpack_some_func (x : 'a option) (default : unit -> 'a) : 'a = match x with
  | Some x -> x
  | None -> default ()

let list_of_string (s : string) : char list = String.fold_right (fun x acc -> x :: acc) s []

let string_of_char (c : char) : string = String.init 1 (fun _ -> c)

let string_of_list (to_string : 'a -> string) (l : 'a list) : string =
  let rec string_of_list_aux (to_string : 'a -> string) (l : 'a list) : string = match l with
    | [] -> "]"
    | [x] -> Printf.sprintf "%s]" (to_string x)
    | h :: t -> Printf.sprintf "%s;%s" (to_string h) (string_of_list_aux to_string t)
  in
  Printf.sprintf "[%s" (string_of_list_aux to_string l)

(** Takes [['a'; ' '; 's'; 't'; 'r'; 'i'; 'n'; 'g']] and returns ["a string"]. *)
let rec string_of_char_list (l : char list) : string = match l with
  | [] -> ""
  | h :: t -> Printf.sprintf "%c%s" h (string_of_char_list t)

let string_fold_lefti (f : 'a -> int * char -> 'a) (acc : 'a) (s : string) : 'a = List.fold_left f acc (List.mapi (fun i x -> (i, x)) (list_of_string s))