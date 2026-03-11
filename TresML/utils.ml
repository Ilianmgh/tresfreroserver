module StringMap = Map.Make(String)

module Environment = Hierarchic.Make(StringMap) (* TODO For distinction open/include: add info to link between sub/supmap: a boolean indicating if the link is strong (survives to open) or weak (is broken when opened) *)

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

(** File operations *)

(** [read_whole_file_str f] returns the content of the file [f] (passed as a [in_channel]), as a string *)
let read_whole_file_str (f : in_channel) : string =
  let rec read_all_lines_str (f : in_channel) (acc : string list) : string list =
    try
      read_all_lines_str f ((input_line f) :: acc)
    with
      End_of_file -> acc
  in
  String.concat "\n" (List.rev (read_all_lines_str f []))

(** [split_on_last_char c s i = (s1, s2)] where [s1 ++ "c" ++ s2 = s] with the first occurence of ["c"] starting from [i] downto [0].
  E.g. [split_on_last_char '/' "path/to/file" 9 = ("path/to", "file")] *)
let rec split_on_last_char (c : char) (s : string) (i : int) =
  if i = -1 then
    ("", s)
  else if s.[i] = c then
    (String.sub s 0 i, String.sub s (i + 1) (String.length s - (i + 1)))
  else
    split_on_last_char c s (i - 1)