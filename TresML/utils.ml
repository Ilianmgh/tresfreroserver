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

(** [split_on_last_char cs s i = (s1, s2)] where [s1 ++ "c" ++ s2 = s] and there are no occurences of [c] in the string [s2] between indices [0] and [i - String.length s1 - 1]. I.e. it finds the last occurence of character [c] in [s] before index [i] and splits the string on this last occurence.
  E.g. [split_on_last_char '/' "path/to/file" 9 = ("path/to", "file")] *)
let rec split_on_last_char (c : char) (s : string) (i : int) =
  if i = -1 then
    ("", s)
  else if s.[i] = c then
    (String.sub s 0 i, String.sub s (i + 1) (String.length s - (i + 1)))
  else
    split_on_last_char c s (i - 1)

(** [split_on_last_one_of_chars cs s i = (s1, s2)] where [s1 ++ "c" ++ s2 = s] and there are no occurences of any chars of list [cs] in the string [s2] between indices [0] and [i - String.length s1 - 1]. I.e. it finds the last occurence of a char of [cs] in [s] before index [i] and splits the string on this last occurence.
  E.g. [split_on_last_one_of_chars ['/'; '\\'] "win\\path/to/a\\file" 16 = ("win\\path/to/a", "file")] *)
let rec split_on_last_one_of_chars (cs : char list) (s : string) (i : int) =
  if i = -1 then
    ("", s)
  else if List.mem s.[i] cs then
    (String.sub s 0 i, String.sub s (i + 1) (String.length s - (i + 1)))
  else
    split_on_last_one_of_chars cs s (i - 1)

(** FILE SYSTEM MANAGEMENT *)

(** If [path] is a file system path, [split_parent_dir_and_file path = (path_parent, last_file_or_dir)] where [path_parent] is path to the parent directory of [path] and [last_file_or_dir] is the farthest directory or file of [path].
  E.g. [split_parent_dir_and_file "C:\\Users/ocaml\\aFolder/test.ml" = ("C:\\Users/ocaml\\aFolder", "test.ml")]
  E.g. [split_parent_dir_and_file "/home/ocaml/aFolder/anotherFolder" = ("/home/ocaml/aFolder", "anotherFolder")] *)
let split_parent_dir_and_file (path : string) : string * string =
  if Sys.os_type = "Win32" then (* directory separator can either be '/' or '\' *)
    split_on_last_one_of_chars ['/'; '\\'] path (String.length path - 1)
  else (* directory separator has to be '/' *)
    split_on_last_char '/' path (String.length path - 1)

(** [get_parent_dir_of_path path = path_parent] where [path_parent] is a file system path to the parent directory of [path] (if it's itself a file system path). *)
let parent_dir_of_path (path : string) : string = fst (split_parent_dir_and_file path)

let filename_of_path (path : string) : string = snd (split_parent_dir_and_file path)

(** [is_subfolder path = true] iff [path] is a relative path (from current working directory) that does not contain [..].
  If [true], ensures that ["path/to/folder/" ^ path] is located in a subfolder of ["path/to/folder"] *)
let is_subfolder (path : string) : bool =
  if Sys.os_type = "Win32" then (* directory separator can either be '/' or '\' *)
    if path.[0] = '\\' || path.[0] = '/' then (* is a "relative" path from root of current drive *)
      false
    else
      let split_on_backslash = String.split_on_char '\\' path in
      let split_on_backslash_and_slash = List.concat_map (fun path_slice -> String.split_on_char '\\' path_slice) split_on_backslash in
      match String.index_opt (List.hd split_on_backslash_and_slash) ':' with
        | None -> not (List.mem ".." split_on_backslash_and_slash)
        | Some _ -> false(* is an absolute path (first element of path is a volume or drive) *)
  else begin
    (* directory separator has to be '/' *)
    if path.[0] = '/' then (* is an absolute path *)
      false
    else
      not (List.mem ".." (String.split_on_char '/' path))
  end

(** If [path] is a path to a directory, [separator_ended_dir_path_opt path = Some path'] where [path'] is a path to the same directory, but it ends by a directory separator character ['/'] or ['\\'] if running system is Windows.
  If [path] is not a directory, [separator_ended_dir_path_opt = None]. *)
let separator_ended_dir_path_opt (path : string) : string option =
  if Sys.is_directory path then begin
    let last_char_of_path = path.[String.length path - 1] in
    if last_char_of_path = '/' || (Sys.os_type = "Win32" && last_char_of_path = '\\') then
      Some path
    else
      Some (path ^ "/")
  end else
    None