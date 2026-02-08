open Lexic (* TODO put StringMap in utils *)
open Syntax

type type_variable = string

type ml_type =
  | Arr of ml_type * ml_type
  | Prod of ml_type * ml_type
  | TypeInt | TypeBool | TypeString | TypeUnit
  | TypeDb
  | TypeHtml
  | TypeForall of type_variable * ml_type
  | TypeVar of type_variable

type typing_environment = ml_type StringMap.t

type modular_typing_environment = ml_type Environment.t

(** Fresh variables *)
let letter_counter : int ref = ref (-1)

let number_counter : int ref = ref (-1)

let fresh () : type_variable =
  begin
    if !letter_counter = 25 then begin
      letter_counter := 0;
      incr number_counter
    end else
      incr letter_counter
  end;
  if !number_counter < 0 then
    Printf.sprintf "'%c" (char_of_int (int_of_char 'a' + !letter_counter))
  else
    Printf.sprintf "'%c%d" (char_of_int (int_of_char 'a' + !letter_counter)) !number_counter
  
(** Pretty-printing *)

let rec string_of_ml_type (t : ml_type) : string = match t with
  | Arr (alpha, beta) -> Printf.sprintf "%s -> %s" (string_of_ml_type alpha) (string_of_ml_type beta)
  | Prod (alpha, beta) -> Printf.sprintf "%s * %s" (string_of_ml_type alpha) (string_of_ml_type beta)
  | TypeInt -> "int"
  | TypeBool -> "bool"
  | TypeString -> "string"
  | TypeUnit -> "unit"
  | TypeHtml -> "html"
  | TypeDb -> "db"
  | TypeVar s -> s
  | TypeForall (x, tau) -> Printf.sprintf "Λ%s.%s" x (string_of_ml_type tau)

let string_of_typing_env ?(prefix : string = "") (gamma : typing_environment) : string =
  let non_empty_prefix = prefix = "" in
  let string_of_one_substitution (x : type_variable) (alpha : ml_type) (acc : string) : string =
    if acc = "" then
      if non_empty_prefix then
        Printf.sprintf "%s.%s : %s" prefix x (string_of_ml_type alpha)
      else
        Printf.sprintf "%s : %s" x (string_of_ml_type alpha)
    else
      if non_empty_prefix then
        Printf.sprintf "%s.%s : %s, %s" prefix x (string_of_ml_type alpha) acc
      else
        Printf.sprintf "%s : %s, %s" x (string_of_ml_type alpha) acc
  in
  StringMap.fold string_of_one_substitution gamma ""

let string_of_type_substitution (gamma : typing_environment) : string =
  let string_of_one_substitution (x : type_variable) (alpha : ml_type) (acc : string) : string =
    if acc = "" then
      Printf.sprintf "%s ↦ %s" x (string_of_ml_type alpha)
    else
      Printf.sprintf "%s ↦ %s, %s" x (string_of_ml_type alpha) acc
  in
  StringMap.fold string_of_one_substitution gamma ""

let rec string_of_modular_typing_environment ?(prefix : string = "") (gamma : modular_typing_environment) : string =
  if Environment.is_empty gamma then "∅" else begin
    let string_of_one_type_binding (prefix : string list) (x : variable) (tau : ml_type) (acc : string) : string =
      if acc = "" then
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s" x (string_of_ml_type tau)
        else
          Printf.sprintf "%s.%s ↦ %s" (String.concat "." (List.rev prefix)) x (string_of_ml_type tau)
      else
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s, %s" x (string_of_ml_type tau) acc
        else
          Printf.sprintf "%s.%s ↦ %s, %s" (String.concat "." (List.rev prefix)) x (string_of_ml_type tau) acc
    in
    Environment.fold string_of_one_type_binding gamma ""
  end