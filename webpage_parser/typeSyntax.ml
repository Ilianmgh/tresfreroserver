open Lexic (* TODO put StringMap in utils *)

type type_variable = string

type ml_type = Arr of ml_type * ml_type | Prod of ml_type * ml_type | TypeInt | TypeBool | TypeString | TypeUnit | TypeHtml | TypeVar of type_variable

type typing_environment = ml_type StringMap.t

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
  | TypeVar s -> s

let string_of_type_substitution (gamma : typing_environment) : string =
  let string_of_one_substitution (x : type_variable) (alpha : ml_type) (acc : string) : string =
    if acc = "" then
      Printf.sprintf "%s ↦ %s" x (string_of_ml_type alpha)
    else
      Printf.sprintf "%s ↦ %s, %s" x (string_of_ml_type alpha) acc
  in
  StringMap.fold string_of_one_substitution gamma ""