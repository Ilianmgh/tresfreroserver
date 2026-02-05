open Lexic
open Syntax

type type_variable = string (* TODO hide this type, but then must provide module StringMap for substitution in this here interface *)

type ml_type =
  | Arr of ml_type * ml_type
  | Prod of ml_type * ml_type
  | TypeInt | TypeBool | TypeString | TypeUnit
  | TypeDb
  | TypeHtml
  | TypeVar of type_variable

type typing_environment = ml_type StringMap.t

type modular_typing_environment = ml_type Environment.t

(** Fresh variable *)
val fresh : unit -> type_variable

val string_of_ml_type : ml_type -> string

val string_of_type_substitution : typing_environment -> string

(** Pretty printing *)

val string_of_typing_env : ?prefix:string -> typing_environment -> string

val string_of_modular_typing_environment : ?prefix:string -> modular_typing_environment -> string