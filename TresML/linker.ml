(** The goal (in this file) is to replace every import with the corresponding page, embedded in the toplevel dynamic page. *)
open Utils
open Lexer
open Syntax
open Parser

exception LinkingError of string

let get_filename (path : string) : string = snd (split_on_last_char '/' path (String.length path - 1))

let is_valid_module_name (potential_mod_name : string) : bool =
  match String.fold_left (fun _Id_state c -> eat_letter_lex_Identifier _Id_state c) (CouldBe ([], [])) potential_mod_name with
    | Is ((), _) -> true
    | _ -> false

let rec link_dynpage (page : dynml_webpage) : dynml_webpage = List.map begin function
    | Pure s -> Pure s
    | Script e -> Script (link_expr e)
    | Decl g -> begin match g with
      | ImportModule path ->
        let filename = get_filename path in
        let filename_without_ext = List.hd (String.split_on_char '.' filename) in
        let file_module_name = String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) filename_without_ext in
        let imported_in = open_in path in
        let loaded_raw_page = read_whole_file_str imported_in in
        close_in imported_in;
        let loaded_lexed = lexer loaded_raw_page in
        let loaded_parsed = parser loaded_lexed in
        if is_valid_module_name file_module_name then
          Decl (Inserted ({module_name = file_module_name ; reset_environment = true ; final_env_available = true ; content_available = true}, link_dynpage loaded_parsed))
        else
          raise (LinkingError "Invalid file name: should always start with an uppercase or lowercase ASCII letter and otherwise match identifier's lexic.")
      | ExprDecl (x, e) -> Decl (ExprDecl (x, link_expr e))
      | ModuleExprDecl (modu, x, e) -> Decl (ModuleExprDecl (modu, x, link_expr e))
      | Inserted (mode, page) -> Printf.fprintf stderr "Inserted encoutered during linking"; Decl (Inserted (mode, link_dynpage page))
      | _ -> Decl g
    end
  end page
and link_expr (e : expr) : expr = match e with
  | Html page -> Html (link_dynpage page)
  | Empty -> Empty
  | Let (x, e, in_body) -> Let (x, link_expr e, link_expr in_body)
  | Fun (x, body) -> Fun (x, link_expr body)
  | Fix (f, x, body) -> Fix (f, x, link_expr body)
  | App (e, e') -> App (link_expr e, link_expr e')
  | If (c, t, e) -> If (link_expr c, link_expr t, link_expr e)
  | Seq (e, e') -> Seq (link_expr e, link_expr e')
  | Var x -> Var x
  | WithModule (modu, e) -> WithModule (modu, link_expr e)
  | Couple (e, e') -> Couple (link_expr e, link_expr e')
  | Unit | String _ -> e (* TODO do that for other cases *)
  | Plus (e, e') -> Plus (link_expr e, link_expr e')
  | Minus (e, e') -> Minus (link_expr e, link_expr e')
  | Neg e -> Neg (link_expr e)
  | Mult (e, e') -> Mult (link_expr e, link_expr e')
  | Div (e, e') -> Div (link_expr e, link_expr e')
  | Pow (e, e') -> Pow (link_expr e, link_expr e')
  | Int n -> Int n
  | Gt (e, e') -> Gt (link_expr e, link_expr e')
  | Lt (e, e') -> Lt (link_expr e, link_expr e')
  | Geq (e, e') -> Geq (link_expr e, link_expr e')
  | Leq (e, e') -> Leq (link_expr e, link_expr e')
  | Eq (e, e') -> Eq (link_expr e, link_expr e')
  | Neq (e, e') -> Neq (link_expr e, link_expr e')
  | And (e, e') -> And (link_expr e, link_expr e')
  | Or (e, e') -> Or (link_expr e, link_expr e')
  | Not e -> Not (link_expr e)
  | Bool b -> Bool b
  | Concat (e, e') -> Concat (link_expr e, link_expr e')
  | Fstring lst -> Fstring ((List.map begin function
      | FstrString s -> FstrString s
      | FstrExpr e -> FstrExpr (link_expr e)
    end) lst)

let linker (page : dynml_webpage) : dynml_webpage = link_dynpage page