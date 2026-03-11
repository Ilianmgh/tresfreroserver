open Utils
open Lexic
open Lexer
open Syntax
open TypeSyntax

let debug = false

type unification_error_type = Incompatible | Recursive

exception UnificationError of ml_type * ml_type * unification_error_type

exception TypingError of string

type type_substitution = ml_type StringMap.t

(** [erase_bound_variable x theta = theta'] where [theta'] is [theta] except:
  - if [x] is bound in [theta], this binding is removed in [theta'] ; and
  - if [x] occurs in some [theta(y)], then these occurences are replaced by a fresh variable in [theta'(y)]. *)
let rec erase_bound_variable (x : type_variable) (theta : type_substitution) : type_substitution =
  StringMap.fold begin fun y tau theta'_acc ->
    if x = y then (* we remove the binding *)
      theta'_acc
    else begin (* we replace occurences of [x] by a fresh variable *)
      StringMap.add y (apply_substitution tau (StringMap.add x (TypeVar (fresh ())) StringMap.empty)) theta'_acc
    end
  end theta StringMap.empty

(** [apply_substitution alpha theta] applies the substitution [theta] to the type [alpha] *)
and apply_substitution (alpha : ml_type) (theta : type_substitution) : ml_type =
  if StringMap.is_empty theta then alpha
  else begin match alpha with
    | Arr (alpha, beta) -> Arr (apply_substitution alpha theta, apply_substitution beta theta)
    | Prod (alpha, beta) -> Prod (apply_substitution alpha theta, apply_substitution beta theta)
    | TypeInt -> TypeInt
    | TypeBool -> TypeBool
    | TypeString -> TypeString
    | TypeDb -> TypeDb
    | TypeUnit -> TypeUnit
    | TypeHtml -> TypeHtml
    | TypeForall (x, alpha') -> apply_substitution alpha' (erase_bound_variable x theta)
    | TypeVar s -> begin match StringMap.find_opt s theta with
      | Some tau -> tau
      | None -> TypeVar s
    end
  end

(** [occurs x tau theta = true] iff [x] occurs in [tau theta] *)
let rec occurs (var : type_variable) (tau : ml_type) (theta : type_substitution) : bool =
  let rec occurs_no_substitution (var : type_variable) (tau : ml_type) : bool = match tau with
    | Arr (alpha, beta) | Prod (alpha, beta) -> occurs_no_substitution var alpha || occurs_no_substitution var beta
    | TypeInt | TypeBool | TypeString | TypeUnit | TypeHtml | TypeDb -> false
    | TypeForall (x, tau') -> if x = var then false else occurs_no_substitution var tau' (* TODO maybe these precautions neutralizes the need of bound variables to be fresh. *)
    | TypeVar s -> (s = var)
  in occurs_no_substitution var (apply_substitution tau theta)

(** [unpack_foralls (TypeForall (x1, TypeForall (x2, ...(TypeForall (xn, tau))))) = tau'], where [tau'] is [tau] where we replaced each occurence of [xi] by a fresh variable. *)
let unpack_foralls (tau : ml_type) : ml_type =
  let rec unpack_foralls_acc (tau : ml_type) (subst_acc : type_substitution) : ml_type = match tau with
    | TypeForall (x, tau') -> unpack_foralls_acc tau' (StringMap.add x (TypeVar (fresh ())) subst_acc)
    | _ -> apply_substitution tau subst_acc
  in
  unpack_foralls_acc tau StringMap.empty

(** Type unification. Returns a minimal substitution [theta] s.t. [alpha theta = beta theta] *)
let unify (alpha : ml_type) (beta : ml_type) : type_substitution =
  if debug then Printf.fprintf stderr "Unifying %s --- %s\n%!" (string_of_ml_type alpha) (string_of_ml_type beta); (* TODO replace by breadth-first exploration of the types to unify to get tail-recursion *)
  let rec update_substitution (x : variable) (tau : ml_type) (theta : type_substitution) = match StringMap.find_opt x theta with
    | None -> StringMap.add x tau theta
    | Some tau' -> let theta_taus = unify_aux tau tau' theta in StringMap.add x (apply_substitution tau theta_taus) theta_taus
  and unify_aux (alpha : ml_type) (beta : ml_type) (theta : type_substitution) : type_substitution = match unpack_foralls alpha, unpack_foralls beta with
    | Arr (alpha, beta), Arr (alpha', beta') ->
      let theta' = unify_aux alpha alpha' theta in
      unify_aux beta beta' theta'
    | Prod (alpha, beta), Prod (alpha', beta') ->
      let theta' = unify_aux alpha alpha' theta in
      unify_aux beta beta' theta'
    | TypeInt, TypeInt | TypeBool, TypeBool | TypeString, TypeString | TypeUnit, TypeUnit | TypeHtml, TypeHtml | TypeDb, TypeDb -> theta
    | TypeVar s1, TypeVar s2 -> if s1 = s2 then theta else begin update_substitution s1 (TypeVar s2) theta end
    | TypeVar s, tau | tau, TypeVar s -> if occurs s tau theta then raise (UnificationError (alpha, beta, Recursive)) else update_substitution s tau theta
    | _, _ -> raise (UnificationError (alpha, beta, Incompatible)) (* TODO replace by hoisting the error to display "thingy should have type stuff but is of type otherstuff"*)
  in unify_aux alpha beta StringMap.empty

(** [update_typing_env gamma theta = gamma'], an update of [gamma] s.t. [gamma'(x) = gamma(x) theta]
Remark: If type variable ['a] is bound in [theta], then there are no more occurences of ['a] in the image of [gamma'] *)
let rec update_typing_env (gamma : modular_typing_environment) (theta : type_substitution) : modular_typing_environment =
  Environment.map (fun alpha -> apply_substitution alpha theta) gamma

(** [update_typing_env_equalizing gamma alpha beta] updates [gamma] to a minimal typing environment compatible with [alpha = beta] *)
let update_typing_env_equalizing (gamma : modular_typing_environment) (alpha : ml_type) (beta : ml_type) : modular_typing_environment =
  update_typing_env gamma (unify alpha beta)

  (* FIXME: not sure the universally quantified types behaves properly... We'll see. *)
(** [type_inferer_one_expr oΓ Γ e = α] iff types variables in Γ can be refined to obtain a Γ' such that Γ' ⊢ e : α; oΓ is the initial typing environment, set when encountering an element that requires to reset the environment e.g. an inserted external page. *) 
let rec type_inferer_one_expr (orig_gamma : modular_typing_environment) (gamma : modular_typing_environment) (e1 : expr) : modular_typing_environment * ml_type = match e1 with
  | Empty -> assert false
  | Let (x, e, e') ->
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    type_inferer_one_expr orig_gamma (Environment.add x alpha gamma') e'
  | Fun (x, e) ->
    let x_type_variable = fresh () in
    let gamma', beta = type_inferer_one_expr orig_gamma (Environment.add x (TypeVar x_type_variable) gamma) e in
    (gamma', TypeForall (x_type_variable, Arr (Environment.find x gamma', beta)))
  | Fix (f, x, e) ->
    let vartype_x = fresh () in
    let vartype_ret = fresh () in
    let gamma_x = Environment.add x (TypeVar vartype_x) gamma in
    let gamma_x_f =  Environment.add f (Arr (TypeVar vartype_x, TypeVar vartype_ret)) gamma_x in
    let final_gamma, beta = type_inferer_one_expr orig_gamma gamma_x_f e in
    (final_gamma, Arr (Environment.find x final_gamma, beta))
  | App (e, e') ->
    let gamma', func_type = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', arg_type = type_inferer_one_expr orig_gamma gamma e' in
    let alpha_var, beta_var = fresh (), fresh () in
    let theta_func = unify func_type (Arr (TypeVar alpha_var, TypeVar beta_var)) in (* [e] must be a fuction *)
    begin match apply_substitution (unpack_foralls func_type) theta_func, arg_type with
      | Arr (alpha, beta), alpha' ->
        let theta = unify alpha alpha' in
        (update_typing_env gamma'' theta, apply_substitution beta theta)
      | func_type, arg_type -> raise (TypingError (Printf.sprintf "%s, %s, %s: this is not a function, it cannot be applied." (string_of_expr (App (e, e'))) (string_of_ml_type func_type) (string_of_ml_type arg_type))) (* TODO underline the function *)
    end
  | If (c, t, e) -> begin
      let gamma', tau = type_inferer_one_expr orig_gamma gamma c in
      let theta = unify TypeBool tau in
      let gamma'', t_type = type_inferer_one_expr orig_gamma (update_typing_env gamma' theta) t in
      let gamma''', e_type = type_inferer_one_expr orig_gamma gamma'' e in
      let theta' = unify t_type e_type in
      (update_typing_env gamma''' theta', apply_substitution t_type theta')
  end
  | Seq (e, e') -> begin
    let gamma', tau = type_inferer_one_expr orig_gamma gamma e in
    try
      let theta = unify TypeUnit tau in
      type_inferer_one_expr orig_gamma (update_typing_env gamma' theta) e'
    with
      UnificationError _ -> Printf.fprintf stderr "%s: is expected to have type unit." (string_of_expr (Seq (e, e'))); type_inferer_one_expr orig_gamma gamma' e'
  end
  | Html h -> (* just to not have a warning at compilation: *)(fun _ -> ()) (type_inferer_page orig_gamma gamma h); (gamma, TypeHtml)
  | Var x -> begin match Environment.find_opt x gamma with
    | Some t -> gamma, t
    | None -> raise (TypingError (Printf.sprintf "%s: undefined variable." (string_of_expr (Var x)))) (* TODO actually, shouldn't be a _typing_ error per se *)
  end 
  | Couple (e, e') ->
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', beta = type_inferer_one_expr orig_gamma gamma' e' in
    (gamma'', Prod (alpha, beta))
  | Neg e -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let theta = unify TypeInt alpha in
    (update_typing_env gamma' theta, TypeInt)
  end
  | Plus (e, e') | Minus (e, e') | Mult (e, e') | Div (e, e') | Pow (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', beta = type_inferer_one_expr orig_gamma gamma' e' in
    let theta = unify alpha beta in
    let t_int = apply_substitution alpha theta in 
    match t_int with
      | TypeInt -> (update_typing_env gamma'' theta, TypeInt)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_int) (string_of_ml_type TypeInt))) (* TODO keep replacing those tests by unification *)
  end
  | Int n -> (gamma, TypeInt)
  | Gt (e, e') | Lt (e, e') | Geq (e, e') | Leq (e, e') | Eq (e, e') | Neq (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', beta = type_inferer_one_expr orig_gamma gamma' e' in
    let theta = unify alpha beta in
    (update_typing_env gamma'' theta, TypeBool)
  end
  | Not e -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let theta = unify TypeBool alpha in
    (update_typing_env gamma' theta, TypeBool)
  end
  | And (e, e') | Or (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', beta = type_inferer_one_expr orig_gamma gamma' e' in
    let theta = unify alpha beta in
    let t_bool = apply_substitution alpha theta in 
    match t_bool with
      | TypeBool -> (update_typing_env gamma'' theta, TypeBool)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_bool) (string_of_ml_type TypeBool)))
  end
  | Bool b -> gamma, TypeBool
  | Concat (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr orig_gamma gamma e in
    let gamma'', beta = type_inferer_one_expr orig_gamma gamma' e' in
    let theta = unify alpha beta in
    let t_unif = apply_substitution alpha theta in 
    let theta' = unify t_unif TypeString in (* FIXME check if it works + if it does apply it verywhere else (&&, ||, +, * and so on...) *)
    let t_str = apply_substitution t_unif theta' in 
    match t_str with
      | TypeString -> (update_typing_env gamma'' theta, TypeString)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_str) (string_of_ml_type TypeString))) (* TODO keep replacing those tests by unification *)
  end
  | String _ -> gamma, TypeString
  | Fstring lst -> List.fold_left begin fun cur_gamma -> function
      | FstrExpr e -> fst (type_inferer_one_expr orig_gamma cur_gamma e)
      | FstrString s -> cur_gamma
    end gamma lst, TypeString
  | Unit -> gamma, TypeUnit
  | WithModule (module_name, e) -> begin match Environment.submap_opt module_name gamma with
    | None -> raise (TypingError (Printf.sprintf "%s: undefined module." module_name))
    | Some new_env -> begin
      if debug then Printf.fprintf stderr "before entering module %s\n\tto evaluate %s\n\t, gamma = %s\n" module_name (string_of_expr e) (string_of_modular_typing_environment gamma);
      let gamma', tau = type_inferer_one_expr orig_gamma new_env e in
      if debug then Printf.fprintf stderr "BEGIN_supmap with gamma' = %s\n" (string_of_modular_typing_environment gamma');
      let new_gamma = Environment.supmap_namespace module_name gamma' in 
      if debug then Printf.fprintf stderr "END_supmap\n";
      (new_gamma, tau)
    end
  end

(** [type_infere gamma [elt1; ...; eltn] = [(gamma1, tau1); ...; (gamman, taun) ; sth]] where when evaluating the dynamic webpage [[elt1; ...; eltn]] with initial typing environment [gamma] we infered each element [elti] had type [taui] with resulting typing environment [gammai].
  [sth] is an irrelevant value. *)
and type_inferer_page (orig_gamma : modular_typing_environment) (gamma : modular_typing_environment) (page : dynml_webpage) : (modular_typing_environment * modular_typing_environment * ml_type) list =
  (* FIXME maybe we should pass on (at least a part of) the accumulated environment when typing an expression if, at some point, we have actual '_weak types. *)
  (* FIXME maybe change ml_type to ml_type option to not type global declarations and stuff *)
  (* This fold evaluates each element in their order of appearance, the first element of the triplet carried during this fold is the typing environment to give when typing the next element. The other two are described in the function's comment (it's what is returned at the end). *)
  List.fold_left begin fun already_typed element -> begin match already_typed with
      | [] -> assert false
      | (cur_gamma, gamma_after_typed, tau) :: already_typed' -> begin match element with
        | Script e -> let gamma_e, tau_e = type_inferer_one_expr orig_gamma cur_gamma e in (cur_gamma, gamma_e, tau_e) :: (cur_gamma, gamma_after_typed, tau) :: already_typed'
        | Pure s -> (cur_gamma, gamma, TypeHtml) :: (cur_gamma, gamma_after_typed, tau) :: already_typed'
        | Decl (ExprDecl (x, e)) -> let gamma_e, tau_e = type_inferer_one_expr orig_gamma cur_gamma e in
          (Environment.add x tau_e cur_gamma, gamma_e, (* this type is not relevant, a declaration has no type *)tau_e) ::
          (cur_gamma, gamma_after_typed, tau) ::
          already_typed'
        | Decl (ModuleExprDecl (modu, x, e)) -> let gamma_e, tau_e = type_inferer_one_expr orig_gamma cur_gamma e in
          (Environment.add_to_sub [modu] x tau_e cur_gamma, gamma_e, (* this type is not relevant, a declaration has no type *)tau_e) ::
          (cur_gamma, gamma_after_typed, tau) ::
          already_typed'
        | Decl (ImportModule path) -> failwith "Trying to type an unlinked page (here link in compilation, not link as in the Web)."
        | Decl (Inserted (mode, insd_page)) -> begin
          let inserted_page_typed = if mode.reset_environment then
              type_inferer_page orig_gamma orig_gamma insd_page
            else
              type_inferer_page orig_gamma gamma insd_page
          in match inserted_page_typed with
            | [] -> failwith "Wasn't supposed to happen: in typechecker, evaluation of a page returned an empty list in Inserted case of type_inferer_one_expr."
            | (last_env, resulting_env, last_type) :: _ ->
              let env_after_typd = if mode.final_env_available then
                  last_env
                else
                  Environment.empty
              in
              (* adding the content of the page so it is accessible as ModuleName.Import.content iff mode.content_available = true *)
              let final_env_from_insd = begin
                  if mode.content_available then
                    Environment.add_and_path ["Import"] "content" TypeHtml
                  else
                    (fun x -> x)
                end env_after_typd
              in
              let next_env = Environment.add_sub mode.module_name final_env_from_insd gamma in
              (next_env, resulting_env, last_type(* the type is irrelevant (globals) *)) :: (cur_gamma, gamma_after_typed, tau) :: already_typed'
        end
        | Decl (OpenModule modu) -> begin match Environment.submap_opt modu gamma with
          | None -> raise (TypingError (Printf.sprintf "%s: undefined module." modu))
          | Some modu_typenv -> begin
            if debug then Printf.fprintf stderr "before opening module %s\n\tgamma = %s\n" modu (string_of_modular_typing_environment gamma);
            let new_gamma = Environment.hoist_submap cur_gamma modu in 
            if debug then Printf.fprintf stderr "after opening module %s\n\tnew_gamma = %s\n" modu (string_of_modular_typing_environment new_gamma);
            (new_gamma, cur_gamma, (* this type is not relevant, a declaration has no type *)TypeUnit) :: (cur_gamma, gamma_after_typed, tau) :: already_typed'
          end
        end
        | Decl (TypeDecl (x, e)) -> raise (UnsupportedError "Type declaration unsupported by now (type_inferer)")
      end
    end
  end [(gamma, gamma, TypeBool)] page

let type_inferer (gamma : modular_typing_environment) (page : dynml_webpage) : (modular_typing_environment * ml_type) list =
  List.tl (List.rev (List.map (fun (x, y, z) -> (y, z)) (type_inferer_page gamma gamma page)))
