open Utils
open Lexer
open Syntax
open Parser
open Linker
open TypeSyntax
open Typechecker
open Interpreter

(** Defining extern functions *)

let ml_type_of_sqlite_exec =
  Arr (
    TypeDb,
    Arr (
      Arr (TypeHtml, Arr (TypeHtml, TypeHtml)),
      Arr (
        Arr (TypeHtml, Arr (TypeString, Arr (TypeString, TypeHtml))),
        Arr (TypeString,
          TypeHtml
        )
      )
    )
  )

(** [straightforward_ml_function (Argsi f)] (with [i = 1,2,3,4]) is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function (name : string) (f : extern_function) : value =
  Clos (Environment.empty, VExternFunction (name, f))

(** To set pre-defined symbols in TresML, the extern_symbol list [predefined_symbols] in [interpret_tml_page]'s body is the only thing to modify.
  You need to add a record to the list of type [extern_symbol] (see below, Error: looping recursion). We explain each field:
  - [namespaces]: a list of namespaces indicating on which submodule the symbol will be available.
  - [name]: the actual name of the symbol
  E.g. if name is ["mySymbol"]: if [namespaces] is [[Module1; Submodule2]], your symbol will be available in TresML files as [Module1.Submodule2.mySymbol]
    whereas if [namespaces] is [[]], your symbol will be available under no namespaces: [mySymbol].
  - [v]: It's basically an OCaml function, tweaked to fit in the TresML interpreter.
    + First, to correctly type, it has to be an occurence of sum type [extern_function] and the label of
      this sum type indicates the number of arguments of the function.
    + Then, it has to be a function of type [value -> ... -> value], but basically you only need to take into
      account one kind of value for each argument, see examples in file `interpreter.ml` e.g. function [ml_fst].
    + Finally, the first argument of [f] as argument of the constructor [Argsi f] is an [environment].
      Quick explanation is, if your function is not-so-complicated (see below for further explanation), write a function
      [myExternFunction : value -> ... -> value] and set [v] to [Argsi (straightforward_fun_dropping_reset_env myExternFunction)].
      Explanation: if your extern function is first-order, or more generally, is not mutually recursive with
      the interpreter ([eval_...] functions), this first argument will never be used.
      So what happens when your extern function is higher-order ? When it is, you may need it to be
      mutually recursive with the interpreter to interpret the functions in argument (the issue here is actually
      the mutually recursive-ness with the interpreter, not the higher-order-ness in itself).
      If that's the case, you have to pass to the interpreter the additional argument [orig_env], which corresponds to
      an evaluation environment that is set when evaluating inserted page (e.g. with import).
      See [extern_sqlite_exec_with_reset_env] for reference.
  *)
type extern_symbol = {namespaces : module_name list ; name : string ; v : extern_function ; tau : ml_type}

(** Producing a page *)

let debug = true

let displayed = ["raw"; "lexed"; "parsed"; "typed"; "eval'd"]

(** [interpret_tml_page init_env source handle_output output_first_line] reads a TresML webpage from [source], computes the resulting html webpage and progressively pass the resulting page to the function [handle_ouput]. A canonical use case is providing [Printf.fprintf a_channel] as [handle_ouput].
  [init_env] is the initial evaluation environment /!\ TODO for now, can only contain string values.
  [output_first_line] is a boolean indicating whether to indicate information destined to the server in the first (e.g. session variables, redirection, ...).
  [os_type] is the OS's name, as given by [Sys.os_type] *)
let rec interpret_tml_page
  ((arguments, arguments_types) : environment * modular_typing_environment)
  (root_dir : string)
  (source : in_channel)
  (handle_output : string -> unit)
  (output_first_line : bool)
  (os_type : string) : unit =
  (** COMPUTING PRE-LOADED ENVIRONMENT *)
  (** List of predefined symbols, pre-loaded in the environment at execution. *)
  let full_extern_sqlite_open_db = straightforward_fun_dropping_reset_env (extern_sqlite_open_db_with_root_path root_dir) in
  let ml_type_fst = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'fst"))) in
  let ml_type_snd = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'snd"))) in
  let ml_type_str_replace = Arr (TypeString, Arr (TypeString, Arr (TypeString, TypeString))) in
  let predefined_symbols : extern_symbol list = [
        {namespaces = [sqlite_module_name] ; name = "exec"    ; v = Args4 extern_sqlite_exec_with_reset_env ; tau = ml_type_of_sqlite_exec}
      ; {namespaces = [sqlite_module_name] ; name = "opendb"  ; v = Args1 full_extern_sqlite_open_db ; tau = Arr (TypeString, TypeDb)}
      ; {namespaces = [sqlite_module_name] ; name = "closedb" ; v = Args1 (straightforward_fun_dropping_reset_env extern_sqlite_close_db) ; tau = Arr (TypeDb, TypeBool)}
      ; {namespaces = [] ; name = "fst" ; v = Args1 (straightforward_fun_dropping_reset_env ml_fst) ; tau = ml_type_fst}
      ; {namespaces = [] ; name = "snd" ; v = Args1 (straightforward_fun_dropping_reset_env ml_snd) ; tau = ml_type_snd}
      ; {namespaces = [] ; name = "not" ; v = Args1 (straightforward_fun_dropping_reset_env ml_not) ; tau = Arr (TypeBool, TypeBool)}
      ; {namespaces = ["String"] ; name = "replace" ; v = Args3 (straightforward_fun_dropping_reset_env ml_string_replace) ; tau = ml_type_str_replace}
      (* FIXME add char type *)
      ; {namespaces = ["String"] ; name = "get" ; v = Args2 (straightforward_fun_dropping_reset_env ml_string_get) ; tau = Arr (TypeString, Arr (TypeInt, TypeString))}
      (* FIXME give actual types to locations ? *)
      ; {namespaces = ["Http"] ; name = "redirect" ; v = Args1 (straightforward_fun_dropping_reset_env ml_redirect) ; tau = Arr (TypeString, TypeUnit)}
    ]
  in
  let pre_included_environment : environment =
    List.fold_left
      (fun acc entry -> Environment.add_and_path entry.namespaces entry.name (straigthforward_ml_function entry.name entry.v) acc)
      Environment.empty
      predefined_symbols
  in
  let pre_included_typing_env : modular_typing_environment =
    List.fold_left
      (fun acc entry -> Environment.add_and_path entry.namespaces entry.name entry.tau acc)
      Environment.empty
      predefined_symbols
  in
  (* EVALUATION *)
  let f_in = source in
  let code : string = read_whole_file_str f_in in
  close_in f_in;
  try
    let lexed = lexer code in
    let parsed = parser lexed in
    let linked = linker root_dir parsed in
    let typ_env = Environment.disjoint_union arguments_types pre_included_typing_env in
    let eval_env = Environment.disjoint_union arguments pre_included_environment in
    let _ = type_inferer typ_env linked in
    let final_env, location, values = eval eval_env linked in
    (* The first line contains information we want to send to the server, but that won't be sent to the client. *)
    if output_first_line then begin
      (* TODO slight optimization: do not re-send session variables that were not modified; but simply mention to the server to keep them *)
      let session_bindings = match Environment.submap_opt session_module_name final_env with
        | None -> ""
        | Some session_env -> Environment.fold
          begin
            fun prefixes x v acc ->
              if List.is_empty prefixes then begin (* for now, only top-level within module Session, could change for further needs *)
                (if debug then Printf.fprintf stderr "trying to repr: [[[%s]]]..." (string_of_value v));
                let repr_of_v = repr_of_value v in
                (if debug then Printf.fprintf stderr "... as : [[[%s]]]\n" repr_of_v);
                Printf.sprintf "%s&%s=%s" acc x repr_of_v
              end else
                acc
          end
          session_env ""
      in
      handle_output (Printf.sprintf "session%s;SEP;" session_bindings); (* FIXME will lead to issues if ';' occurs in a session variable *)
      begin match location with
        | None -> ()
        | Some target -> handle_output (Printf.sprintf "redirect=%s" target)
      end;
      handle_output "\n"
    end;
    List.iter (fun v -> handle_output (string_of_value v)) values
  with
    | PrelexingError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "PrelexingError: %s\n" s)
    end
    | LexingError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "LexingError: %s\n" s)
    end
    | ParsingError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "ParsingError: %s\n" s)
    end
    | LinkingError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "LinkingError: %s\n" s)
    end
    | TypingError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "TypingError: %s\n" s)
    end
    | UnificationError (alpha, beta, Recursive) -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf ";SEP;\nUnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta))
    end
    | UnificationError (alpha, beta, Incompatible) -> begin
      if output_first_line then handle_output ";\n";
      handle_output (Printf.sprintf ";SEP;\nUnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta))
    end
    | InterpreterError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "InterpreterError: %s\n" s)
    end
    | UnsupportedError s -> begin
      if output_first_line then handle_output ";SEP;\n";
      handle_output (Printf.sprintf "UnsupportedError: %s\n" s)
    end

(** [output_page init_env root_dir source dest output_first_line] reads a TresML webpage from [source], computes the resulting html webpage and writes it in [dest].
  [root_dir] is the root directory of the project. Included files have to be in subfolders of this one. 
  [init_env] is the initial evaluation environment /!\ TODO for now, can only contain string values.
  [output_first_line] is a boolean indicating whether to indicate information destined to the server in the first (e.g. session variables, redirection, ...).
  [os_type] is the OS's name, as given by [Sys.os_type] *)
let rec output_page ((arguments, arguments_types) : environment * modular_typing_environment) (root_dir : string) (source : in_channel) (dest : out_channel) (output_first_line : bool) (os_type : string) : unit =
  interpret_tml_page (arguments, arguments_types) root_dir source (fun s -> Printf.fprintf dest "%s" s) output_first_line os_type

exception MalformedCommandLine of string

(** Organized and named arguments obtained from the command line. *)
type command_line_args = {
    root_dir : string
  ; source : in_channel (* The source code *)
  ; target : out_channel (* Where to output the resulting webpage *)
  ; initial_environment : environment (* The environment initially obtained directly from the command line *)
  ; initial_typing_environment : modular_typing_environment (* The typing for values from [initial_environment] *)
  ; output_first_line : bool (* Whether or not to output the first line, indicating information to the server e.g. session variables. *)
}

(** [parse_command_line command_line = (source, dest, env)] parses the command line and retrieves the path to the source file [source], the path to the destination [dest] and the environment passed in argument [env] e.g. Post/Get/Session variables.
  Raises [MalformedCommandLine] if [command_line] is ill-formed. *)
let parse_command_line (command_line : string array) : command_line_args =
  let n = Array.length command_line in
  if n > 2 then
    let source_path = command_line.(1) in
    let dest_path = command_line.(2) in
    let root_dir, source_fd =
      if source_path = "-stdin" then
        None, stdin
      else
        Some (parent_dir_of_path source_path), open_in source_path
    in
    let dest_fd =
      if dest_path = "-stdout" then
        stdout
      else
        open_out dest_path
    in
    let rec parse_options
      (command_line : string array) (i : int)
      (env_acc : environment) (typ_env_acc : modular_typing_environment)
      (output_first_line_acc : bool) (root_dir : string option) : (environment * modular_typing_environment) * bool * string option =
      if i < n then
        match String.lowercase_ascii command_line.(i) with
          | "-argrepr" -> begin if i + 1 >= n then (* The remaining of the command line is supposed to be of the form `<argoption> <arguments> ...` *)
              raise (MalformedCommandLine "Argument expected after option -argrepr.")
            else
              let new_env, new_typenv = Parse_url_dictionary.parse_url_dictionary value_of_repr command_line.(i + 1) env_acc typ_env_acc in
              parse_options command_line (i + 2) new_env new_typenv output_first_line_acc root_dir
          end
          | "-argstr" -> begin if i + 1 >= n then (* The remaining of the command line is supposed to be of the form `<argoption> <arguments> ...` *)
              raise (MalformedCommandLine "Argument expected after option -argstr.")
            else
              let new_env, new_typenv = Parse_url_dictionary.parse_url_dictionary (fun s -> (VString s, TypeString)) command_line.(i + 1) env_acc typ_env_acc in
              parse_options command_line (i + 2) new_env new_typenv output_first_line_acc root_dir
          end
          | "--root" -> begin if i + 1 >= n then (* The remaining of the command line is supposed to be of the form `<argoption> <arguments> ...` *)
              raise (MalformedCommandLine "Argument expected after option --root.")
            else
              let new_env, new_typenv = Parse_url_dictionary.parse_url_dictionary (fun s -> (VString s, TypeString)) command_line.(i + 1) env_acc typ_env_acc in
              parse_options command_line (i + 2) new_env new_typenv output_first_line_acc (Some command_line.(i + 1))
          end
          | "-noserverdata" -> parse_options command_line (i + 1) env_acc typ_env_acc false root_dir
          | unknown_option -> raise (MalformedCommandLine (Printf.sprintf "%s: Unknown options." unknown_option))
      else
        (env_acc, typ_env_acc), output_first_line_acc, root_dir
    in
    let (initial_env, initial_typenv), output_first_line, root_dir = parse_options command_line 3 Environment.empty Environment.empty true root_dir in
    match root_dir with
      | None -> raise (MalformedCommandLine "Root directory unspecified. Maybe you set the input to stdin and did not add option --root <rootDirectory> ?")
      | Some root_dir_path -> { root_dir = root_dir_path ; source =  source_fd ; target = dest_fd ; initial_environment = initial_env ; initial_typing_environment = initial_typenv ; output_first_line = output_first_line }
  else
    raise (MalformedCommandLine "Not enough arguments: need at least input and output.")

let () =
  try
    let cl_args = parse_command_line Sys.argv in
    output_page (cl_args.initial_environment, cl_args.initial_typing_environment) cl_args.root_dir cl_args.source cl_args.target cl_args.output_first_line Sys.os_type;
    close_in cl_args.source;
    close_out cl_args.target
  with
    MalformedCommandLine cli_parser_msg -> begin
      let help_message_path = "./help_message.txt" in
      let f_help_msg = open_in help_message_path in
      let help_message = read_whole_file_str f_help_msg in
      Printf.printf "Malformed command line: %s\n\n%s\n" cli_parser_msg help_message
    end