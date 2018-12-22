#use "semantic-analyser.ml";;

module type CODE_GEN = sig
  val make_consts_tbl : expr' list -> (constant * ('a * string)) list
  val make_fvars_tbl : expr' list -> (string * int * string) list
  val generate : (constant * ('a * string)) list -> (string * 'a) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

let prefix_fvar_tbl = 
	["boolean?", "is_boolean"; "float?", "is_float"; "integer?", "is_integer"; "pair?", "is_pair";
   "null?", "is_null"; "char?", "is_char"; "vector?", "is_vector"; "string?", "is_string";
   "procedure?", "is_procedure"; "symbol?", "is_symbol"; "string-length", "string_length";
   "string-ref", "string_ref"; "string-set!", "string_set"; "make-string", "make_string";
   "vector-length", "vector_length"; "vector-ref", "vector_ref"; "vector-set!", "vector_set";
   "make-vector", "make_vector"; "symbol->string", "symbol_to_string"; 
   "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "eq?", "is_eq";
   "+", "bin_add"; "*", "bin_mul"; "-", "bin_sub"; "/", "bin_div"; "<", "bin_lt"; "=", "bin_equ"
(* you can add yours here *)];;

let make_indx_fvar_tbl l = List.mapi (fun indx (name, value) -> (name,indx,value)) l ;;

let rec _make_fvar_tbl_ acc asts= 
	match asts with 
		| Const' (expr) ->  acc
		| Var'(VarFree (expr)) when not(exsits expr acc) -> acc @ [(expr,"T_UNDEFINED")]
		| Var'(_) -> acc
		| Box' (_) -> acc
		| BoxGet'(_) -> acc
		| BoxSet'(_ , expr) -> (_make_fvar_tbl_ acc expr)
		| If' (test,dit, dif) ->  (_make_fvar_tbl_  (_make_fvar_tbl_ (_make_fvar_tbl_ acc dif) dit ) test)
		| Seq'(exprlst) | Or'(exprlst) -> (List.fold_left _make_fvar_tbl_  acc exprlst)
		| Def' (name, value) -> (_make_fvar_tbl_ (_make_fvar_tbl_ acc name) value)
		| Set' (name, value) -> (_make_fvar_tbl_ (_make_fvar_tbl_ acc name) value)
		| Applic' (expr, exprlst) -> (List.fold_left _make_fvar_tbl_  acc ([expr] @ exprlst))
		| ApplicTP' (expr, exprlst) -> (List.fold_left _make_fvar_tbl_  acc ([expr] @ exprlst))
		| LambdaSimple' (_ , body) -> _make_fvar_tbl_ acc body
		| LambdaOpt'(_ ,_ , body) -> _make_fvar_tbl_ acc body

and exsits str acc = List.exists (fun (name,value) -> String.equal str name) acc;;

let make_consts_tbl asts = raise X_not_yet_implemented;;
let make_fvars_tbl asts  = make_indx_fvar_tbl (List.fold_left _make_fvar_tbl_ prefix_fvar_tbl asts);;
let generate consts fvars e = raise X_not_yet_implemented;;
end;;

