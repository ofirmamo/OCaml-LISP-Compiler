#use "semantic-analyser.ml";;

exception X_const_tbl
exception X_get_ext_const
exception X_get_fvar_address

module type CODE_GEN = sig
  val make_consts_tbl : expr' list -> (constant * int * string) list
  val make_fvars_tbl : expr' list -> (string * int * string) list
	val generate : (constant * int * string) list -> (string * int * string) list -> expr' -> string
	val get_const_address: constant -> (constant * int * string) list -> string
	val get_fvar_address: string -> (string * int * string) list -> string
end;;

module Code_Gen : CODE_GEN = struct

let byte_size = 1;;
let qw_size = 8;;
let const_tbl_name = "const_tbl";;
let fvar_tbl_name = "fvar_tbl";;

let void_size = byte_size;;	(* Only Tag.. *)
let nil_size = byte_size;; (* Only Tag.. *)
let bool_size = byte_size + byte_size;; (* One byte for tag, and one for value *)
let char_size = byte_size + byte_size;; (* One byte for tag, and one for value *)
let int_size = byte_size + qw_size;; (* One byte for tag and 8 bytes for value *)
let float_size = byte_size + qw_size;; (* One byte for tag and 8 bytes for value *)
let string_size str = byte_size + qw_size + (String.length str);; (* Tag, Length, and chars seq *)
let symbol_size = byte_size + qw_size;; (* One byte for tag and 8 bytes for string ptr *)
let pair_size = byte_size + (qw_size * 2);; (* One byte for tag and 16 bytes for ptrs *)
let vec_size vec = byte_size + qw_size + ((List.length vec) * qw_size);;

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

and exsits str acc = List.exists (fun (name,value) -> String.equal str name) acc
and get_fvar_address value fvar_tbl = 
	let filtred = List.filter (fun (s, _, _) -> String.equal value s) fvar_tbl in
	if List.length filtred == 1 then fvar_to_addr (List.hd filtred) else raise X_get_fvar_address
and fvar_to_addr (_, i, _) = "[" ^ fvar_tbl_name ^ " + " ^ string_of_int (i * qw_size) ^ "]";;

let rec collect_unique_const acc expr =
	match expr with
		| Const'(Sexpr value) -> 
				acc @ (get_ext_const (Sexpr value))
		| BoxSet'(v, e) -> 
				(collect_unique_const acc e)
		| If'(test, dit, dif) -> 
				(collect_unique_const (collect_unique_const (collect_unique_const acc test) dit) dif)
		| Seq'(lst) | Or'(lst) -> 
				List.fold_left collect_unique_const acc lst
		| Set'(v, e) | Def'(v, e) -> 
				(collect_unique_const (collect_unique_const acc v) e)
		| LambdaSimple'(_, body) | LambdaOpt'(_, _, body) ->
				collect_unique_const acc expr
		| Applic'(rator, rands) | ApplicTP'(rator, rands) ->
				List.fold_left collect_unique_const acc ([rator] @ rands)
		| _ -> acc

and get_ext_const value = 
	match value with
		| Sexpr (Symbol str) -> 
				[Sexpr(String str); value]
		| Sexpr(Pair (car, cdr)) -> 
				(get_ext_const (Sexpr car)) @ (get_ext_const (Sexpr cdr)) @ [value]
		| Sexpr(Vector lst) -> 
				(List.fold_left (fun acc e -> acc @ (get_ext_const (Sexpr e))) [] lst) @ [value]
		| _ -> [value]

and clean_consts lst =
	List.fold_left (fun acc elem -> if not(exists_const acc elem) then acc @ [elem] else acc) 
		[Void; Sexpr(Nil); Sexpr(Bool(false)); Sexpr(Bool(true))] lst
and exists_const lst elem = List.exists (fun const -> expr_eq (Const const) (Const elem)) lst
and const_tbl asts = clean_consts (List.fold_left collect_unique_const [] asts);;

let rec _make_consts_tbl_ acc indx lst =
	match lst with
		| [] -> acc
		| Void :: tl -> 
			(_make_consts_tbl_ (acc @ [(Void, indx, "MAKE_LIT_VOID")]) (indx + void_size) tl)
		| Sexpr(Nil) :: tl ->  
			(_make_consts_tbl_ (acc @ [Sexpr(Nil), indx, "MAKE_LIT_NIL"]) (indx + nil_size) tl)
		| Sexpr(Bool(false)) :: tl-> 
			(_make_consts_tbl_ (acc @ [Sexpr(Bool false), indx, "MAKE_LIT_BOOL(0)"]) 
				(indx + bool_size) tl)
		| Sexpr(Bool(true)) :: tl ->
			(_make_consts_tbl_ (acc @ [Sexpr(Bool true), indx, "MAKE_LIT_BOOL(1)"]) 
				(indx + bool_size) tl)
		| Sexpr(Char(c)) :: tl -> 
			(_make_consts_tbl_ (acc @ [Sexpr(Char(c)), indx, "MAKE_LIT_CHAR('" ^ String.make 1 c ^"')"]) 
				(indx + char_size) tl)
		| Sexpr(Number(Int x)) :: tl -> 
			(_make_consts_tbl_ (acc @ [Sexpr(Number(Int x)), indx, "MAKE_LIT_INT("^ string_of_int x ^")"]) 
				(indx + int_size) tl)
		| Sexpr(Number(Float x)) :: tl ->
			(_make_consts_tbl_ (acc @ [Sexpr(Number(Float x)), indx, "MAKE_LIT_FLOAT("^ string_of_float x ^")"])
				 (indx + float_size) tl)
		| Sexpr(String str) :: tl -> 
			(_make_consts_tbl_ 
				(acc @ [Sexpr(String str), indx, "MAKE_LIT_STRING " ^ string_of_int(String.length str) ^
				", \"" ^ str ^ "\""]) 
				(indx + (string_size str)) tl)
		| Sexpr(Symbol str) :: tl -> (* Because Topologic sort str exist *)
			(_make_consts_tbl_ (acc @ [Sexpr(Symbol str), indx, 
				"MAKE_LIT_SYMBOL(" ^ get_const_address (Sexpr(String(str))) acc ^ ")"])
					(indx + symbol_size) tl)
		| Sexpr(Pair(car, cdr)) :: tl -> (* Because Topologic sort str exist *)
			(_make_consts_tbl_ (acc @ [Sexpr(Pair(car,cdr)), indx,
				"MAKE_LITERAL_PAIR(" ^ get_const_address (Sexpr car) acc ^ ", " 
				^ get_const_address (Sexpr cdr) acc ^ ")"]) (indx + pair_size) tl)
		| Sexpr(Vector(lst)) :: tl -> (_make_consts_tbl_ (acc @ [ Sexpr(Vector lst) , indx,
				"MAKE_LIT_VECTOR " ^ (String.concat ", " (List.map (fun sexpr -> get_const_address (Sexpr(sexpr)) acc) lst))  ^ "" ]) (indx + (vec_size lst)) tl)

and get_const_address e lst = 
	"" ^ const_tbl_name ^ " + " ^ string_of_int (get_index(filter_consts lst e)) ^ ""
and get_index (_, i, _) = i
and filter_consts lst sexpr = 
	List.hd (List.filter (fun (e,_,_) -> expr_eq (Const e) (Const sexpr)) lst);;

let make_consts_tbl asts = _make_consts_tbl_ [] 0 (const_tbl asts);;
let make_fvars_tbl asts  = make_indx_fvar_tbl (List.fold_left _make_fvar_tbl_ prefix_fvar_tbl asts);;
let generate consts fvars e = "";;
end;;

