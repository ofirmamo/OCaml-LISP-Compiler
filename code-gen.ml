#use "semantic-analyser.ml";;

exception X_const_tbl
exception X_get_ext_const
exception X_get_fvar_address
exception X_genrate

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
let print_subroutine = "\tcall write_sob_if_not_void\t;;;Print Sub Routine\n";;
let after_app = "\tadd rsp, WORD_SIZE\n\tpop rbx\n\tadd rbx, 1\n\tshl rbx, 3\n\tadd rsp, rbx";;

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
let count () =
	let n = 0 in
	let rn = ref n in
	let x() = 
		rn := !rn+1;
		!rn in
		x;;
let counter = count();;


let prefix_fvar_tbl = 
	["boolean?", "is_boolean"; "float?", "is_float"; "integer?", "is_integer"; "pair?", "is_pair";
   "null?", "is_null"; "char?", "is_char"; "vector?", "is_vector"; "string?", "is_string";
   "procedure?", "is_procedure"; "symbol?", "is_symbol"; "string-length", "string_length";
   "string-ref", "string_ref"; "string-set!", "string_set"; "make-string", "make_string";
   "vector-length", "vector_length"; "vector-ref", "vector_ref"; "vector-set!", "vector_set";
   "make-vector", "make_vector"; "symbol->string", "symbol_to_string"; 
   "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "eq?", "is_eq";
	 "+", "bin_add"; "*", "bin_mul"; "-", "bin_sub"; "/", "bin_div"; "<", "bin_lt"; "=", "bin_equ";
	 "car", "car"; "cdr", "cdr"; "cons", "cons"; "set-car!", "set_car"; "set-cdr!", "set_cdr";
	 "apply", "apply"];;

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
and fvar_to_addr (_, i, _) = fvar_tbl_name ^ " + " ^ string_of_int (i * qw_size);;

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
				collect_unique_const acc body
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
			(_make_consts_tbl_ (acc @ [Sexpr(Char(c)), indx, "MAKE_LIT_CHAR(" ^ (string_of_int (Char.code c)) ^")"]) 
				(indx + char_size) tl)
		| Sexpr(Number(Int x)) :: tl -> 
			(_make_consts_tbl_ (acc @ [Sexpr(Number(Int x)), indx, "MAKE_LIT_INT("^ string_of_int x ^")"]) 
				(indx + int_size) tl)
		| Sexpr(Number(Float x)) :: tl ->
			(_make_consts_tbl_ (acc @ [Sexpr(Number(Float x)), indx, "MAKE_LIT_FLOAT("^ string_of_float x ^")"])
				 (indx + float_size) tl)
		| Sexpr(String str) :: tl -> 
			if String.equal str "" then (acc @ [Sexpr(String str), indx, "MAKE_LIT_STRING 0, 0"])
			else (_make_consts_tbl_ 
				(acc @ [Sexpr(String str), indx, "MAKE_LIT_STRING " ^ string_of_int(String.length str) ^
				", " ^ (make_comma_str str 0 (String.length str))]) 
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

and make_comma_str str indx len =
	if indx = (len - 1) 
		then (string_of_int(Char.code(String.get str indx))) 
			else (string_of_int(Char.code(String.get str indx))) ^ ", " ^ (make_comma_str str (indx + 1) len)

and get_const_address e lst = 
	"" ^ const_tbl_name ^ " + " ^ string_of_int (get_index(filter_consts lst e)) ^ ""
and get_index (_, i, _) = i
and filter_consts lst sexpr = 
	List.hd (List.filter (fun (e,_,_) -> expr_eq (Const e) (Const sexpr)) lst);;

let rec genrate_asm del sub_routine consts fvars e env_deepnace parent_params= 
		match e with
			| Const'(c) -> 
				sub_routine del ("\tmov rax, " ^ get_const_address c consts ^ "\t;;;Const by genrate")
			| Var'(VarFree(n)) -> 
				sub_routine del ("\tmov rax, qword [" ^ (get_fvar_address n fvars) ^ "] ;;; fvar " ^ n)
			| Set'(Var'(VarFree(n)), expr) | Def'(Var'(VarFree(n)), expr) -> 
				set_free_to_asm fvars consts n expr sub_routine env_deepnace parent_params
			| Var'(VarBound(_,maj,min )) -> 
				var_bound_to_asm fvars consts (string_of_int maj) (string_of_int min) sub_routine
			| Set'(Var'(VarBound(_, maj, min)), expr) -> 
				set_var_bound_to_asm fvars consts (string_of_int maj) (string_of_int min) 
					expr sub_routine env_deepnace parent_params
			| Var'(VarParam(_,min))-> 
					sub_routine del ("\tmov rax, qword [rbp + 8*(4 +"^(string_of_int min)^")]")
			| Set'(Var'(VarParam(_,min)), expr) -> 
					set_param_to_asm fvars consts (string_of_int min) expr sub_routine env_deepnace parent_params
			| BoxGet'(v) -> 
					box_get_to_asm fvars consts v sub_routine env_deepnace parent_params
			| BoxSet'(v,expr)-> 
					box_set_to_asm fvars consts v expr sub_routine env_deepnace parent_params
			| Box'(VarParam(_,min)) -> 
					box_param_to_asm fvars consts (string_of_int min) sub_routine env_deepnace 
			| Seq'(lst) -> 
					e_in_seq lst consts fvars sub_routine env_deepnace parent_params
			| If'(test,dit,dif) -> 
					if_to_asm fvars consts test dit dif sub_routine env_deepnace parent_params
			| Or'(lst) -> 
					or_to_asm fvars consts lst sub_routine env_deepnace parent_params
			| LambdaSimple'(strlst, body) ->
					lambda_simple_to_asm fvars consts (List.length strlst) body sub_routine env_deepnace parent_params
			| LambdaOpt'(params, vs, body) ->
					lambda_opt_to_asm fvars consts (List.length params) body sub_routine env_deepnace parent_params
			| Applic'(rator, rands) -> 
					app_to_asm fvars consts rator rands sub_routine env_deepnace parent_params
			| ApplicTP'(rator, rands) ->
					appTP_to_asm fvars consts rator rands sub_routine env_deepnace parent_params
			| _ -> raise X_genrate


and appTP_to_asm fvars consts rator rands sub_routine env_deepnace parent_params =
	let rands_count = string_of_int (List.length rands) in	
	let pushti = String.concat "\tpush rax ;;; push arg applic\n\n" 
		(List.fold_right (fun e acc -> acc @ 
			[(genrate_asm "\n" not_subroutine consts fvars e env_deepnace parent_params)]) rands []) in	
	let push_rands_asm = if (String.equal pushti "") then "\tpush SOB_NIL_ADDRESS\n"
			else "\tpush SOB_NIL_ADDRESS\n" ^ pushti ^ "\tpush rax ;;; push arg applic\n" in
	let rator_to_asm = genrate_asm "\n" not_subroutine consts fvars rator env_deepnace parent_params ^
			"\tCLOSURE_ENV rbx, rax\n\tpush rbx\n\n" in 
	let pushti_rands = push_rands_asm ^ "\tpush " ^ rands_count ^ " ;;; args count applic\n\n" in
	let old_rbp = "\tpush qword [rbp + 1 * WORD_SIZE] ;;; old rbp ApplicTP'\n" in
	let frame = "\tpush qword [rbp]\n\tSHIFT_FRAME " ^ (string_of_int (5 + (List.length rands))) ^ "\n\tpop rbp\n" in
	let sof = "\tCLOSURE_CODE rax, rax\n\tjmp rax" in
	sub_routine "\n\n" (pushti_rands ^ rator_to_asm ^ old_rbp ^ frame ^ sof)

and app_to_asm fvars consts rator rands sub_routine env_deepnace parent_params=
	let rands_count = string_of_int (List.length rands) in
	let pushti = String.concat "\tpush rax ;;; push arg applic\n\n" 
								(List.fold_right (fun e acc -> acc @ 
									[(genrate_asm "\n" not_subroutine consts fvars e env_deepnace parent_params)]) rands []) in
	let push_rands_asm = if (String.equal pushti "") then "\tpush SOB_NIL_ADDRESS\n"
		else "\tpush SOB_NIL_ADDRESS\n" ^ pushti ^ "\tpush rax ;;; push arg applic\n" in
	let rator_to_asm = genrate_asm "\n" not_subroutine consts fvars rator env_deepnace parent_params in 
	let pushti_rands = push_rands_asm ^ "\tpush " ^ rands_count ^ " ;;; args count applic\n" in
	sub_routine "\n\n" (pushti_rands^rator_to_asm^"\tCLOSURE_ENV rbx, rax\n\tpush rbx\n\tCLOSURE_CODE rbx, rax
	call rbx\n\n" ^ after_app)


and copy_old_envs i env_deepnace acc_str =
if i >= env_deepnace then acc_str
		else acc_str^(copy_old_envs (i+1) env_deepnace ("\n\tmov rcx, qword [rbx + (8 * " ^ (string_of_int (i- 1))
			^ ")]\n\tmov qword [rax + (8 * "^(string_of_int i)^")], rcx\n"))

and deep_copy_params i num_params acc_str = 
if i = num_params then acc_str
	else acc_str^(deep_copy_params (i+1) num_params 
		("\n\tmov rcx, qword [rbp + (8 * (4 + "^(string_of_int i)^"))]\n\tmov qword [rbx + " ^ (string_of_int ( 8 * i)) ^"], rcx\n"))

and lambda_simple_to_asm fvars consts num_params body sub_routine env_deepnace parent_params = 
	let malloc_cp_old_env = 
	 ("\tMALLOC rax, (8 * "^(string_of_int env_deepnace)^")\n\tmov rbx, qword [rbp + (8 * 2)]"^(copy_old_envs 1 env_deepnace ""))^"\n" in  
	let make_new_env = if (parent_params = (-1)) then ""
			else "\tMALLOC rbx, (8 *"^(string_of_int parent_params)^")\n"^(deep_copy_params 0 parent_params "")^"\n" in  
	let build_ext_env = "\tmov qword [rax] , rbx\n\tmov rdx, rax\n" in
	let body_to_asm = (genrate_asm "\n" not_subroutine consts fvars body (env_deepnace + 1) num_params) in 
	let lconter = (string_of_int (counter())) in
	let body_proc = "\tjmp Lcont_"^lconter^"\nLcode_"^lconter^":\n\tpush rbp\n\tmov rbp, rsp\n\n"^body_to_asm^"\tleave\n\tret\nLcont_"^lconter^":\n" in
	let proc_env_if_should =
		if (env_deepnace = 0) then "\tmov rdx, qword SOB_NIL_ADDRESS\n"
 			else (malloc_cp_old_env^make_new_env^build_ext_env) in
	sub_routine "\n" (proc_env_if_should^body_proc^("\tMAKE_CLOSURE(rax , rdx , Lcode_"^lconter^")"))	

and build_adjustment_loop params_str = ".loop:\n\tcmp rbx, 0\n\tjle .after_adjust\n\n" ^
		"\tadd rsp, (" ^ params_str ^ " + 2) * WORD_SIZE\n\tmov rax, rbx\n\tmov rdx, WORD_SIZE\n\tmul rdx" ^
		"\n\tadd rsp, rax\n\tmov rdx, qword [rsp]\n\tsub rsp, rax\n\tsub rsp, (" ^ params_str ^" + 2) * WORD_SIZE" ^
		"\n\tmov rsi, rcx\n\tMAKE_PAIR(rcx, rdx, rsi)\n\tsub rbx, 1\n\tjmp .loop\n\n"

and adjust_stack num_params =
		let params_str = (string_of_int num_params) in
		let get_n = "mov rbx, qword [rsp + 2 * WORD_SIZE]\n" in
		let sub_m = "\tsub rbx, " ^ params_str ^ "\n" in
		let nil = "\tmov rcx, SOB_NIL_ADDRESS\n" in
		let loop = build_adjustment_loop params_str in
		get_n ^ sub_m  ^ nil ^ loop ^
		".after_adjust:\n\tmov qword [rsp + (3 + " ^ params_str ^ ") * WORD_SIZE], rcx\n"

and lambda_opt_to_asm fvars consts num_params body sub_routine env_deepnace parent_params = 
let malloc_cp_old_env = 
	("\tMALLOC rax, (8 * "^(string_of_int env_deepnace)^")\n\tmov rbx, qword [rbp + (8 * 2)]"^(copy_old_envs 1 env_deepnace ""))^"\n" in  
let make_new_env = if (parent_params = (-1)) then ""
	else "\tMALLOC rbx, (8 *"^(string_of_int parent_params)^")\n"^(deep_copy_params 0 parent_params "")^"\n" in  
let build_ext_env = "\tmov qword [rax] , rbx\n\tmov rdx, rax\n" in
let body_to_asm = (genrate_asm "\n" not_subroutine consts fvars body (env_deepnace + 1) (num_params + 1)) in 
let lconter = (string_of_int (counter())) in
let body_proc = "\tjmp Lcont_"^lconter^"\nLcode_" ^ lconter ^
		":\n\t" ^ (adjust_stack num_params) ^ "\tpush rbp\n\tmov rbp, rsp\n\n"^body_to_asm^"\tleave\n\tret\nLcont_"^lconter^":\n" in
let proc_env_if_should =
	if (env_deepnace = 0) then "\tmov rdx, qword SOB_NIL_ADDRESS\n"
		 else (malloc_cp_old_env^make_new_env^build_ext_env) in
sub_routine "\n" (proc_env_if_should^body_proc^("\tMAKE_CLOSURE(rax , rdx , Lcode_"^lconter^")"))	

and box_param_to_asm fvars consts min sub_routine env_deepnace =
	sub_routine "\n\n" 
		("\tMALLOC rax, WORD_SIZE\n\tmov rbx, PVAR("^min^")\n\tmov qword [rax], rbx")

and box_set_to_asm fvars consts v expr sub_routine env_deepnace parent_params = 
	let expr_to_asm = genrate_asm "\n" not_subroutine consts fvars expr env_deepnace parent_params in
	let v_to_asm = genrate_asm "\n" not_subroutine consts fvars (Var' v) env_deepnace parent_params in
	sub_routine "\n" (expr_to_asm^"\tpush rax\n"^v_to_asm^"\tpop qword [rax]\n\tmov rax, SOB_VOID_ADDRESS")

and box_get_to_asm fvars consts v sub_routine env_deepnace parent_params = 
	let v_to_asm = genrate_asm "\n" not_subroutine consts fvars (Var' v) env_deepnace parent_params in
	sub_routine "\n" (v_to_asm^"\tmov rax, qword [rax]")

and set_param_to_asm fvars consts min expr sub_routine env_deepnace parent_params = 
	let expr_to_asm = genrate_asm "\n" not_subroutine consts fvars expr env_deepnace parent_params in
	sub_routine "\n"  (expr_to_asm^"\tmov qword [rbp + 8 * (4 +" ^ min ^ ")], rax\n\tmov rax, SOB_VOID_ADDRESS") 

and set_var_bound_to_asm fvars consts maj min expr sub_routine env_deepnace parent_params= 
	let expr_to_asm = genrate_asm "\n" not_subroutine consts fvars expr env_deepnace parent_params in
	sub_routine "\n" (expr_to_asm^"\tmov rbx, qword [rbp + 8*2]\n\tmov rbx, qword [rbx + 8*" ^ maj ^ "]\n\tmov qword [rbx + 8*" ^ min ^ "], rax\n\tmov rax, SOB_VOID_ADDRESS")
 
and var_bound_to_asm fvars consts maj min sub_routine = 
	sub_routine "\n" ("\tmov rax, qword [rbp + 8*2]\n\tmov rax, qword [rax + 8*" ^ 
										maj ^ "]\n\tmov rax, qword [rax + 8*" ^ min ^ "]")

and set_free_to_asm fvars consts n expr sub_routine env_deepnace parent_params= 
	let expr_to_asm = genrate_asm "\n" not_subroutine consts fvars expr env_deepnace parent_params in
	let addr = get_fvar_address n fvars in 
	sub_routine "\n" (expr_to_asm ^ "\tmov qword ["^addr^"], rax\n\tmov rax, SOB_VOID_ADDRESS") 
	
and or_to_asm fvars consts lst sub_routine env_deepnace parent_params =
	let to_asm = List.fold_left (fun acc e -> acc @ 
			[(genrate_asm "" not_subroutine consts fvars e env_deepnace parent_params)]) [] lst in
	let indx = string_of_int (counter()) in
	sub_routine ("\nLexit_" ^ indx ^ ":\n") 
		(String.concat ("\n\tcmp rax, SOB_FALSE_ADDRESS\n\tjne Lexit_"^ indx ^ "\n\n") to_asm)

and e_in_seq lst consts fvars sub_routine  env_deepnace parent_params= 
	let to_asm = List.fold_left (fun acc e -> acc @ 
			[(genrate_asm "" not_subroutine consts fvars e env_deepnace parent_params)]) [] lst in
	sub_routine "\n" (String.concat "\n" to_asm) 

and if_to_asm fvars consts test dit dif sub_routine env_deepnace parent_params=  
	let test_to_asm = genrate_asm "" not_subroutine consts fvars test env_deepnace parent_params in
	let dit_to_asm =  genrate_asm "" not_subroutine consts fvars dit env_deepnace parent_params in
	let dif_to_asm =  genrate_asm "" not_subroutine consts fvars dif env_deepnace parent_params in
	let indx = string_of_int (counter()) in
	sub_routine "\n"
	(""^test_to_asm^"\n\tcmp rax, SOB_FALSE_ADDRESS\n\tje Lelse_"^indx^"\n"^dit_to_asm^"\t\njmp Lexit_"^indx^"\n\nLelse_"^indx^":\n"^dif_to_asm^"\n\nLexit_"^indx^":")

and catenate_subroutine str del = del ^ str ^ "" ^ print_subroutine ^ ""
and not_subroutine str del = del ^ str ;;

let make_consts_tbl asts = _make_consts_tbl_ [] 0 (const_tbl asts);;
let make_fvars_tbl asts  = make_indx_fvar_tbl (List.fold_left _make_fvar_tbl_ prefix_fvar_tbl asts);;
let generate consts fvars e = genrate_asm "\n" catenate_subroutine consts fvars e 0 (-1);;
end;;

