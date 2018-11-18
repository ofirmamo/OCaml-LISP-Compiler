module SymbolParser: sig
  val parser: char list -> sexpr * char list
end
= struct


let _warns_ = PC.char '!';;
let _dollar_ = PC.char '$';;
let _carret_ = PC.char '^';;
let _astrix_ = PC.char '*';;
let _minus_ = PC.char '-';;
let _lodash_ = PC.char '_';;
let _eq_ = PC.char '=';;
let _plus_ = PC.char '+';;
let _lt_  = PC.char '<';;
let _gt_  = PC.char '>';;
let _quem_  = PC.char '?';;
let _fslash_ = PC.char '/';;
let _colon_ = PC.char ':';;


let _num_range_ = PC.range '0' '9' ;;
let _low_char_ = PC.range 'a' 'z' ;;
let _up_char_ = PC.range 'A' 'Z';;

(*symbol char parser*)
let _symbol_char_ =
	
	let _up_to_lower_ = PC.pack _up_char_ (fun ch-> lowercase_ascii ch) in 
	(* let _special_sym_chars_ = PC.disj_list [_ws_; _ds_ ] in  *)
	PC.disj_list [_num_range_ ; _low_char_; _up_to_lower_ ; _dollar_  ; _warns_ ;_carret_ ; _astrix_ ; _minus_ ;_lodash_ ; _eq_ ; _plus_ ; _lt_; _gt_; _quem_ ;_fslash_ ; _colon_] ;;

let parser = 
	let _stared_sym_char_ = PC.plus _symbol_char_ in 
	PC.pack _stared_sym_char_ (fun (sym) -> Symbol (list_to_string sym)
					 );;

end;;
