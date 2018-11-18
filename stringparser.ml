module StringParser: sig
 	val parser : char list -> sexpr * char list 
end
= struct


let pack_chars string_to_char new_char =
	PC.pack (PC.word string_to_char) (fun chl -> new_char);;

let _bslash_ = pack_chars "\\\\" '\\';;
let _dquote_ = pack_chars "\\\"" '\"';;
let _tab_ = pack_chars "\\t" '\t';;
let _page_ =pack_chars "\\f" '\012';;
let _newline_ = pack_chars "\\n" '\n';;
let _return_ = pack_chars "\\r" '\r';;

(* string meta char parser*)
let _string_meta_char_= PC.disj_list [_page_;_bslash_; _dquote_ 
										; _tab_ ; _newline_ ; _return_];;

(* <hex digit>+ parser assuming no more than 2 digs are given - as in assignment 1 discription*)
let _hex_digit_ = PC.pack (fun (chl) -> PC.nt_hex_nat('0' ::'x' :: chl) ) 
					(fun (h) -> Char.chr h) ;;

let _quote_ = PC.word "\"";;

(* literal char parser *)
let _lit_ = 
(* C where C is all ascii chars except double quote or backslash *)
	PC.pack (PC.range '\000' '\255')
				(fun (ch) ->  match ch with
							  | '\"' -> raise PC.X_no_match
							  | '\\' -> raise PC.X_no_match
							  | _ -> ch );;



(* string hex digit parser *)

let string_hex_char = 
	let _prefix_ = PC.word_ci "\\x" in 
	let _semicolon_ = PC.char ';' in
	let _hexed_ = PC.caten _prefix_  (PC.caten _hex_digit_ _semicolon_) in
	PC.pack _hexed_ (fun (pr, (xch ,sc))-> xch) ;;


let _string_char_ = PC.disj_list  [_string_meta_char_ ; string_hex_char ; _lit_];;

let parser = 
	let _string_cahrs_ = PC.star _string_char_ in 
	let _quoted_ = PC.caten _quote_ (PC.caten _string_cahrs_  _quote_) in 
	PC.pack _quoted_ (fun (qf, (sc , ql)) -> String (list_to_string sc));;
end;;

