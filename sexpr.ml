#use "types.ml";;
#use "pc.ml";;
#use "symbolparser.ml";;
#use "boolparser.ml";;
#use "charparser.ml";;
#use "numberparser.ml";;
#use "stringparser.ml";;

module SExpParser: sig
  val parser : char list -> sexpr * char list
  val test_bool : char list -> sexpr * char list  (* TODO -> Delete before submission *)
  val test_char: char list -> sexpr * char list   (* TODO -> Delete before submission *)
  val test_number: char list -> sexpr * char list (* TODO -> Delete before submission *)
  val test_string: char list -> sexpr * char list (* TODO -> Delete before submission *)
  val test_symbol: char list -> sexpr * char list (* TODO -> Delete before submission *)

end

= struct

(* SExpr parser.. *)
let parser = 
  PC.disj_list [BoolParser.parser; CharParser.parser; NumberParser.parser; 
  				SymbolParser.parser; StringParser.parser];;

(* Testing bool implemtation *)
(* TODO -> Delete before submission *)
let test_bool t = 
  BoolParser.parser t;;

(* Testing bool implemtation *)
(* TODO -> Delete before submission *)
let test_char t = 
  CharParser.parser t;;

(* Testing number implemtation *)
(* TODO -> Delete before submission *)
let test_number t = 
  NumberParser.parser t;;

let test_string t = 
  StringParser.parser t;;


let test_symbol t = 
  SymbolParser.parser t;;

end;;