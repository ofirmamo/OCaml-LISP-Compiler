#use "reader.ml"

exception Test_fail;;

(* Symbol Tester *)
assert (SymbolParser.parser (string_to_list "Abc") = (Symbol("abc") , []));;
assert (SymbolParser.parser (string_to_list "ABC") = (Symbol("abc") , []));;
assert (SymbolParser.parser (string_to_list "ABC^12") = (Symbol("abc^12") , []));;

(* Boolean Tester :) *)
assert (SExpParser.test_bool (string_to_list "#t") = (Bool(true) , []));;
assert (SExpParser.test_bool (string_to_list "#T") = (Bool(true) , []));;
assert (SExpParser.test_bool (string_to_list "#f") = (Bool(false), []));;
assert (SExpParser.test_bool (string_to_list "#F") = (Bool(false), []));;
 
(* Char Tester :D *)

assert (SExpParser.test_char (string_to_list "#\\a") = (Char('a'), []));;
assert (SExpParser.test_char (string_to_list "#\\xa") = (Char('\n'), []));;
assert (SExpParser.test_char (string_to_list "#\\newline") = (Char('\n'), []));;
assert (SExpParser.test_char (string_to_list "#\\page") = (Char('\012'), []));;
assert (SExpParser.test_char (string_to_list "#\\return") = (Char('\r'), []));;
assert (SExpParser.test_char (string_to_list "#\\tab") = (Char('\t'), []));;
assert (SExpParser.test_char (string_to_list "#\\space") = (Char(' '), []));;
assert (SExpParser.test_char (string_to_list "#\\nul") = (Char('\000'), []));;
try assert (SExpParser.test_char (string_to_list "#\\ ") = (Char(' '), [])); raise Test_fail
with PC.X_no_match -> ();;


(* Number Tester :X *)
assert (SExpParser.test_number (string_to_list "1234") = (Number(Int 1234), []));;
assert (SExpParser.test_number (string_to_list "1234.1") = (Number(Float 1234.1), []));;
assert (SExpParser.test_number (string_to_list "1234.1") = (Number(Float 1234.1), []));;
assert (SExpParser.test_number (string_to_list "#xa") = (Number(Int 10), []));;
assert (SExpParser.test_number (string_to_list "1234.1.1") = (Number(Float 1234.1), ['.'; '1']));;
assert (SExpParser.test_number (string_to_list "#xa.a") = (Number(Float 10.625), []));;
assert (SExpParser.test_number (string_to_list "#xa.A") = (Number(Float 10.625), []));;
assert (SExpParser.test_number (string_to_list "#xA.A") = (Number(Float 10.625), []));;


(* SExpr Tester :P *)
assert (SExpParser.parser (string_to_list "#xA.A") = (Number(Float 10.625), []));;
assert (SExpParser.parser (string_to_list "#xa.a") = (Number(Float 10.625), []));;
assert (SExpParser.parser (string_to_list "1234.1") = (Number(Float 1234.1), []));;
assert (SExpParser.parser (string_to_list "1234") = (Number(Int 1234), []));;
assert (SExpParser.parser (string_to_list "#\\page") = (Char('\012'), []));;


(* Simple Comment Tester *)
assert (Reader.test_simple_comment (string_to_list ";Hello") = (Nil, []));;
assert (Reader.test_simple_comment (string_to_list ";Hello;aa\nd") = (Nil, ['d']));;
assert (Reader.test_simple_comment (string_to_list ";Hello;aa\n ") = (Nil, [' ']));;
try assert (Reader.test_simple_comment (string_to_list "a;Hello;aa\n ") = (Nil, [' '])); 
raise Test_fail with PC.X_no_match -> ();;

(* Sexpr comments Tester :\ *)
assert (Reader.sexpr_comment_parser (string_to_list "#;1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; 1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#;  ;Hello\n ;hELL\n 1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#;  ;Hello\n 1 ;HEllo") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #; 1 1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #; 1 1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #; 1 #;#;1 1 1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #; 1;hell\n1") = (Nil, []));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #xA.A #xA.A") = (Nil, ['#';'x';'A';'.';'A']));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #\\newline h") = (Nil, ['h']));;
assert (Reader.sexpr_comment_parser (string_to_list "#; #; #\\newline #; 1 #; #; 1 #; #; 1 1 #xa 1 1") = (Nil, ['1']));;
assert (Reader.sexpr_comment_parser (string_to_list "#; \n\n 1 ") = (Nil, []));;



(* read_sexpr Tester :O *)
assert (Reader.read_sexpr "1" = Number(Int 1));;
assert (Reader.read_sexpr "12.3" = Number(Float 12.3));;
assert (Reader.read_sexpr "#x12" = Number(Int 18));;
assert (Reader.read_sexpr "#x12.8" = Number(Float 18.5));;
assert (Reader.read_sexpr "#t" = Bool true);;
assert (Reader.read_sexpr "#f" = Bool false);;
assert (Reader.read_sexpr "#\\page" = Char '\012');;
assert (Reader.read_sexpr "#\\newline" = Char '\n');;
try assert (Reader.read_sexpr "1 1" = Number (Int 1)) with
PC.X_no_match -> ();;
assert (Reader.read_sexpr "     #\\newline" = Char '\n');;
assert (Reader.read_sexpr "     #\\newline     " = Char '\n');;
assert (Reader.read_sexpr "#\\newline     " = Char '\n');;
assert (Reader.read_sexpr ";Hi WOrld\n#\\nul" = Char '\000');;
assert (Reader.read_sexpr ";hello\n#\\nul;h" = Char '\000');;
assert (Reader.read_sexpr "#\\nul;n\n" = Char '\000');;
assert (Reader.read_sexpr "#; #; 1 #; 1 #; 1 #; 1 1 #\\xa" = Char '\n');;
assert (Reader.read_sexpr "#;1 #\\newline#; #;1 1" = Char '\n');;
assert (Reader.read_sexpr "#\\xa#;1" = Char '\n');; 

(* String parser test (.)(.) *)
assert (SExpParser.test_string (string_to_list "\"\\xa;\"") = (String("\n"), []));;
assert (SExpParser.test_string (string_to_list "\"&!\"") = (String("&!"), []));;
assert (SExpParser.test_string (string_to_list "\" &!\"") = (String(" &!"), []));;
assert (SExpParser.test_string (string_to_list "\" \"&!\"") = (String(" "), ['&';'!';'\"']));;
assert (SExpParser.test_string (string_to_list "\"\\x34;\"") = (String("4"), []));;

(* Symbol parser test  ;() *)
assert (SExpParser.test_symbol (string_to_list "Asym304") = (Symbol("asym304"), []));;
assert (SExpParser.test_symbol (string_to_list "!$*^+_=-><!/") = (Symbol("!$*^+_=-><!/"), []));;
assert (SExpParser.test_symbol (string_to_list "sym!$>304") = (Symbol("sym!$>304"), []));;
assert (SExpParser.test_symbol (string_to_list "$!#") = (Symbol("$!"), ['#']));;
