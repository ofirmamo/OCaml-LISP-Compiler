#use "semantic-analyser.ml";;

(* Lexical address *)
assert (Semantics.annotate_lexical_addresses(
Tag_Parser.tag_parse_expression(
Reader.read_sexpr "#t")) = Const' (Sexpr (Bool true)));;

assert (Semantics.annotate_lexical_addresses(
Tag_Parser.tag_parse_expression(
Reader.read_sexpr "x")) = Var'(VarFree "x"));;

assert (Semantics.annotate_lexical_addresses(
Tag_Parser.tag_parse_expression(
Reader.read_sexpr "(lambda (x) x)")) = LambdaSimple' (["x"], Var'(VarParam("x", 0))));;

assert (Semantics.annotate_lexical_addresses(
Tag_Parser.tag_parse_expression(
Reader.read_sexpr "(lambda x x)")) = LambdaOpt' ([], "x", Var'(VarParam("x", 0))));;

assert (Semantics.annotate_lexical_addresses(
Tag_Parser.tag_parse_expression(
Reader.read_sexpr "(lambda (x) (lambda (y) x))")) = 
LambdaSimple'(["x"], LambdaSimple'(["y"], Var'(VarBound("x", 0, 0)))))