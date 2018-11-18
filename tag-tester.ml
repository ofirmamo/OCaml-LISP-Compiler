#use "tag-parser.ml";;

(* Atomic tags tester *)

(* Vars *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "a")) = (Var "a"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "expr")) = (Var "expr"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "+")) = (Var "+"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "eq?")) = (Var "eq?"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "string->symbol")) 
  = (Var "string->symbol"));;

(* Boolean *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#t")) = (Const(Sexpr(Bool true))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#T")) = (Const(Sexpr(Bool true))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#f")) = (Const(Sexpr(Bool false))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#F")) = (Const(Sexpr(Bool false))));;

(* Char *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#\\a")) = (Const(Sexpr(Char 'a'))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#\\xa")) = (Const(Sexpr(Char '\n'))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#\\newline")) = (Const(Sexpr(Char '\n'))));;

(* Numbers *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "12")) = (Const(Sexpr(Number(Int 12)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "12.1")) = (Const(Sexpr(Number(Float 12.1)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#xa")) = (Const(Sexpr(Number(Int 10)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "#xa.a")) = (Const(Sexpr(Number(Float 10.625)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "2e1")) = (Const(Sexpr(Number(Float 20.0)))));;

(* Seq tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin)")) = (Const Void));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin a)")) = (Var "a"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin (define x 1))")) 
  = Def(Var "x", Const(Sexpr(Number(Int 1)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin (lambda x 1))")) 
  = LambdaOpt([], "x", Const(Sexpr(Number(Int 1)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin a b)")) = (Seq [Var "a"; Var "b"]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin)")) = (Const Void));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin (define x 1) x)")) 
  = Seq [Def(Var "x", Const(Sexpr(Number(Int 1)))); Var "x"]);;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin (define x 1) (lambda (y) x))")) 
  = Seq [Def(Var "x", Const(Sexpr(Number(Int 1)))); LambdaSimple(["y"], Var "x")]);;

(* Lambda tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda () 1)")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x) 1)")) 
  = (LambdaSimple (["x"], (Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x y) 1)")) 
  = (LambdaSimple (["x"; "y"], (Const(Sexpr(Number(Int 1)))))));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x) )")) 
    = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x) 1 #\\a)")) 
  = (LambdaSimple (["x"], Seq [Const(Sexpr(Number(Int 1))); Const(Sexpr(Char 'a'))]) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda x 1)")) 
  = (LambdaOpt ([], "x", (Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda x 1 #\\a)")) 
  = (LambdaOpt ([], "x",  Seq [Const(Sexpr(Number(Int 1))); Const(Sexpr(Char 'a'))]) ));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda x )")) 
      = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x . y) 1)")) 
  = (LambdaOpt (["x"], "y", (Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x . y) 1 #t)")) 
  = (LambdaOpt (["x"], "y", Seq [(Const(Sexpr(Number(Int 1)))); Const(Sexpr(Bool true))])));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x x) 1)")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x . x) 1)")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;

(* Def - core form tester *)
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define)")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x 1 1)")) 
    = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x)")) 
  = (Def (Var "x", Const Void)));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x 1)")) 
  = (Def (Var "x", Const(Sexpr(Number(Int 1))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (begin 1 #t))")) 
  = (Def (Var "x", Seq [Const(Sexpr(Number(Int 1))); Const(Sexpr(Bool true))] )));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (lambda () 1))")) 
  = (Def (Var "x", LambdaSimple([], Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (lambda x 1))")) 
  = (Def (Var "x", LambdaOpt([], "x", Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (lambda (y . x) 1))")) 
  = (Def (Var "x", LambdaOpt(["y"], "x", Const(Sexpr(Number(Int 1)))))));;

(* If tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 1)")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1)")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), Const Void));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if x 1 1 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 (lambda () #t))")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), LambdaSimple([], Const(Sexpr(Bool true)))));;

(* Applic Tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(+ 1 1 1)"))
  = Applic(Var "+", [Const(Sexpr(Number(Int 1)));Const(Sexpr(Number(Int 1)));Const(Sexpr(Number(Int 1)))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "((lambda (x) x) #t)"))
  = Applic(LambdaSimple(["x"], Var "x"), [Const(Sexpr(Bool true))]));;
  assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "((lambda (x) (if x 1)) #t)"))
  = Applic(LambdaSimple(["x"], If(Var "x", Const(Sexpr(Number (Int 1))), Const Void)), 
      [Const(Sexpr(Bool true))]));;
