#use "tag-parser.ml";;

(* Atomic tags tester *)

(* Vars *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "a")) = (Var "a"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "expr")) = (Var "expr"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "+")) = (Var "+"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "eq?")) = (Var "eq?"));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "string->symbol")) 
  = (Var "string->symbol"));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "and")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "lambda")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;

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
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin 1 . 1)")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1))))))) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(begin (lambda x 1) '1 '(2) '3)")) 
  = Seq ([(LambdaOpt([], "x", Const(Sexpr(Number(Int 1))))); 
        Const(Sexpr(Number(Int 1))); Const(Sexpr(Pair(Number(Int 2), Nil))); 
        Const(Sexpr(Number(Int 3)))]));;

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
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda () '1)")) 
  = (LambdaSimple ([], (Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda () '(1))")) 
  = (LambdaSimple ([], (Const(Sexpr(Pair(Number(Int 1), Nil)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda () (if '(1) 1 '1))")) 
= (LambdaSimple ([], If(Const(Sexpr(Pair(Number(Int 1), Nil))), 
      Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda () (if '(1) 1 '1) (display #t))")) 
  = (LambdaSimple ([], 
      Seq([If(Const(Sexpr(Pair(Number(Int 1), Nil))), 
      Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1)))); 
      Applic(Var "display", [Const(Sexpr(Bool true))])]))));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(lambda (x . x) ())")) 
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
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (lambda (y . x) '1))")) 
  = (Def (Var "x", LambdaOpt(["y"], "x", Const(Sexpr(Number(Int 1)))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x '(1))")) 
  = (Def (Var "x", Const(Sexpr(Pair(Number(Int 1), Nil))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define x (begin '1 #t))")) 
  = (Def (Var "x", Seq [Const(Sexpr(Number(Int 1))); Const(Sexpr(Bool true))] )));;

(* If tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 1)")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1)")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), Const Void));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if x 1 1 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 (lambda () #t))")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), LambdaSimple([], Const(Sexpr(Bool true)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 (cons '1 '2))")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), 
      Applic(Var "cons", [Const(Sexpr(Number(Int 1)));Const(Sexpr(Number(Int 2)))])));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 (cons '1))")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))), 
      Applic(Var "cons", [Const(Sexpr(Number(Int 1)))])));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(if 1 1 '1)")) 
  = If(Const(Sexpr(Number(Int 1))), Const(Sexpr(Number(Int 1))),Const(Sexpr(Number(Int 1)))));;

(* Applic Tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(+ 1 1 1)"))
  = Applic(Var "+", [Const(Sexpr(Number(Int 1)));Const(Sexpr(Number(Int 1)));Const(Sexpr(Number(Int 1)))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "((lambda (x) x) #t)"))
  = Applic(LambdaSimple(["x"], Var "x"), [Const(Sexpr(Bool true))]));;
  assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "((lambda (x) (if x 1)) #t)"))
  = Applic(LambdaSimple(["x"], If(Var "x", Const(Sexpr(Number (Int 1))), Const Void)), 
      [Const(Sexpr(Bool true))]));;

(* Let tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let () 1)"))
  = Applic(LambdaSimple([], Const(Sexpr(Number(Int 1)))), []));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let ((x 1)) x)"))
  = Applic(LambdaSimple(["x"], Var "x"), [Const(Sexpr(Number(Int 1)))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let ((x 1) (y +) (z 2)) (y x z))"))
= Applic(LambdaSimple(["x"; "y"; "z"], Applic(Var "y", [Var "x"; Var "z"])), 
    [Const(Sexpr(Number(Int 1))); Var "+"; Const(Sexpr(Number(Int 2)))]));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let ((x . 1)) x)")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let ((x 1)) . x)")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let)")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let ((x 1)))")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(let x)")) 
    = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
  "(let ( (x '(1 2 3)) (y '(4 5 6)) ) (cons x y))"))
    = Applic(LambdaSimple(["x"; "y"], Applic(Var "cons", [Var "x"; Var "y"])), 
      [Const(Sexpr(Pair(Number (Int 1), Pair(Number (Int 2), Pair(Number (Int 3), Nil))))); 
        Const(Sexpr(Pair(Number (Int 4), Pair(Number (Int 5), Pair(Number (Int 6), Nil)))))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
  "(let ( (x (rand)) (y 2) ) (if (eq? x y) (display 'lukcy) (display 'unlucky) ))"))
    = Applic(LambdaSimple(["x"; "y"], 
        If(Applic(Var "eq?", [Var "x"; Var "y"]), 
          Applic(Var "display", [Const(Sexpr(Symbol "lukcy"))]),
          Applic (Var "display", [Const(Sexpr(Symbol "unlucky"))]))), 
      [Applic(Var "rand", []); Const(Sexpr(Number(Int 2)))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
  "(let ((x 1) (y +) (z 2)) (lambda () (y x z)))"))
    = Applic(LambdaSimple(["x"; "y"; "z"], LambdaSimple([], Applic(Var "y", [Var "x"; Var "z"]))), 
        [Const(Sexpr(Number(Int 1))); Var "+"; Const(Sexpr(Number(Int 2)))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
  "(let ((x 1) (y +) (z 2)) '(y x z))"))
    = Applic(LambdaSimple(["x"; "y"; "z"], 
      Const(Sexpr(Pair(Symbol "y", Pair(Symbol "x", Pair(Symbol "z", Nil)))))), 
        [Const(Sexpr(Number(Int 1))); Var "+"; Const(Sexpr(Number(Int 2)))]));;
        assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
"(let ((x 1) (y +) (z 2)) (lambda () '(y x z)))"))
  = Applic(LambdaSimple(["x"; "y"; "z"], 
    LambdaSimple([],Const(Sexpr(Pair(Symbol "y", Pair(Symbol "x", Pair(Symbol "z", Nil))))))), 
      [Const(Sexpr(Number(Int 1))); Var "+"; Const(Sexpr(Number(Int 2)))]));;
    
(* Or tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or)"))
  = Const(Sexpr(Bool false)));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or 'moshe)")
  = Const(Sexpr(Symbol "moshe"))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or ''moshe)")
  = Const(Sexpr(Pair(Symbol "quote", Pair(Symbol "moshe", Nil))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or 'moshe 'dan)")
  = Or[Const(Sexpr(Symbol "moshe")); Const(Sexpr(Symbol "dan"))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or #F (or 2.1 3))")
  = Or[Const(Sexpr(Bool false)); Or[Const(Sexpr(Number(Float 2.1))); Const(Sexpr(Number(Int 3)))]]));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or . 1)")) 
    = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or 1 . 2)")) 
    = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or 'moshe ''dan)")
    = Or[Const(Sexpr(Symbol "moshe")); Const(Sexpr(Pair(Symbol "quote", Pair(Symbol "dan", Nil))))]));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or 1 . ''2)")) 
    = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or (or 'moshe 'dan))")
    = Or[Const(Sexpr(Symbol "moshe")); Const(Sexpr(Symbol "dan"))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or (and 'moshe 'dan))")
    = If(Const(Sexpr(Symbol "moshe")), Const(Sexpr(Symbol "dan")), Const(Sexpr(Bool false)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or (and (or (and 'moshe 'dan))))")
    = If(Const(Sexpr(Symbol "moshe")), Const(Sexpr(Symbol "dan")), Const(Sexpr(Bool false)))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or (and (or 'moshe 'dan)))")
    = Or[Const(Sexpr(Symbol "moshe")); Const(Sexpr(Symbol "dan"))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(or (and (or ['moshe 'dan])))")
    = Applic (Const(Sexpr(Symbol "moshe")), [Const(Sexpr (Symbol "dan"))])));;

(* Set tester *)
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x 1)"))
  = Set( Var "x", Const(Sexpr(Number(Int 1)))));;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set!)")) 
  = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x)")) 
  = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x 1 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x (lambda () 1))"))
  = Set( Var "x", LambdaSimple([], Const(Sexpr(Number(Int 1)))) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x (begin #T(lambda () 1)))"))
  = Set( Var "x", Seq [Const(Sexpr(Bool true));LambdaSimple([], Const(Sexpr(Number(Int 1))))]));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr 
"(set! x (let ((x 1)) x))"))
  = Set( Var "x", Applic(LambdaSimple(["x"], Var "x"), [Const(Sexpr(Number(Int 1)))]) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x (lambda () 1))"))
  = Set( Var "x", LambdaSimple([], Const(Sexpr(Number(Int 1)))) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x '#(1 2))"))
  = Set( Var "x", Const(Sexpr(Vector [Number(Int 1); Number (Int 2)])) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x '#(1 2))"))
  = Set( Var "x", Const(Sexpr(Vector [Number(Int 1); Number (Int 2)])) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x ''moshe)"))
  = Set( Var "x", Const(Sexpr(Pair(Symbol "quote", Pair(Symbol "moshe", Nil)))) ));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(set! x (and (or #f)))"))
  = Set( Var "x", Const(Sexpr(Bool false)) ));;

(* Def MIT Style tester *)
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt . 1) 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt . 1) . 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
try assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt . x) . 1)")) 
  = (Const Void)) with X_syntax_error -> ();;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt) 1)")) 
  =  Def (Var "sqrt", LambdaSimple([], Const(Sexpr(Number(Int 1))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt . ()) 1)")) 
  =  Def (Var "sqrt", LambdaSimple([], Const(Sexpr(Number(Int 1))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (sqrt . ()) 1)")) 
  =  Def (Var "sqrt", LambdaSimple([], Const(Sexpr(Number(Int 1))))));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (car . lst) (car lst))")) 
  =  Def (Var "car", LambdaOpt([],"lst", Applic(Var "car", [Var "lst"]) )));;
assert ((Tag_Parser.tag_parse_expression (Reader.read_sexpr "(define (car . (lst . x)) (car lst))")) 
  =  Def (Var "car", LambdaOpt(["lst"],"x", Applic(Var "car", [Var "lst"]) )));;
assert ((Tag_Parser.tag_parse_expression 
  (Reader.read_sexpr "(define (sqrt . (x)) (* x x))")) 
  =  Def (Var "sqrt", LambdaSimple(["x"], Applic(Var "*", [Var "x"; Var "x"]) )));;
assert ((Tag_Parser.tag_parse_expression 
  (Reader.read_sexpr "(define (null? . (x . y)) (eq? (null) x))")) 
  =  Def (Var "null?", LambdaOpt(["x"],"y", 
      Applic(Var "eq?", [Applic(Var "null", []); Var "x"]) )));;
assert ((Tag_Parser.tag_parse_expression 
    (Reader.read_sexpr "(define (car . (lst . x)) (car lst))")) 
      =  Def (Var "car", LambdaOpt(["lst"],"x", Applic(Var "car", [Var "lst"]) )));;