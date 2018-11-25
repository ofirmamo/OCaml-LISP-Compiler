(* tag-parser.ml
 * A compiler from Scheme to CISC
 *
 * Programmer: Mayer Goldberg, 2018
 *)

#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
    "if"; "lambda"; "let"; "let*"; "letrec"; "or";
    "quasiquote"; "quote"; "set!"; "unquote";
    "unquote-splicing"];;

(* work on the tag parser starts here *)

let lazy_raise = (fun x -> raise X_syntax_error);;

(* Extract pairs into list *)
let rec pairs_to_list sexpr f =
  match sexpr with
    | Nil -> []
    | Pair(hd, tl) -> (f hd) ::  pairs_to_list tl f
    | tl -> [(f tl)];;

(* Extract the string from symbol *)
let sym_to_string s =
  match s with
    | Symbol word -> word
    | _ -> raise X_syntax_error;;
   
(* Finds the index of elemnt in list *)
let get_index lst elem =
  let rec find lst elem i = 
    match lst with
      | [] -> -1
      | hd :: _ when hd = elem -> i
      | hd :: tl -> find tl elem (i + 1) in
  find lst elem 0;;

(* Return the nth elem from list *)
let rec get_nth lst indx =
  match lst with
    | [] -> raise PC.X_no_match
    | hd :: _ when indx = 0 -> hd
    | hd :: tl -> get_nth tl (indx - 1);;
  
(* Return true iff(if and only if) word eaual to one of the reserved words *)
let is_reverved_word word = 
  ormap (fun str -> str = word ) reserved_word_list;;

(* Return true iff(if and only if) the string does'nt exist in the list *)
let is_unique word lst =
  andmap (fun str -> not (String.equal word str)) lst;;

(* Return true iff(if and only if) the sexpr is symbol *)
let is_symbol sexpr =
  match sexpr with
    | Symbol _ -> true
    | _ -> false;;

(* Returns true iff(if and only if) the expression is the empty list *)
let is_empty_list sexpr = 
  match sexpr with
    | Nil -> true
    | _ -> false;;
  
(* Return true iff(if and only if) the expression is proper list*)
let rec is_proper_list sexpr =
  match sexpr with
    | Nil -> true
    | Pair(hd, tl) -> is_proper_list tl
    | _ -> false;;

(* Returns true iff(if and only if) the expression is imporer list *)
let is_imporer_list sexpr =
  let rec is_imporer sexpr =
    match sexpr with
      | Nil -> false
      | Pair (hd, tl) -> is_imporer tl
      | _ -> true in
  match sexpr with 
    | Pair(hd, tl) -> is_imporer sexpr
    | _ -> false;;

(* Return true iff(if and only if) all vars in list are symbols and unique *)
let is_unique_symbols sexpr =
  let symbols_list = [] in
  let rec is_unique_rec sexpr lst =
    match sexpr with
      | Nil -> true
      | Symbol word -> is_unique word lst
      | Pair(Symbol word, rest) when (is_unique word lst) && not(is_reverved_word word) = true ->  
          is_unique_rec rest (word :: lst)
      | _ -> false in
  is_unique_rec sexpr symbols_list;;

(* Return true iff(if and only if) sexpr is Bool\Char\NUmber\String *)
let is_atomic_sexpr sexpr = 
  match sexpr with
    | Bool _ | Char _ | Number _ | String _ | Symbol _ -> true
    | _ -> false

(* Builds Const expr from Sexpr\Nil *)
let make_const sexpr =  
  Const(Sexpr (sexpr));;

(* Builds Var expr from string. *)
let make_var strsym =  
  Var strsym;;

(* Builds If expr from give three expr *)
let make_if expr1 expr2 expr3 = 
  If(expr1, expr2, expr3);;

(* Builds quoted\quasiqouted\unqouted\unqouted-and-spliced forms *)
let make_quoted sexpr =
  match sexpr with
    | Pair (x, Nil) -> Const(Sexpr x)
    | _ -> raise X_syntax_error;;

(* Builds LambdaSimple from given arguments *)
let make_lambda_simple strlist expr =
  LambdaSimple (strlist, expr);;

(* Builds LambdaOpt from given arguments *)
let make_lambda_opt strlist str expr =
  LambdaOpt (strlist, str, expr);;

(* Builds Def from given arguments *)
let make_def var expr =
  Def (var, expr);;

(* Builds Applic from given arguments *)
let make_applic expr exprlist =
  Applic(expr, exprlist);;

(* Builds Or from given arguments *)
let make_or exprlist =
  Or (exprlist);;

(* Builds Set expression from given arguments *)
let make_set expr1 expr2 =
    Set (expr1, expr2);;

(* Primary tag parser, split expr to atomic or compund *)
let rec rec_tag_parser sexpr = 
  match (is_atomic_sexpr sexpr) with
    | true -> atomic_tagger sexpr
    | false -> compund_tagger sexpr

(* Atomic tag parser *)
and atomic_tagger sexpr =
  match sexpr with
    | Symbol word when not(is_reverved_word word) -> make_var word
    | Symbol word -> raise X_syntax_error
    | _ -> make_const sexpr

(* Compund tag parser *)
and compund_tagger sexpr = 
  match sexpr with
    | Pair (Symbol resrved, rest) when (is_reverved_word resrved) = true -> 
      ((get_tagger resrved) rest)
    | Pair(applic, rest) when (is_proper_list sexpr) -> 
        make_applic (rec_tag_parser applic) (pairs_to_list rest rec_tag_parser)
    | _ -> raise X_syntax_error

(* Builds if expr from given sexpr 2 pattaren is available
   1. Pair(expr, Pair(expr, Nil))
   2. Pair(expr, Pair(expr, Pair(expr, Nil))) *)
and if_tagger sexpr =
  match sexpr with 
    | Pair (test , Pair (dit, Nil)) -> 
        make_if (rec_tag_parser test) (rec_tag_parser dit) (Const Void)
    | Pair (test , Pair ( dit , Pair(dif, Nil))) ->
        make_if (rec_tag_parser test) (rec_tag_parser dit) (rec_tag_parser dif)
    | _ -> raise X_syntax_error

(* Builds lambda expr from given sexpr 3 pattrens is available
  1. Pair(<arglist>, <exps>) - when arglist is proper list and exps not empty proper list
  2. Pair(<arglist>, <exps>) - when arglist is imporer list and exps not empty proper list
  3. Pair(<arglist>, exps) - when arglist is symbol and exps not empty list *)
and lambda_tagger sexpr =
  match sexpr with
    | Pair(arglist, exprs) 
        when (is_proper_list arglist) &&  (is_unique_symbols arglist)
            && (is_proper_list exprs) && (not (is_empty_list exprs)) = true ->
              make_lambda_simple (pairs_to_list arglist sym_to_string) (seq_tagger exprs)
    | Pair(arglist, exprs)
        when (is_symbol arglist) && (is_proper_list exprs)
            && (not (is_empty_list exprs)) = true ->
              make_lambda_opt [] (sym_to_string arglist) (seq_tagger exprs)
    | Pair (arglist, exprs)
        when (is_imporer_list arglist) && (is_unique_symbols arglist)
            && (is_proper_list exprs) && (not(is_empty_list exprs)) ->
              make_lambda_opt (List.rev(List.tl(List.rev(pairs_to_list arglist sym_to_string))))
                (List.hd(List.rev(pairs_to_list arglist sym_to_string))) (seq_tagger exprs)
    | _ -> raise X_syntax_error

(* Builds seq expressions from given sexpr 3 patterns is available
  1. Nil -> Builds Const Void
  2. Pair(sexpr, Nil) -> Builds Expr sexpr
  3. Pair(sexpr, Pair(...)) -> Build Seq([exp1, exp2, ..., expn]) *)
and seq_tagger sexpr = 
  match sexpr with
    | Nil -> Const Void
    | Pair(hd, Nil) -> rec_tag_parser hd
    | Pair(hd, tl) when (is_proper_list tl) -> make_seq sexpr
    | _ -> raise X_syntax_error
  
(* Builds def expressions 3 pattrens is available
  1. Pair(Symbol var, Pair(expr, Nil))  -> Core form
  2. Pair(Symbol var, Nil) -> assign Void to var
  3. Pair(Pair(Symbol name, arglist), body) - MIT def the var name is
    name and we should pass Pair(arglist, body) to lambda_tagger *)
and def_tagger sexpr = 
  match sexpr with
    | Pair(Symbol var, Pair(exp, Nil)) when not( is_reverved_word var) -> make_def (Var var) (rec_tag_parser exp)
    | Pair(Pair (Symbol name, arglist), body) when not(is_reverved_word name) -> 
        make_def (Var name) (lambda_tagger (Pair(arglist, body)))
    | _ -> raise X_syntax_error

(* Builds or expresion 3 pattren is available
  1. Nil -> false
  2. sexpr -> tag_parse sexpr
  3. Pair(sexpr, rest) -> (tag_parse sexpr) :: or(rest) *)
and or_tagger sexpr =
  match sexpr with
    | Nil -> Const(Sexpr(Bool false))
    | Pair(sexp, Nil) -> (rec_tag_parser sexp)
    | Pair(sexp, rest) when (is_proper_list rest)-> 
        make_or (pairs_to_list sexpr rec_tag_parser)
    | _ -> raise X_syntax_error

(* Builds set! expression 1 pattren is available
    1. Pair(Symbol var, expr) - when expr != Nil *)
and set_bang_tagger sexpr =
  match sexpr with
    | Pair(Symbol var, Pair(expr, Nil)) when not(is_reverved_word var) -> make_set (Var var) (rec_tag_parser expr)
    | _ -> raise X_syntax_error

(* Builds let expressions 1 pattren is available
  1. Pair(vars, body) -> will convert into applic when the operator is
                          the lambda applied w*)
and let_tagger sexpr =
  match sexpr with
    | Pair(vars, body) when not(body = Nil) -> 
        make_applic (make_lambda_simple (pairs_to_list vars to_var) (seq_tagger body))
                    (pairs_to_list vars to_val)
    | _ -> raise X_syntax_error

(* Builds let* expressions 3 pattren is available
  1. Pair(vars, body) -> will convert into applic when the operator is
                          the lambda applied w*)
and let_star_tagger sexpr =  
  match sexpr with
    | Pair (Nil, body) -> let_tagger sexpr
    | Pair (Pair(arg,Nil) ,body) ->  let_tagger sexpr
    | Pair (Pair(arg, args), body) -> 
        make_applic (make_lambda_simple [(to_var arg)] (let_star_tagger (Pair(args, body)))) [(to_val arg)]
    | _ -> raise X_syntax_error



(* Builds letrec expressions 3 pattren is available
  1. Pair(vars, body) -> will convert into applic when the operator is
                          the lambda applied w*)
and letrec_tagger sexpr = 
  let make_whatever sexp = Const(Sexpr(Symbol "whatever")) in
  let set sexp =
    match sexp with
      | Pair(Symbol name, Pair(vali, Nil)) -> make_set (Var name) (rec_tag_parser vali)
      | _ -> raise X_syntax_error in
  match sexpr with
    | Pair (Nil, body) -> let_tagger sexpr
    | Pair(args , body) when not(body = Nil)-> 
        make_applic (make_lambda_simple (pairs_to_list args to_var) 
        (Seq ((pairs_to_list args set) @ (pairs_to_list body rec_tag_parser)))) 
        (pairs_to_list args make_whatever)
    | _ -> raise X_syntax_error

(* Builds and expression with macro expansions we should valid
    that the incoming expression is proper list *)
and and_tagger sexpr = 
  match sexpr with
    | Nil -> Const(Sexpr(Bool true))
    | Pair(expr, Nil) -> rec_tag_parser expr
    | Pair(expr, rest) when (is_proper_list rest) ->
      make_if (rec_tag_parser expr) (and_tagger rest) (make_const (Bool false)) 
    | _ -> raise X_syntax_error

(* Builds cond expressions 3 pattren is available
  1. Pair(Pair(test, Pair(Symbol "=>", Pair(dit, Nil))), rest) -> 
        Will do: if test then (dit test) else (parse rest)
  2. Pair(Pair(test, dit), rest) -> 
        Will do: if test then (seq_tagger dit) else (parse rest) 
  3. Pair(Pair(Symbol "else", doit), Nil) -> (doit) *)
and cond_tagger sepxr =
  let f_apply_value rator rands = make_applic (make_applic (Var rator) []) rands in
  let apply_rest rest = make_applic rest [] in 
  let make_arrow test sexp else_clause = make_applic (make_lambda_simple ["value"; "f"] 
                   (make_if (Var "value") (f_apply_value "f" [(Var "value")]) else_clause)) 
                  [(rec_tag_parser test); (make_lambda_simple [] (seq_tagger sexp))] in
  let make_arrow2 test sexp else_clause = make_applic 
  (make_lambda_simple ["value"; "f"; "rest"] 
    (make_if (Var "value") (f_apply_value "f" [(Var "value")]) (apply_rest (Var "rest"))))
      [(rec_tag_parser test); (make_lambda_simple [] (seq_tagger sexp)); 
        (make_lambda_simple [] (cond_tagger else_clause))] in
  match sepxr with
    | Pair(Pair(Symbol "else", exp), Nil) when not(exp = Nil) -> seq_tagger exp
    | Pair (Pair(test, Pair (Symbol "=>" , sexp)),Nil )  -> 
        make_arrow test sexp (Const Void)
    | Pair(Pair(test, Pair(Symbol "=>", sexp)), rest) -> 
        make_arrow2 test sexp rest
    | Pair(Pair(test, dit), Nil) ->  make_if (rec_tag_parser test) (seq_tagger dit) (Const Void)
    | Pair(Pair(test, dit), rest) -> make_if (rec_tag_parser test) (seq_tagger dit) (cond_tagger rest)
    | _ -> raise X_syntax_error 
(* 
 Pair(Pair(Pair(Symbol "unquote-splicing", Pair(a, Nil)), b),Nil)  ->  
           make_applic (Var "append") [(quasiquote_tagger a); (quasiquote_tagger b)] *)
  and traverse_unquote_s car cdr=
    match car,cdr with
      |  Pair(Symbol "unquote-splicing", Pair(a ,Nil)), _ -> make_applic (Var "append") 
          [(rec_tag_parser a); (quasiquote_tagger (Pair(cdr, Nil))) ]
      |  _ , Pair(Symbol "unquote-splicing", Pair(b ,Nil)) ->
          make_applic ( Var "cons" ) [  (quasiquote_tagger (Pair(car, Nil))) ; (rec_tag_parser b) ] 
      |  _ , _ -> make_applic (Var "cons") [ (quasiquote_tagger (Pair(car, Nil))) ; 
                                            (quasiquote_tagger (Pair (cdr, Nil))) ]

(* comment *)
and quasiquote_tagger sexpr = 
  match sexpr with 
    | Pair(Pair(Symbol "unquote", Pair(sexp, Nil)), Nil) -> rec_tag_parser sexp
    | Pair(Pair(Symbol "unquote-splicing", Pair(sexp, Nil)), Nil) -> raise X_syntax_error

    | Pair (Vector sexp , Nil) -> 
        make_applic (Var "vector") (List.map (fun x -> quasiquote_tagger (Pair(x, Nil))) sexp)
    | Pair(Pair(car, cdr) , Nil) -> traverse_unquote_s car cdr
    | Pair (sexp, Nil)-> make_const sexp
    | _ -> raise X_syntax_error

and get_tagger word =
  let tagger_list = 
    [(*"and"*) and_tagger ; (*"begin"*) seq_tagger; (*"cond"*) cond_tagger; 
     (*"define"*) def_tagger; (*"else"*) lazy_raise; (*"if"*) if_tagger; 
     (*"lambda"*) lambda_tagger; (*"let"*)let_tagger; (*"let*"*) let_star_tagger; 
     (*"letrec"*) letrec_tagger; (*"or"*) or_tagger; (*"quasiquote"*) quasiquote_tagger; 
     (*"quote"*) make_quoted; (*"set!"*) set_bang_tagger; (*"unquote"*) lazy_raise;
     (*"unquote-splicing"*) lazy_raise] in
  get_nth tagger_list (get_index reserved_word_list word)

and to_var sexp =
  match sexp with
    | Pair (Symbol var, Pair(vali, Nil)) when not(is_reverved_word var) -> var
    | _ -> raise X_syntax_error 

and to_val sexp =
  match sexp with
    | Pair(Symbol var, Pair(vali, Nil)) -> rec_tag_parser vali
    | _ -> raise X_syntax_error

(* Builds Seq from given arg *)
and make_seq sexpr =
  if (is_proper_list sexpr)
  then Seq (pairs_to_list sexpr rec_tag_parser)
  else raise X_syntax_error;;

let tag_parse_expression sexpr = rec_tag_parser sexpr;;
          
let tag_parse_expressions sexpr = 
  List.map (fun sexp -> tag_parse_expression sexp) sexpr;;
  
end;; (* struct Tag_Parser *)
