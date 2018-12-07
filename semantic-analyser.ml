(* tag-parser.ml
 * A compiler from Scheme to CISC
 *
 * Programmer: Mayer Goldberg, 2018
 *)

#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;

exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct
  
(* Returns  index of element within list, -1 if now found *)
let get_index lst elem =
  let rec find lst elem i = 
    match lst with
      | [] -> -1
      | hd :: _ when hd = elem -> i
      | hd :: tl -> find tl elem (i + 1) in
  find lst elem 0;;

(* Returns index of param within params list, assuning element exist! *)
let get_param_index s params = Var'(VarParam  (s, (get_index params s)));;

(* Returns tru iff s is within the lexical enviorments *)
let is_bound s bounds = 
  List.fold_left (fun acc elem -> (acc || (List.exists (String.equal s) elem) )) false bounds;;

(* Returns major and minor element of bound variable, assuming variable exist! *)
let rec get_bound_index s bounds mj_indx =
  match bounds with
    | [] -> raise Not_found
    | hd :: tl when (List.exists (String.equal s) hd)-> Var'(VarBound(s, mj_indx, (get_index hd s))) 
    | hd :: tl -> get_bound_index s tl (mj_indx + 1);;

(* Annotate variable to Param, Bound or Free *)
let annotate_var s params bounds =
  if List.exists (String.equal  s) params then (get_param_index s params) 
  else if (is_bound s bounds) then (get_bound_index s bounds 0)
  else Var'(VarFree s);;
  
(* Annotate lexical addres for given express *)
let rec annotate_rec params bounds expr =
  match expr with
    | Const c -> Const' c
    | Var v -> annotate_var v params bounds
    | If (test , th ,el) -> 
        If' ((annotate_rec params bounds test), 
            (annotate_rec params bounds th) , 
            (annotate_rec params bounds el))  
    | Seq(exprlst) -> 
        Seq' (List.map (annotate_rec params bounds) exprlst)
    | Set (vari, vali) -> 
        Set'((annotate_rec params bounds vari), 
            ( annotate_rec params bounds vali))
    | Def(vari, vali) -> 
        Def' ((annotate_rec params bounds vari), 
              (annotate_rec params bounds vali))
    | Or(exprlst) -> 
        Or' ((List.map (annotate_rec params bounds) exprlst))
    | Applic (expr, exprlst) -> 
        Applic'((annotate_rec params bounds expr), 
                (List.map (annotate_rec params bounds) exprlst))
    | LambdaSimple(strlst, expr) -> 
        LambdaSimple' (strlst, 
                      (annotate_rec strlst (params :: bounds) expr))
    | LambdaOpt(strlst, lst, expr) -> 
        LambdaOpt'(strlst, lst,
                  (annotate_rec (strlst @ [lst]) (params :: bounds) expr));;

let annotate_lexical_addresses e = annotate_rec [] [] e;;

let annotate_tail_calls e = raise X_not_yet_implemented;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)

