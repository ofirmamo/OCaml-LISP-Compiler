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

let car (first, second) = first;;
let cdr (first, second) = second;;
  
(* Returns  index of element within list, -1 if now found *)
let get_index lst elem =
  let rec find lst elem i = 
    match lst with
      | [] -> -1
      | hd :: _ when hd = elem -> i
      | hd :: tl -> find tl elem (i + 1) in
  find lst elem 0;;

(* Finds the index of elemnt in list *)
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

let rec annotate_tail_rec is_tp exprt =
  match exprt with
  | Applic' (expr, exprlst) when is_tp -> 
      ApplicTP'((annotate_tail_rec false expr), (List.map (annotate_tail_rec false) exprlst))
  | Applic' (expr ,exprlst) -> 
      Applic'((annotate_tail_rec false expr), (List.map (annotate_tail_rec false) exprlst))
  | LambdaSimple' (strlst, body) -> LambdaSimple' (strlst, (annotate_tail_rec true body))
  | LambdaOpt' (stlst , str , body) -> LambdaOpt' (stlst, str, (annotate_tail_rec true body))
  | Or' (exprlst) when is_tp -> Or'(annotate_tail_lst (exprlst))
  | Or' (exprlst) -> Or'(List.map (annotate_tail_rec false) exprlst)
  | Def' (expr1 , expr2) -> Def'((annotate_tail_rec false expr1 ), (annotate_tail_rec false expr2) )
  | Set' (expr1 , expr2) -> Set'((annotate_tail_rec false expr1 ), (annotate_tail_rec false expr2))
  | Seq' (exprlst) when is_tp -> Seq'(annotate_tail_lst (exprlst))
  | Seq' (exprlst) -> Seq'(List.map (annotate_tail_rec false) exprlst)
  | Var' (expr) -> Var'(expr)
  | Const' (expr) -> Const'(expr)
  | If'(test, dit, dif) -> 
      If'((annotate_tail_rec false test), 
          (annotate_tail_rec is_tp dit), 
          (annotate_tail_rec is_tp dif))
  | _ -> raise X_syntax_error

and annotate_tail_lst exprlst = 
  List.mapi 
    (fun i expr -> if (i = (List.length exprlst - 1 )) 
      then (annotate_tail_rec true expr) 
        else (annotate_tail_rec false expr)) exprlst;;
 
let rec annotate_box expr = 
  match expr with 
    | BoxSet'(vari, vali) -> BoxSet'(vari, (annotate_box vali))
    | Set'(vari, vali) -> Set'((annotate_box vari), (annotate_box vali))
    | If'(test, dit, dif) ->
        If'((annotate_box test), (annotate_box dit), (annotate_box dif)) 
    | Seq'(lst) -> Seq'((List.map annotate_box lst))
    | Def'(vari, vali) -> Def' ((annotate_box vari), (annotate_box vali))
    | Or'(lst) -> Or'((List.map annotate_box lst))
    | LambdaSimple'(params, body) -> LambdaSimple'(params, annotate_box((box_process body params)))
    | LambdaOpt'(params, lst, body) -> LambdaOpt'(params, lst, annotate_box ((box_process body (params @ [lst]))))
    | Applic'(rator, rands) -> Applic'((annotate_box  rator), (List.map annotate_box rands))
    | ApplicTP'(rator, rands) -> ApplicTP'((annotate_box  rator), (List.map annotate_box rands))
    | _ -> expr

and box_process body params =
  let to_box = get_params_to_box body params in
  let body_prefix = 
    List.map (fun n -> Set'(Var'(VarParam(n, (get_index params n))), 
    Box'(VarParam(n, (get_index params n))))) to_box in
  match to_box, body with
    | [], _ -> body
    | _, _ -> Seq'(body_prefix @ [(do_box to_box body)])
  
and get_params_to_box body params =
    let to_box = 
      collect_params_to_box body (List.map (fun n -> (n, false, false, false, false, true, false)) params) in
    (* p to_box; *)
    List.filter (fun n -> is_var_should_box to_box n) params

and setRP (n, rp, wp, rb, wb, acc, ans) = (n, true, wp, rb, wb, acc, wb || ans)
and setWP (n, rp, wp, rb, wb, acc, ans) = (n, rp, true, rb, wb, acc, rb || ans)
and setRB (n, rp, wp, rb, wb, acc, ans) = (n, rp, wp, true, wb, acc, wp || ans)
and setWB (n, rp, wp, rb, wb, acc, ans) = (n, rp, wp, rb, true, acc, rp || ans)
and setACC (n, rp, wp, rb, wb, acc, ans) = (n, rp, wp, rb, wb, false, ans)
and setTACC vali (n, rp, wp, rb, wb, acc, ans) = (n, rp, wp, rb, wb, vali, ans)
and getRP (n, rp, wp, rb, wb, acc, ans) = rp
and getWP (n, rp, wp, rb, wb, acc, ans) = wp
and getRB (n, rp, wp, rb, wb, acc, ans) = rb
and getWB (n, rp, wp, rb, wb, acc, ans) = wb
and getACC (n, rp, wp, rb, wb, acc, ans) = acc
and getANS (n, rp, wp, rb, wb, acc, ans) = ans
and getNAME (n, rp, wp, rb, wb, acc, ans) = n
and is_name_equal (n, rp, wp, rb, wb, acc, ans) name = (name = n)
and is_var_avail obj name =
  List.exists (fun (n, rp, wp, rb, wb, acc, ans) -> (n = name) && acc) obj
and is_var_should_box obj name =
  List.exists (fun (n, rp, wp, rb, wb, acc, ans) -> (n = name) && ans) obj
and set_var obj n f = 
  List.map (fun obji -> if (is_name_equal obji n) then (f obji) else obji) obj
and disable_unavail_vars new_params obj =
      List.map (fun obji -> if (is_name_exists (getNAME obji) new_params) 
                  then (setACC obji) else obji) obj
and is_name_exists name lst = List.exists (fun n -> n = name) lst
and printT (n, rp, wp, rb, wb, acc, ans) = Printf.printf "\t-n:%s rp:%B wp:%B rb:%B wb:%B acc:%B ans:%B\n" n rp wp rb wb acc ans
and p obj = Printf.printf "obj:\n"; List.iter (fun o -> printT o) obj
and collect_params_to_box expr obj =
    match expr with 
      | Var'(VarParam (n, i)) when (is_var_avail obj n) -> set_var obj n setRP
      | Set'(Var'(VarParam(n, i)), e) when (is_var_avail obj n) -> 
          collect_params_to_box e (set_var obj n setWP)
      | BoxSet'(vari, vali) -> collect_params_to_box vali obj
      | Set'(vari, vali) -> collect_params_to_box vali (collect_params_to_box vari obj)
      | If'(test, dit, dif) -> 
          collect_params_to_box dif (collect_params_to_box dit (collect_params_to_box test obj))
      | Seq'(lst) | Or'(lst) -> 
          List.fold_left (fun acc e -> (collect_params_to_box e acc)) obj lst
      | Def'(vari, vali) -> collect_params_to_box vali obj
      | Applic'(rator, rands) | ApplicTP'(rator, rands) -> 
          collect_params_to_box rator 
            (List.fold_left (fun acc e -> (collect_params_to_box e acc)) obj rands)
      | LambdaSimple'(params, body) -> collect_status body (disable_unavail_vars params obj)
      | LambdaOpt'(params, vs, body) -> collect_status body (disable_unavail_vars (params @ [vs]) obj)
      | _ -> obj

and collect_status body obj = 
  let obj2 = on_body body 
    (List.map (fun ((n, rp, wp, rb, wb, acc, ans)) -> (n, rp, wp, false, false, acc, ans)) obj) in
  List.map2 (fun old curr -> 
    ((getNAME old), (getRP old), (getWP old), ((getRB old) ||(getRB curr)), 
      ((getWB old) || (getWB curr)), true, 
        ((getANS curr) || ((getRB old) && (getWB curr)) || ((getWB old) && (getRB curr))))) obj obj2

and on_body body obj = 
  match body with
    | Var'(VarBound(n, i, j)) when (is_var_avail obj n) -> set_var obj n setRB
    | Set'(Var'(VarBound(n, i, j)), e) when (is_var_avail obj n) -> 
        on_body e (set_var obj n setWB)
    | Set'(vari, vali) | Def'(vari, vali) -> on_body vali (on_body vari obj)
    | BoxSet'(vari, vali) -> on_body vali obj
    | If'(test, dit, dif) -> 
        on_body dif (resotreACC obj (on_body dit (resotreACC obj (on_body test obj))))
    | Seq'(lst) | Or'(lst) -> 
        (resotreACC obj (List.fold_left (fun acc e -> on_body e (resotreACC obj acc)) obj lst))
    | Applic'(rator, rands) | ApplicTP'(rator, rands) ->
        on_body rator (resotreACC obj ((List.fold_left (fun acc e -> on_body e (resotreACC obj acc)) obj rands)))
    | LambdaSimple'(new_params, bdy) -> 
        on_body bdy (disable_unavail_vars new_params obj)
    | LambdaOpt'(new_params, vs, bdy) -> 
        on_body bdy (disable_unavail_vars (new_params @ [vs]) obj)
    | _ -> obj

and resotreACC old curr = 
  List.map2 (fun oldi curri -> (setTACC (getACC oldi) curri)) old curr

and is_exists lst name = List.exists (fun e -> e = name) lst
and remove_duplicates l c = List.filter (fun e -> not(is_exists c e)) l
and do_box params expr = 
    match expr with 
      | Var'(VarParam (n, i)) when (is_exists params n) -> BoxGet'(VarParam(n, i))
      | Var'(VarBound (n, i, j)) when (is_exists params n) -> BoxGet'(VarBound(n, i, j))
      | Set'(Var'(VarParam(n, i)), expr) when (is_exists params n) -> 
          BoxSet'(VarParam(n, i), (do_box params expr))
      | Set'(Var'(VarBound(n, i, j)), expr) when (is_exists params n) -> 
          BoxSet'(VarBound(n, i, j), (do_box params expr))
      | Set'(vari, vali) -> Set'((do_box params vari), (do_box params vali))
      | BoxSet'(vari, vali) -> BoxSet'(vari, (do_box params vali))
      | If'(test, dit, dif) ->
          If'((do_box params test), (do_box params dit), (do_box params dif))
      | Seq'(lst) -> Seq'((List.map (do_box params) lst))
      | Def'(vari, vali) -> Def'((do_box params vari), (do_box params vali))
      | Or'(lst) -> Or'(List.map (do_box params) lst)
      | Applic'(rator, rands) -> Applic'((do_box params rator), (List.map (do_box params) rands))
      | ApplicTP'(rator, rands) -> ApplicTP'((do_box params rator), (List.map (do_box params) rands))
      | LambdaSimple'(new_params, body) -> 
          LambdaSimple'(new_params, (do_box (remove_duplicates params new_params) body))
      | LambdaOpt'(new_params, vs, body) -> 
          LambdaOpt'(new_params, vs, (do_box (remove_duplicates (params @ [vs]) new_params) body))
      | _ -> expr;;

let annotate_lexical_addresses e = annotate_rec [] [] e;;

let annotate_tail_calls e = annotate_tail_rec false e;;

let box_set e = annotate_box e;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)



