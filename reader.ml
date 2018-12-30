
(* reader.ml
 * A compiler from Scheme to x86/64
 *
 * Programmer: Mayer Goldberg, 2018
 *)

 #use "sexpr.ml";;
 #use "commentsparser.ml";;
   
 module Reader: sig
   val read_sexpr : string -> sexpr
   val read_sexprs : string -> sexpr list
   val sexpr_comment_parser : char list -> sexpr * char list
   val test_simple_comment: char list -> sexpr * char list (* TODO delete before submission *)
 end
 = struct
 let normalize_scheme_symbol str =
   let s = string_to_list str in
   if (andmap
   (fun ch -> (ch = (lowercase_ascii ch)))
   s) then str
   else Printf.sprintf "|%s|" str;;

let _spaces_ = PC.star (PC.range '\000' ' ');;
let _lparen_ = PC.word "(";;
let _rparen_ = PC.word ")";;
let _lbparen_ = PC.word "[";;
let _rbparen_ = PC.word "]";;
let _dot_ = PC.word ".";;
let _hashtag_ = PC.word "#";;
let _dots_  = PC.word "..." ;;

let _quot_ = PC.pack (PC.word "'") (fun q -> Symbol "quote") ;;
let _qquot_ = PC.pack (PC.word "`") (fun q -> Symbol "quasiquote") ;;
let _unspquot_ = PC.pack (PC.word ",@") (fun q -> Symbol "unquote-splicing") ;;
let _unquot_ = PC.pack (PC.word ",") (fun q -> Symbol "unquote") ;;
let _all_quoted_ = PC.disj_list [_quot_;_qquot_; _unspquot_; _unquot_] ;;

(* Simple comment catenate with spaces *)
let _scomment_ = 
  let _sm_ = PC.caten (PC.caten _spaces_ CommentParser.simple_comment_parser) _spaces_ in
  PC.pack _sm_ (fun ((_, _), _) -> Nil);;

(* Regular expression before sexprssion commit *)
(* <spaces>* (<spaces>* <line_comment> <sapces>* )* <spaces>* *)
let _scwithsps_ = 
  (* <spaces>* (<spaces>* <line_comment> <sapces>* )* <spaces>* *)
  let _catenated_ = 
    PC.caten (PC.caten _spaces_ (PC.star _scomment_)) _spaces_ in 
  (* Packing... *)
  PC.pack _catenated_ (fun ((_, _), _) -> Nil);;

(* Sexpr Prefix Comment Parser *)
let _hwsc_ = 
  let _cosign_ = PC.word "#;" in
  PC.caten (PC.caten _scwithsps_ _cosign_) _scwithsps_ ;;


 (* White spaces prasers *)
 let _stared_spaces_ = PC.pack (PC.star (PC.range '\000' ' ')) (fun c -> Nil);;
 let _plused_spaces_ = PC.pack (PC.plus (PC.range '\000' ' ')) (fun c -> Nil);;
 
 (* Simple comment parsers *)
 let _stared_simple_comment_ = 
   PC.pack (PC.star (CommentParser.simple_comment_parser)) (fun _ -> Nil);;
 let _plused_simple_comment_ = 
   PC.pack (PC.plus (CommentParser.simple_comment_parser)) (fun _ -> Nil);;

(* END *)

(* Sexpr parser *)
let rec _sexpr_ chl = 
  (PC.disj_list [SExpParser.parser; _compound_sexpr_]) chl
and _compound_sexpr_ chl = 
    (PC.disj_list [ _vector_; _list_; _dotedlist_ ;_quoted_ ]) chl

and _nested_sexpr_ chl = 
    (PC.disj_list [SExpParser.parser; _nested_compound_sexpr_]) chl

and _nested_compound_sexpr_ chl = 
    (PC.disj_list [_nested_vector_; _nested_dotedlist_ ;_nested_list_ ;_nested_quoted_]) chl
(* Sexpr parser regular expression *)
and _sexp_parser_sp_  chl =
  (* <spaces>* <sexpr> <spaces>* *) 
  (PC.pack (PC.caten (PC.caten _scwithsps_ _sexpr_) _scwithsps_)
    (fun ((_, s), _) -> s)) chl

and _sexps_ cdepth edepth chl =
(* Base case if #(#;) = #(sexpr) return with success *)
if cdepth = edepth 
then (Nil, chl)
(* Else, #(#;) > #(sexpr) continue to parse text.. *)
else match (PC.maybe _hwsc_) chl  with
      (* There is more #; call parser with #(#;) + 1 *)
      | (Some _, ch) -> _sexps_ (cdepth + 1) edepth ch
      | (None, ch) -> 
              (* There is sexpr call parser with #(sexpr) + 1 *)
                  let (s, r) = _sexp_parser_sp_ ch in
                  _sexps_ cdepth (edepth + 1) r

(* Sexpr comment praser *)
and sexpr_comment_parser chl = 
  (PC.pack  (fun chl -> match (PC.maybe (PC.word "#;")) chl with
                        | (Some _, r) -> _sexps_ 1 0 r
                        | (None, _) -> raise PC.X_no_match) 
      (fun _ -> Nil)) chl

and _list_ chl =
    let _pwd_ p = PC.pack (PC.caten (PC.maybe p) (PC.caten _valid_ _dots_)) (fun (_, (_, _)) -> []) in
    let _skip_ =  PC.pack (PC.caten _valid_ (PC.caten (PC.star _one_sexpr_) _valid_))
                (fun (_, (sxpl, _)) -> sxpl) in
    let _nested_skip_ = PC.pack (PC.caten _valid_ (PC.caten (PC.star _one_nested_sexpr_) _valid_))
                (fun (_, (sxpl, _)) -> sxpl) in
    let _listed_ = PC.caten _lparen_ (PC.caten  _skip_ _rparen_) in
    let _listed_barcket_ = PC.caten _lbparen_ (PC.caten _skip_ _rbparen_) in
    let _listed_doted_ = PC.caten _lparen_ (PC.caten _nested_skip_ (_pwd_ _rparen_)) in
    let _listed_bdoted_ = PC.caten _lbparen_ (PC.caten _nested_skip_ (_pwd_ _rbparen_)) in

    (PC.pack (PC.disj_list [_listed_ ; _listed_barcket_; _listed_bdoted_;_listed_doted_])
      (fun (lp ,(sxprl , rp)) -> 
        List.fold_right (fun elem acc -> Pair (elem, acc)) sxprl Nil)) chl

and _nested_list_ chl = 
    let _nested_skip_ = PC.pack (PC.caten _valid_ (PC.caten (PC.star _one_nested_sexpr_) _valid_))
                (fun (_, (sxpl, _)) -> sxpl) in
    let _listed_n_ = PC.caten _lparen_ (PC.caten  _nested_skip_ (PC.maybe _rparen_) ) in
    let _listed_barcket_n_ = PC.caten _lbparen_ (PC.caten _nested_skip_ (PC.maybe _rbparen_) ) in

     (PC.pack (PC.disj_list [_listed_n_ ; _listed_barcket_n_ ])
      (fun (lp ,(sxprl , rp)) -> 
        List.fold_right (fun elem acc -> Pair (elem, acc)) sxprl Nil)) chl



and _one_sexpr_ chl = 
   let _sexpred_ = PC.caten _valid_ (PC.caten _sexpr_ _valid_) in
   (PC.pack _sexpred_ (fun (_, (exp, _)) -> exp)) chl

and _one_nested_sexpr_ chl = 
   let _sexpred_ = PC.caten _valid_ (PC.caten _nested_sexpr_ _valid_) in
   (PC.pack _sexpred_ (fun (_, (exp, _)) -> exp)) chl
 
and _multi_sexpr_ chl = 
   if chl = []
   then []
   else let (exp, r) = _one_sexpr_ chl in
         exp :: (_multi_sexpr_ r)

and _try_disj_ chl = (PC.disj_list [_try_spaces_; 
        _try_simple_comment_; _try_sexpr_comment_]) chl


(* Sexpr comments parsers *)
and _stared_sexpr_comment_ chl = 
   (PC.pack (PC.star (sexpr_comment_parser)) (fun _ -> Nil)) chl

and _plused_sexpr_comment_ chl = 
  (PC.pack (PC.plus (sexpr_comment_parser)) (fun _ -> Nil)) chl

and _valid_ chl = 
   let _try_star_ = PC.star _try_disj_ in
   (PC.pack _try_star_ (fun _ -> Nil)) chl

and _try_spaces_ chl = 
  let _try_ = PC.caten _plused_spaces_
        (PC.caten _stared_simple_comment_ _stared_sexpr_comment_) in
   (PC.pack _try_ (fun (_, (_, _)) -> Nil)) chl
 
and _try_simple_comment_ chl = 
  let _try_ = PC.caten _stared_spaces_
        (PC.caten _plused_simple_comment_ _stared_sexpr_comment_) in
 (PC.pack _try_ (fun (_, (_, _)) -> Nil)) chl
 
and _try_sexpr_comment_ chl = 
   let _try_ = PC.caten _stared_spaces_
         (PC.caten _stared_simple_comment_ _plused_sexpr_comment_) in
 (PC.pack _try_ (fun (_, (_, _)) -> Nil)) chl


and _vector_ chl = 
  let _skip_ =  PC.pack (PC.caten _valid_ (PC.caten (PC.star _one_sexpr_) _valid_))
                (fun (_, (sxpl, _)) -> sxpl) in
  let _nested_skip_= PC.pack (PC.caten _valid_ (PC.caten (PC.star _one_nested_sexpr_) _valid_))  
                (fun (_, (nsxpl, _)) -> nsxpl)  in 
  let _hashtagged_closed_ = PC.caten (PC.word "#(") 
              (PC.caten _skip_ _rparen_) in
  let _hashtagged_doted_ = PC.caten (PC.word "#(") 
              (PC.caten _nested_skip_ _dots_) in
  let _sexp_or_nested_ = PC.disj_list [_hashtagged_doted_ ;_hashtagged_closed_  ] in 
  (PC.pack _sexp_or_nested_ (fun (hslp, (sexprl , rp_dots)) -> Vector sexprl)) chl


and _nested_vector_ chl = 
  let _nested_skip_ =  PC.pack (PC.caten _valid_ (PC.caten (PC.star  _one_nested_sexpr_) _valid_))
                (fun (_, (sxpl, _)) -> sxpl) in
  let _hashtagged_closed_m_ = PC.caten (PC.word "#(") 
              (PC.caten _nested_skip_ (PC.maybe _rparen_)) in  
  (PC.pack _hashtagged_closed_m_ (fun (hslp , (nsexprl , closedm)) -> Vector nsexprl)) chl 



and _dotedlist_ chl = 
  let _pwd_ p = PC.pack (PC.caten (PC.maybe p) (PC.caten _valid_ _dots_)) (fun (_, (_, _)) -> []) in
  let _parend_ = PC.caten _lparen_ (PC.caten (PC.plus  _one_sexpr_)
                           (PC.caten _dot_ (PC.caten _one_sexpr_  _rparen_) ) ) in 

  let _parend_barcket_ = PC.caten _lbparen_ (PC.caten (PC.plus  _one_sexpr_)
                          (PC.caten _dot_ (PC.caten _one_sexpr_  _rbparen_) ) ) in 

  let _parend_3dot_ = PC.caten _lparen_  (PC.caten ( PC.plus _one_nested_sexpr_)   
                           (PC.caten _dot_ ( PC.caten _one_nested_sexpr_ (_pwd_ _rparen_)) ) )in
  let _parend_b3dot_ = PC.caten _lbparen_  (PC.caten ( PC.plus _one_nested_sexpr_)   
                           (PC.caten _dot_ ( PC.caten _one_nested_sexpr_ (_pwd_ _rbparen_)) ) )in 

  (PC.pack (PC.disj_list [_parend_b3dot_;_parend_3dot_;_parend_ ; _parend_barcket_])
    (fun (lp, (sxprl, (dot, (sexpr , rp)))) -> 
      List.fold_right (fun elem acc -> Pair (elem , acc)) sxprl sexpr)) chl

and _nested_dotedlist_ chl = 
  let _parend_ = PC.caten _lparen_ (PC.caten (PC.plus  _one_nested_sexpr_)
                           (PC.caten _dot_ (PC.caten _one_nested_sexpr_ (PC.maybe  _rparen_)   ) ) )in 
  let _parend_barcket_ = PC.caten _lbparen_ (PC.caten (PC.plus  _one_nested_sexpr_)
                          (PC.caten _dot_ (PC.caten _one_nested_sexpr_  (PC.maybe _rbparen_) ) ) ) in

  (PC.pack (PC.disj_list [_parend_ ; _parend_barcket_])
    (fun (lp, (sxprl, (dot, (sexpr , rp)))) -> 
      List.fold_right (fun elem acc -> Pair (elem , acc)) sxprl sexpr)) chl

and _quoted_ chl =  (PC.pack (PC.caten _all_quoted_ _one_sexpr_) 
    (fun (qt, sxpr) -> Pair (qt , Pair (sxpr, Nil)))) chl

and _nested_quoted_ chl = (PC.pack (PC.caten _all_quoted_ _one_nested_sexpr_) 
    (fun (qt, sxpr) -> Pair (qt , Pair (sxpr, Nil)))) chl;;



(* nested _quated  : TODO ! *)

(* END *)

 let read_sexpr string = 
   match _one_sexpr_ (string_to_list string) with
   | (exp, []) -> exp
   | _ -> raise PC.X_no_match;;
   
 
 let read_sexprs string = 
   match _valid_ (string_to_list string) with
   | (Nil, []) -> []
   | _ -> _multi_sexpr_ (string_to_list string);;
 
 (* TODO Delete before submissiom *)
 let test_simple_comment t =
   CommentParser.simple_comment_parser t;;
 
 end;; (* struct Reader *)

