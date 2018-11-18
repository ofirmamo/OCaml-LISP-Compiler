
module NumberParser : sig
  val parser: char list -> sexpr * char list
end
= struct

(* Convert Numeric Char to Real Value *)
let make_char_value base_char displacement =
  let base_char_value = Char.code base_char in
  fun ch -> (Char.code ch) - base_char_value + displacement;;

(* Decimal Digits Parser *)
let _digit_0_9_ = PC.pack (PC.range '0' '9') (make_char_value '0' 0);;

(* Hex Lower Digits Parser *)
let _digit_a_f_ = PC.pack (PC.range 'a' 'f') (make_char_value 'a' 10);;

(* Hex Capital Digits Parser *)
let _digit_A_F_ = PC.pack (PC.range 'A' 'F') (make_char_value 'A' 10);;

(* Decimal Natural Number Parser *)
let _nat_dec_ = 
  let _chdigits_ = PC.plus _digit_0_9_ in
  (PC.pack _chdigits_ 
    (fun ns -> List.fold_left (fun acc elem -> acc * 10 + elem) 0 ns));;

(* Hexadecimal Natural Number Parser *)
let _nat_hex_ = 
  let _chdigits_ = PC.plus (PC.disj_list [_digit_0_9_; _digit_a_f_; _digit_A_F_]) in
  (PC.pack _chdigits_ 
    (fun ns -> List.fold_left (fun acc elem -> acc * 16 + elem) 0 ns));;

(* Float Decimal *)
let _flt_dec_ = 
  let _chdigits_ = PC.plus _digit_0_9_ in
  (PC.pack _chdigits_ 
    (fun ns -> List.fold_right (fun elem acc -> (acc +. float_of_int(elem))  /. 10.0) ns 0.0));;

(* Float Hexadecimal *)
let _flt_hex_ = 
  let _chdigits_ = PC.plus (PC.disj_list [_digit_0_9_; _digit_a_f_; _digit_A_F_]) in
  (PC.pack _chdigits_ 
  (fun ns -> List.fold_right (fun elem acc -> (acc +. float_of_int(elem))  /. 16.0) ns 0.0));;

(* Impl of Integer Calculator *)
let _int_ parser = 
  let _pm_ = PC.one_of "+-" in
  let _signed_ = PC.caten _pm_  parser in
  let _unsigned_ = (fun (nchs) -> (PC.caten (PC.char '+') parser) ('+' :: nchs)) in
  let _inted_ = PC.disj _signed_ _unsigned_ in
  PC.pack _inted_ (fun (s, n) -> match s with
                            | '+' -> n
                            | _ -> -n);;
      
(* Impl of Float Calculator *)
let _flt_ bigs_parser smalls_parser = 
  let _dot_ = PC.char '.' in
  let _pm_ = PC.one_of "+-" in
  let _signed_ = PC.caten (PC.caten (PC.caten _pm_ bigs_parser) _dot_) smalls_parser in
  let _unsigned_ = (fun (nchs) -> 
    (PC.caten (PC.caten (PC.caten (PC.char '+') bigs_parser) _dot_) smalls_parser) ('+' :: nchs)) in
  let _floated_ = PC.disj _signed_ _unsigned_ in
  PC.pack _floated_ (fun (((s, n), d), sn) -> match s with
                                              | '+' -> float_of_int(n) +. sn
                                              | _ -> -.(float_of_int(n) +. sn));;

(* Impl of Integer *)
let _integer_ = 
  let _hexprefix_ = PC.word_ci "#x" in
  let _is_hex_ = PC.maybe _hexprefix_ in
  (PC.pack (fun (chl) -> match _is_hex_ chl with
                        | (Some _, n) -> _int_ _nat_hex_ n
                        | (None, n) -> _int_ _nat_dec_ n)
            (fun (n) -> Number(Int(n))));;

(* Impl of Float *)
let _float_ = 
  let _hexprefix_ = PC.word_ci "#x" in
  let _is_hex_ = PC.maybe _hexprefix_ in
  (PC.pack (fun (chl) -> match _is_hex_ chl with
                        | (Some _, n) -> _flt_ _nat_hex_ _flt_hex_ n
                        | (None, n) -> _flt_ _nat_dec_ _flt_dec_ n)
      (fun (n) -> Number(Float(n))));;

(* Scientific Notation numbers *)
(* 
  <ScientificNotation> = (<Integer> | <Float> ) <ScientificChar> <Integer>
  <ScientificChar> = (e | E)
*)
let _scientific_char_ = PC.char_ci 'e';;

let _scientific_notation_ =
  let _integer_sn_ = PC.pack (_int_ _nat_dec_) (fun n -> Number(Int(n))) in
  let _flt_sn_ = PC.pack (_flt_ _nat_dec_ _flt_dec_) (fun n -> Number(Float (n))) in
  let _dec_integer_ = PC.disj _flt_sn_ _integer_sn_ in
  let _sned_ = PC.caten _dec_integer_ (PC.caten _scientific_char_ _integer_sn_) in
  PC.pack _sned_ (fun (n, (_, e)) -> match n,e with
                                      | Number(Int(num)), Number(Int exp) -> 
                                        Number(Float((float_of_int num) *. (10.0 ** (float_of_int (exp)))))
                                      | Number(Float(num)), Number(Int(exp)) ->
                                        Number(Float(num *. (10.0 ** (float_of_int (exp)))))
                                      | _ -> raise PC.X_no_match);;

let parser = 
    (PC.not_followed_by (PC.disj_list [_scientific_notation_;_float_; _integer_]) 
    (PC.disj_list [BoolParser.parser; CharParser.parser; SymbolParser.parser]));;

end;; (* Module Number Parser *)