module CommentParser: sig
  val simple_comment_parser : char list -> sexpr * char list
end

= struct

let _spaces_ = PC.star (PC.range '\000' ' ');;

(* Line Comment Parser *)
let simple_comment_parser = 
  (* Greater than newline *)
  let _gtnewline_ = PC.range '\011' '\255' in
  (* Less than newline *)
  let _ltnewline_ = PC.range '\000' '\009' in
  (* Semicolon (;) parser *)
  let _semicolon_ = PC.pack (PC.char ';') (fun c -> [c]) in
  (* Valid comment chars parser *)
  let _commentchs_ = PC.star (PC.disj _gtnewline_ _ltnewline_) in
  (* Newline parser - return list coz OCaml type system *)
  let _newline_ = PC.pack (PC.char '\n') (fun c -> [c]) in
  (* End of comment parser *)
  let _commentend_ = PC.disj _newline_ PC.nt_end_of_input in
  (* Real comment parser *)
  let _comment_ = PC.caten (PC.caten _semicolon_ _commentchs_) _commentend_ in
  (*  Packing... *)
    (PC.pack _comment_ (fun ((s, m), e) -> Nil));;

end;;