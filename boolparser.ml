module BoolParser: sig
  val parser: char list -> sexpr * char list
end
= struct

let _hashtag_ = PC.char '#';;                (* Hashtags *)

(* Bool parser *)
let _booled_ = 
  let _true_ = PC.char_ci 't' in                (* Case insensitive *)
  let _false_ = PC.char_ci 'f' in               (* Case insensitive *)
  PC.caten _hashtag_ (PC.disj _true_ _false_)   (* Implmentation *)

let parser = 
    PC.pack (PC.not_followed_by _booled_ SymbolParser.parser)
      (fun ((_, b)) -> if (lowercase_ascii b) = 't' then Bool(true) else Bool(false));;

end;; (* Struct BoolParser *)