
module CharParser: sig
  val parser: char list -> sexpr * char list
end
= struct

let _pack_named_ named = 
  PC.pack (PC.word_ci named) (fun chl -> List.map(fun c -> lowercase_ascii c) chl);;

(* Named Chars Concrete parsers *)
let _newline_ = _pack_named_ "newline";;
let _return_ = _pack_named_ "return";;
let _tab_ = _pack_named_ "tab";;
let _page_ = _pack_named_ "page";;
let _space_ = _pack_named_ "space";;
let _nul_ = _pack_named_ "nul";;

let string_to_named ch =
  match ch with
  | "newline" -> '\n'
  | "return" -> '\r'
  | "tab" -> '\t'
  | "space" -> ' '
  | "nul" -> '\000'
  | _ -> Char.chr 0xc;;

(* Char Parser *)
let parser =
  (* CharPrefix Parser *)
  let _charprefix_ = PC.word "#\\" in
  (* Hex Char PArser *)
  let _hexchar_ = PC.pack (fun (chl) -> PC.nt_hex_nat('0' :: chl))
                        (fun (h) -> [Char.chr h]) in
  (* Named CHar Parser *)
  let _named_ = PC.disj_list [_newline_; _return_; _tab_; _page_; _space_; _nul_] in
  (* VisibleSimpleChar parser *)
  let _vschar_ = PC.pack (PC.range (Char.chr 33) (Char.chr 255)) (fun (c) -> [c]) in
  let _chared_ = PC.caten _charprefix_ (PC.disj_list [_hexchar_; _named_; _vschar_]) in
  (* Impl of char parser.. *)
  PC.pack (PC.not_followed_by _chared_ SymbolParser.parser)
            (fun (p, c) -> match c with
                          | car :: cdr when cdr = [] -> Char(car)
                          | _ -> Char(string_to_named (list_to_string c)));;

end;; (* Struct CharParser *)