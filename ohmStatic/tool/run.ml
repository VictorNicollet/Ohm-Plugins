(* Ohm is © 2012 Victor Nicollet *)

let hub = Hub.make (function
  | "verb" | "verbatim" -> Some Lex_verbatim.lex 
  | "html" -> Some Lex_html.lex
  | _ -> None
) 

