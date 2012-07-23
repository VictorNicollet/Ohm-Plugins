{ (* Ohm is © 2012 Victor Nicollet *) 

  type state = {
    nl2br : bool ;
    skip2p : bool ;
    mailto : bool ;
    url : bool ;
  } 

}

rule format acc state = parse

  | [ '\t' ' ' '\r' ] 
      { Ohm.Html.str " " acc ;
	format acc state lexbuf } 

  | '\n'
      { Ohm.Html.str (if state.nl2br then "<br/>" else " ") acc ;
	format acc state lexbuf }

  | '\n' ['\n' ' ' '\t' '\r'] * '\n' 
      { Ohm.Html.str (if state.skip2p then "</p><p>" else if state.nl2br then "<br/>" else " ") acc ;
	format acc state lexbuf } 

  | ['A'-'Z' '0'-'9' 'a'-'z'] + as s
      { Ohm.Html.str s acc ;
	format acc state lexbuf } 
      
  | (['A'-'Z' '0'-'9' 'a'-'z' '.' '+' '-' '_'] + '@' ['A'-'Z' '0'-'9' 'a'-'z' '.' '+' '-' '_'] +) as e   
      { if state.mailto then ( 
	  Ohm.Html.str "<a href=\"mailto:" acc ;
	  Ohm.Html.esc e acc ;
	  Ohm.Html.str "\">" acc ;
	  Ohm.Html.esc e acc ;
	  Ohm.Html.str "</a>" acc
	) else (
	  Ohm.Html.esc e acc 
	) ;
	format acc state lexbuf }

  | ( "http://" ['A'-'Z' '0'-'9' 'a'-'z' '.' '-' '_' '/'] +
	| "https://" ['A'-'Z' '0'-'9' 'a'-'z' '.' '-' '_' '/'] + 
	| "www." ['A'-'Z' '0'-'9' 'a'-'z' '.' '-' '_' '/'] +) as url
      { if state.url then (
	  Ohm.Html.str "<a href=\"" acc ;
	  Ohm.Html.str (if BatString.starts_with url "www" then "http://" else "") acc ;
	  Ohm.Html.esc url acc ;
	  Ohm.Html.str "\" target=\"_blank\">" acc ;
	  Ohm.Html.esc url acc ;
	  Ohm.Html.str "</a>" acc
	) else (
	  Ohm.Html.esc url acc 
	) ;
	format acc state lexbuf
      }
  | [^'A'-'Z' '0'-'9' 'a'-'z'] + as s 
      { Ohm.Html.esc s acc ;
	format acc state lexbuf } 
  | eof { () } 

{}
