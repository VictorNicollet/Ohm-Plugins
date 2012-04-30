(* Ohm is © 2012 Victor Nicollet *)

type inline = [ `Text of string
	      | `A    of string * inline
	      | `I    of inline
	      | `B    of inline
	      | `BR
	      ] list

type doc = inline list

module Clean : Ohm.Fmt.FMT with type t = doc

val parse : Lexing.lexbuf -> doc 
val parse_string : string -> doc 

val secure_link : string -> string

val html : doc -> string

val text : doc -> string

val length : doc -> int

val cut : max_lines:int -> max_chars:int -> doc -> doc
