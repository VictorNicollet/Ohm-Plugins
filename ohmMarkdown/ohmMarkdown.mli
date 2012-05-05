(* Ohm is © 2012 Victor Nicollet *)

type inline = 
  [ `CODE of string 
  | `SUB  of inline list
  | `B    of inline list
  | `I    of inline list
  | `A    of (string * string option * inline list) 
  | `IMG  of string * string * string option 
  | `STR  of string   
  ]

type tree = 
  [ `P   of inline list
  | `R   of inline list
  | `UL  of tree list list
  | `OL  of tree list list
  | `PRE of string
  | `IMG of string * string * string option
  | `BQ  of tree list
  | `H   of int * inline list
  ]

val parse : Lexing.lexbuf -> tree list

val to_html : tree list -> Ohm.Html.writer
