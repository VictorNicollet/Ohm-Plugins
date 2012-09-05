(* Ohm is © 2012 Victor Nicollet *)

type key = string
type renaming = key -> string
type item = [ `Page of (renaming -> Ohm.Html.writer)
	    | `File of string ] 
type site = (string,item) BatPMap.t
