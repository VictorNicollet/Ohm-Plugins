(* Ohm is © 2012 Victor Nicollet *)

open Ohm

let non_alphanumeric = Str.regexp "[^a-z0-9]+"

let make string = 
  let string = Util.fold_accents string in 
  let string = String.lowercase string in 
  let string = Str.global_replace non_alphanumeric "-" string in 
  let string = BatString.strip ~chars:"-" string in 
  string
  
