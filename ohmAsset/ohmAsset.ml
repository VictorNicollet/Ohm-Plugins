(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Ohm
open Ohm.Universal

let notEmpty list = 
  list <> []

let listCount list = 
  Html.esc (string_of_int (List.length list))
