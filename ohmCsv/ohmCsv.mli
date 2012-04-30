(* Ohm is © 2011 Victor Nicollet *)

(** Reading and writing CSV files. 

    @author Victor Nicollet
    @version 0.9
*)

val to_csv : string list -> string list list -> Ohm.View.text

val datetime : float -> string

val mime : string
