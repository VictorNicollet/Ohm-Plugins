(* Ohm is © 2012 Victor Nicollet *)

(** Text manipulation. *)

(** Cut a text after a certain number of characters. Slicing occurs between words (that is, on 
    a space character). Optionally append provided ellipses after the cut. *)
val cut : ?ellipsis:string -> int -> string -> string
