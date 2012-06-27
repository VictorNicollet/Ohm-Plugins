(* Ohm is © 2012 Victor Nicollet *)

(** Text manipulation. *)

(** Cut a text after a certain number of characters. Slicing occurs between words (that is, on 
    a space character). Optionally append provided ellipses after the cut. *)
val cut : ?ellipsis:string -> int -> string -> string

(** Format a piece of text as HTML, wrapped in <p></p>. 
    The following transformations are possible (by default, none are used). 
 
    @param nl2br Each newline (\n) is turned into a <br>
    @param skip2p Each line skip (\n\n) is turned into a <p>
    @param mailto Each substring that looks like an e-mail is turned into an <a href="mailto:">
    @param url Each substring that looks like an URL is turned into an <a href="">
*)
val format : 
     ?nl2br:bool
  -> ?skip2p:bool
  -> ?mailto:bool
  -> ?url:bool
  -> string
  -> string
