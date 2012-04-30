(* Ohm is © 2012 Victor Nicollet *) 

module Item : sig 
  type t = { 
    title       : string option ;
    link        : string option ;
    guid        : string option ;
    description : string option ;
    pubdate     : string option ;
    author      : string option ;
  } 
end

val parse : string -> Item.t list 
