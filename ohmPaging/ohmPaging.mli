(* Ohm is © 2012 Victor Nicollet *)

val slice : count:int -> 'a list -> 'a list * 'a option

module Book : sig

  type ('key,'data) t 

  val make : ('key * 'data) list -> ('key, 'data) t

  type ('key,'data) page = <
    first : bool ; 
    last  : bool ;
    key   : 'key ;
    data  : 'data ;
    pos   : int ;
    sel   : bool ; 
    url   : string option ;
    prev  : 'key option ;
    next  : 'key option ;
  >

  val full_list : ?url:('key -> string option) -> ('key,'data) t -> ('key,'data) page list 

  val list : 
       ?url:('key -> string option)
    -> 'key
    -> ('key,'data) t
    -> ('key,'data) page list option 
    
  val prev_next : 
       ?url:('key -> string option)
    -> 'key 
    -> ('key,'data) t 
    -> < prev : ('key,'data) page option ; next : ('key,'data) page option > option 

end
