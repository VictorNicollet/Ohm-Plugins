(* Ohm is © 2012 Victor Nicollet *)

module Make : functor(Db:Ohm.CouchDB.DATABASE) -> sig

  val prove : ?timed:bool -> reason:string -> thing:string -> string
  val check : ?timed:int  -> reason:string -> thing:string -> proof:string -> bool

  val passhash : string -> string

  module Id : sig 
    type t = Ohm.Id.t
    val delay : int
    val prove : t -> string * string
    val check : string * string -> t option
  end

end
