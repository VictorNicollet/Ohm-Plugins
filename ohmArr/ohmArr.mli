(* OhmArr is © 2012 Victor Nicollet *)

module type CONFIG = sig
  val host : string
  val port : int
  val salt : string
end

type room = Ohm.Id.t
type key  = string
type url  = string
type user = Ohm.Id.t

module type SERVER = sig

  val stats : unit -> <
    rooms  : int ;
    active : int ;
    idle   : int
  > option

  val create   : room -> key -> unit

  val delete   : room -> unit

  val post     : room -> Json_type.t -> unit

  val user_url : room -> user -> key -> url
 
end

module Connect : functor(Config:CONFIG) -> SERVER
