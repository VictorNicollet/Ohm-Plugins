(* Ohm is © 2011 Victor Nicollet *)

type id    = Ohm.Id.t 
type value = string

val pair : id -> id -> value

module Make : 
  functor (DB:Ohm.CouchDB.DATABASE) -> 
sig
  
  val get : value -> (#Ohm.CouchDB.ctx, id) Ohm.Run.t 
  val get_if_exists : value -> (#Ohm.CouchDB.ctx, id option) Ohm.Run.t
  val remove : value -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t 
  val remove_atomic : value -> id -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t
  val reverse : id -> (#Ohm.CouchDB.ctx, value list) Ohm.Run.t
  val lock : value -> id -> (#Ohm.CouchDB.ctx, id) Ohm.Run.t

end
