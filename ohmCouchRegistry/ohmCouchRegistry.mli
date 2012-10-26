(* Ohm is © 2012 Victor Nicollet *)

module type REGISTRY_CONFIG = sig
  module Id : Ohm.CouchDB.ID
  module Store : Ohm.CouchDB.CONFIG
end

module type REGISTRY = sig

  type id 
  type 'a property 

  val property : 'a Ohm.Fmt.t -> string -> 'a property

  val set : id -> 'a property -> 'a -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t
  val get : id -> 'a property -> (#Ohm.CouchDB.ctx,'a option) Ohm.Run.t
  val unset : id -> 'a property -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  val update : 
       id
    -> 'a property
    -> ('a option -> (#Ohm.CouchDB.ctx as 'ctx, 'a) Ohm.Run.t)
    -> ('ctx,'a) Ohm.Run.t
    
end 

module Make : functor(R:REGISTRY_CONFIG) -> REGISTRY with type id = R.Id.t
