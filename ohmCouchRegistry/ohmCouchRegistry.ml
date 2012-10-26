(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

module type REGISTRY_CONFIG = sig
  module Id : CouchDB.ID
  module Store : CouchDB.CONFIG
end

module Entry = Fmt.Make(struct
  type json t = < 
    value "v" : Json.t ;
    key   "k" : Id.t 
  >
end)

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

module Make = functor(R:REGISTRY_CONFIG) -> struct

  type id = R.Id.t

  module Db = CouchDB.Database(R.Store)
  module Tbl = CouchDB.Table(Db)(Id)(Entry)

  let reserved = Hashtbl.create 100

  type 'a property = {
    fmt : 'a Fmt.t ;
    suffix : string ;
  }

  let property fmt name = 
    begin 
      try Hashtbl.find reserved name ; 
	      Util.log "Property %s is already reserved !" name ;
      with Not_found -> Hashtbl.add reserved name ()
    end ;
    let suffix = "." ^ String.concat ".." (BatString.nsplit name ".") in
    { fmt ; suffix }

  let merge id prop = Id.of_string (Id.to_string (R.Id.to_id id) ^ prop.suffix)

  let set id prop value = 
    let key = R.Id.to_id id in
    let id  = merge id prop in
    Tbl.set id (object 
      method value = prop.fmt.Fmt.to_json value
      method key   = key
    end)

  let get id prop = 
    let id = merge id prop in 
    let! line = ohm_req_or (return None) $ Tbl.get id in
    return (prop.fmt.Fmt.of_json (line # value))    

  let unset id prop = 
    let id = merge id prop in 
    Tbl.delete id 

  let update id prop f = 
    let key = R.Id.to_id id in
    let id = merge id prop in 
    Tbl.transact id begin fun oline -> 
      let  oelt = BatOption.bind (#value |- prop.fmt.Fmt.of_json) oline in 
      let!  elt = ohm (f oelt) in
      let  line = object
	method value = prop.fmt.Fmt.to_json elt 
	method key   = key
      end in 
      return (elt, `put line)
    end 

end
