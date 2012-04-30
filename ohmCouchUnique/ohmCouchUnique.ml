(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives
open Util

type id    = Id.t
type value = string 

let pair ida idb = 
  Id.str ida ^ "-" ^ Id.str idb

module Join = Fmt.Make(struct
  type json t = <
    id : Id.t
  > 
end)

module Value = struct
  type t = value
  let to_id = Id.of_string
  let of_id = Id.to_string
end

module Make = functor (DB:CouchDB.DATABASE) -> struct

  module MyTable = CouchDB.Table(DB)(Value)(Join)

  let id_of x = x # id

  let get_if_exists value = 
    MyTable.get value |> Run.map (BatOption.map id_of)

  let get value = 
    let id = Id.gen () in 
    let fresh = lazy (object method id = id end) in 
    MyTable.transaction value (MyTable.ensure fresh) |> Run.map id_of 

  let remove value =
    MyTable.transaction value MyTable.remove |> Run.map ignore
	
  let remove_atomic value current_id = 
    let has_id uniq = uniq # id = current_id in
    MyTable.transaction value (MyTable.remove_if has_id) |> Run.map ignore

  module Design = struct
    module Database = DB
    let name = "unique"
  end

  module Reverse = CouchDB.MapView(struct
    module Key   = Ohm.Id
    module Value = Ohm.Fmt.Unit
    module Design = Design 
    let name = "reverse"
    let map  = "emit(doc.id)"
  end)

  let reverse id = 
    let! list = ohm $ Reverse.by_key id in
    return $ List.map (#id |- Id.str) list

  let lock value id = 
    let update value = 
      let! stored = ohm $ MyTable.get value in 
      match stored with
	| None   -> return (id, `put (object method id = id end))
	| Some x -> return (id_of x, `keep) 
    in
    MyTable.transaction value update

end

