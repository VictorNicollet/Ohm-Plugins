(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open Universal
open BatPervasives

module type VERSIONED = sig
  val name : string
  module DataDB : Ohm.CouchDB.CONFIG    
  module Id : Ohm.CouchDB.ID
  module VersionDB : Ohm.CouchDB.CONFIG
  module Data : Ohm.Fmt.FMT
  module Diff : Ohm.Fmt.FMT
  val apply : 
       Diff.t
    -> ( Ohm.CouchDB.ctx ,
	    Id.t 
	 -> float 
	 -> Data.t
	 -> ( Ohm.CouchDB.ctx, Data.t ) Ohm.Run.t ) Ohm.Run.t
  module VersionData : Ohm.Fmt.FMT
  module ReflectedData : Ohm.Fmt.FMT
  val reflect : Id.t -> Data.t -> ( Ohm.CouchDB.ctx, ReflectedData.t ) Ohm.Run.t
end

module Make = functor (Versioned:VERSIONED) -> struct

  module ObjectId = Versioned.Id
  module Data = Versioned.Data
  module Diff = Versioned.Diff
  module VersionData = Versioned.VersionData
  module ReflectedData = Versioned.ReflectedData

  module VersionId : Id.PHANTOM = Id.Phantom

  (* Basic object management *)

  module DataDB = CouchDB.Database(Versioned.DataDB)

  module Object = Fmt.Make(struct
    module Float = Fmt.Float
    type json t = <
       initial   "i" : Data.t ;
       current   "c" : Data.t ;
      ?time      "t" : Float.t = 0.0 ;
       reflected "r" : ReflectedData.t
     >
  end)

  module Raw = Fmt.Make(struct
    type json t = <
       current   "c" : Data.t ;
       reflected "r" : ReflectedData.t
     >
  end)
    
  module ObjectTable = CouchDB.Table(DataDB)(ObjectId)(Object)

  type t = ObjectId.t * Object.t

  let get id = 
    ObjectTable.get id |> Run.map (BatOption.map (fun obj -> id, obj)) 

  let id (id,_) = id
  let current (_,obj) = obj # current
  let reflected (_,obj) = obj # reflected

  (* Basic version management *)

  module VersionDB = CouchDB.Database(Versioned.VersionDB)

  module VersionDesign = struct
    module Database = VersionDB
    let name = "versioned"
  end      

  module Version = Fmt.Make(struct
    module Float = Fmt.Float
    type json t = <
      id    "i" : Id.t ;
      time  "t" : Float.t ;
      data  "d" : VersionData.t ;
      diffs "v" : Diff.t list
    >
  end)

  module VersionTable = CouchDB.Table(VersionDB)(VersionId)(Version)

  type version = VersionId.t * Version.t

  module VersionByIdView = CouchDB.DocView(struct      
    module Key = Fmt.Make(struct
      module Float = Fmt.Float
      type json t = Id.t * Float.t
    end)
    module Value = Fmt.Unit
    module Doc = Version
    module Design = VersionDesign
    let name = "by_id"
    let map  = "emit([doc.i,doc.t],null)"
  end)

  let get_versions_before oid ?(since=0.0) time = 
    let id = ObjectId.to_id oid in 
    let! list = ohm $ VersionByIdView.doc_query
      ~startkey:(id,since) ~endkey:(id,time) ~endinclusive:false ()
    in
    return $ List.map (fun i -> VersionId.of_id (i # id), i # doc) list

  let get_versions oid = get_versions_before oid max_float 

  let get_versions_since time oid = get_versions_before oid ~since:time max_float 

  let get_version vid = 
    let! version = ohm_req_or (return None) $ VersionTable.get vid in 
    return $ Some (vid, version)

  let version_time   (_,v) = v # time
  let version_data   (_,v) = v # data
  let version_diffs  (_,v) = v # diffs
  let version_object (_,v) = ObjectId.of_id v # id
  let version_id     (i,_) = i

  (* Diff application *)

  let apply_versions versions oid initial = 

    Run.edit_context CouchDB.ctx_decay begin
      
      let diffs_of_version version = 
	let time = version_time version in 
	List.map (fun diff -> time, diff) (version_diffs version)
      in
      
      let pre_apply (time,diff) = 
	let! result = ohm $ Versioned.apply diff in
	return (time, result)
      in
      
      let rec apply data = function
	| []                   -> return data
	| (time,diff) :: diffs -> let! data = ohm $ diff oid time data in
				  apply data diffs
      in	
      
      let diffs : (float * Diff.t) list = List.concat (List.map diffs_of_version versions) in	
      
      let! compiled_diffs = ohm $ Run.list_map pre_apply diffs in
      let! current        = ohm $ apply initial compiled_diffs in
      
      return current
    end

  (* Advanced version management : snapshots *)

  let version_snapshot v = 
    let  oid      = version_object v in
    let! versions = ohm $ get_versions_before oid (version_time v) in
    let! _, obj   = ohm_req_or (return None) (get oid) in 
    let! before   = ohm $ apply_versions versions oid (obj # initial) in 
    let! after    = ohm $ apply_versions [v]      oid  before in 
    return $ Some (before, after)

  (* Triggered signals *)

  module Signals = struct

    let call,   version_create   = Sig.make (Run.list_iter identity)
    let version_create_call args = Run.edit_context CouchDB.ctx_decay (call args)

    let call,           update           = Sig.make (Run.list_iter identity)
    let update_call args = Run.edit_context CouchDB.ctx_decay (call args)

    let call, explicit_reflect = Sig.make (Run.list_iter identity)
    let explicit_reflect_call args = Run.edit_context CouchDB.ctx_decay (call args)

  end

  (* Updating reflected data *)

  let reflect oid = 
    
    let update oid = 
      let! obj = ohm_req_or (return (None, `keep)) $ ObjectTable.get oid in
      let! reflected = ohm $ 
	Run.edit_context CouchDB.ctx_decay (Versioned.reflect oid (obj # current)) in

      if reflected == obj # reflected then return (None, `keep) else 
	let obj = object
	  method initial = obj # initial
	  method current = obj # current
	  method time    = obj # time
	  method reflected = reflected
	end in
	return (Some obj, `put obj)
    in

    let! obj = ohm_req_or (return ()) $ ObjectTable.transaction oid update in
    let! () = ohm $ Signals.explicit_reflect_call (oid, obj) in
    let! () = ohm $ Signals.update_call (oid, obj) in

    return ()

  (* Creating a new version *)

  let refresh ?latest (oid : ObjectId.t) (default:Data.t option) = 

    let update oid = 

      let! obj_opt = ohm $ ObjectTable.get oid in

      let initial_opt, fetch = match obj_opt, latest with 
	| None,     _      -> default, None
	| Some obj, None   -> Some (obj # initial), None
	| Some obj, Some l -> if (snd l) # time > obj # time 
	  then Some (obj # current), Some l 
	  else Some (obj # initial), None
      in

      let! versions = ohm begin
	match fetch with 
	  | None   -> get_versions oid 
	  | Some v -> return [v]
      end in
      
      match initial_opt with 
	| None -> return (None, `keep) 
	| Some initial -> 
	  
	  let! current        = ohm $ apply_versions versions oid initial in
	  let! reflected      = ohm $ 
	    Run.edit_context CouchDB.ctx_decay (Versioned.reflect oid current) in

	  let time = List.fold_left (fun t (_,v) -> max (v # time) t) 0.0 versions in 

	  let obj = object
	    method initial   = initial
	    method current   = current
	    method reflected = reflected
	    method time      = time
	  end in 
	  return (Some (oid, obj), `put obj)
    in
    
    let! oid, obj = ohm_req_or (return None) $ ObjectTable.transaction oid update in
    let! () = ohm $ Signals.update_call (oid, obj) in
    return (Some (oid, obj))
	
  let do_update ~id ~default ~diffs ~info () = 

    let time = Unix.gettimeofday () in    
    let vid = VersionId.gen () in

    let version = object
      method id    = ObjectId.to_id id
      method time  = time 
      method data  = info
      method diffs = diffs
    end in
    
    let! () = ohm begin 
      let! version = ohm $ VersionTable.transaction vid (VersionTable.insert version) in
      Signals.version_create_call (vid,version)
    end in

    refresh ~latest:(vid,version) id default

  let update ~id ~diffs ~info () = 
    do_update ~id ~default:None ~diffs ~info ()

  let create ~id ~init ~diffs ~info () = 
    (* We provide a value, so a value should be returned *)
    do_update ~id ~default:(Some init) ~diffs ~info () |> Run.map BatOption.get

  let obliterate oid = 

    (* Remove all versions first. *)
    let id = ObjectId.to_id oid in 
    let! versions = ohm $ VersionByIdView.doc_query 
      ~startkey:(id,0.0) ~endkey:(id,max_float) ()
    in
    let remove_version vid = 
      VersionTable.transaction vid VersionTable.remove |> Run.map ignore in
    let! _ = ohm $ Run.list_iter
      (#id |- VersionId.of_id |- remove_version) versions
    in 

    (* Remove the object itself *)
    let! _ = ohm $ ObjectTable.transaction oid ObjectTable.remove in 

    return ()


  module Id = ObjectId

end
