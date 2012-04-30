(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

module type CONFIG = sig

  module Source    : Ohm.Fmt.FMT
  module Content   : Ohm.Fmt.FMT
  module PollDB    : Ohm.CouchDB.DATABASE
  module ContentDB : Ohm.CouchDB.DATABASE

  val poll : Source.t -> (string * Content.t Lazy.t) option 

end

module Make = functor(Config:CONFIG) -> struct

  (* Initial definitions : types, databases, tables... *)

  module Source  = Config.Source
  module Content = Config.Content    

  module PollInfo = struct
    module Float = Fmt.Float
    module T = struct
      type json t = {
	fetched "f" : Float.t ;
	wait    "w" : Float.t ;
	source  "s" : Source.t ;
	digest  "d" : string ;
	gc      "g" : int ;
	errors  "e" : int 
      }
    end
    include T
    include Fmt.Extend(T)
  end

  module ContentData = struct
    module T = struct
      type json t = {
	content "c" : Content.t 
      }
    end
    include T
    include Fmt.Extend(T)
  end 

  module MyDB = Config.PollDB
  module Design = struct
    module Database = MyDB
    let name = "poll-url"
  end

  module InfoTable = CouchDB.Table(Config.PollDB)(Id)(PollInfo) 
  module ContentTable = CouchDB.Table(Config.ContentDB)(Id)(ContentData) 

  module Signals = struct
    let change_call, change = Sig.make 
      (Run.list_fold (fun a b -> Run.map (fun a -> a || b) a) false)
  end

  (* Minor operations on the metadata *)

  let poll ~delay source = 
    let id = Id.gen () in 
    let info = PollInfo.({
      fetched = 0.0 ;
      wait    = delay ;
      source  ;
      digest  = "NO DIGEST YET";
      gc      = 0 ;
      errors  = 0 ;
    }) in
    let! _ = ohm $ InfoTable.transaction id (InfoTable.insert info) in
    return id

  let disable id = 
    let update info = PollInfo.({ info with gc = 999 }) in
    let! _ = ohm $ InfoTable.transaction id (InfoTable.update update) in
    return () 

  let insist id = 
    let update info = PollInfo.({ info with fetched = 0.0 }) in
    let! _ = ohm $ InfoTable.transaction id (InfoTable.update update) in
    return () 

  let get id = 
    let! data = ohm_req_or (return None) $ ContentTable.get id in 
    return $ Some data.ContentData.content

  (* Fetching the oldest active source available. *)

  module ByTimeView = CouchDB.DocView(struct
    module Key    = Fmt.Float
    module Value  = Fmt.Unit
    module Doc    = PollInfo
    module Design = Design
    let name = "by-time"
    let map  = "if (doc.g < 3) emit(doc.f + (doc.g ?  3*doc.g : (1 + doc.e) * doc.w))"
  end)

  let get_next () =
    let  now  = Unix.gettimeofday () in
    let! list = ohm $ ByTimeView.doc_query ~limit:1 ~endkey:now () in
    match list with [] -> return None | h :: _ ->
      return $ Some (h # id, h # doc) 

  (* Processing a given source *)
	
  let download id info = 
    try let! digest, lazy_content = req_or (return (`keep,info.PollInfo.digest))
	  (Config.poll info.PollInfo.source)
	in
	if info.PollInfo.digest = digest then return (`keep,digest) else
	  let  content = Lazy.force lazy_content in 
	  let  insert  = ContentData.({content}) in
	  let! _    = ohm $ ContentTable.transaction id (ContentTable.insert insert) in
	  let! keep = ohm $ Signals.change_call (id,info.PollInfo.source,content) in 
	  return begin 
	    if keep then `keep, digest else ( 
	      Util.log "OhmCouchPollUrl: polling %s: GC attempt %d/3 failed" 
		(Source.to_json_string info.PollInfo.source) (1 + info.PollInfo.gc);
	      `gc, digest)
	  end
    with exn -> 
      Util.log "OhmCouchPollUrl: polling %s: %s" 
	(Source.to_json_string info.PollInfo.source) (Printexc.to_string exn) ;
      return (`error,info.PollInfo.digest)

  let process = 
    let! () = ohm $ return () in

    let! id, info = ohm_req_or (return false) $ get_next () in

    (* "lock" the task to avoid multiple processing *)
    let  fetched = Unix.gettimeofday () in
    let  update info = PollInfo.({ info with fetched }) in
    let! _ = ohm $ InfoTable.transaction id (InfoTable.update update) in

    (* Perform the processing *)
    let! status, digest = ohm $ download id info in
    let err, gc = match status with
      | `keep  -> 0, 0
      | `gc    -> 0, 1
      | `error -> 1, 0
    in
    let  update info = PollInfo.({ info with 
      digest = if gc > 0 then "NO DIGEST - RETRY FOR GC" else digest ;
      gc     = (1 + info.gc) * gc ;
      errors = (1 + info.errors) * err
    }) in
    let! _ = ohm $ InfoTable.transaction id (InfoTable.update update) in

    return true
    
end
