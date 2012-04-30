(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open Util
open Universal
open BatPervasives

module type TABULAR = sig

  module Key       : CouchDB.ID

  module Source    : Fmt.FMT 
  module Evaluator : Fmt.FMT
  module Column    : Fmt.FMT

  val evaluator_of_column : Column.t -> Evaluator.t

  module ListDB : CouchDB.CONFIG
  module LineDB : CouchDB.CONFIG
  module UniqDB : CouchDB.CONFIG

  val background : (CouchDB.ctx,bool) Run.t -> unit 

  val sources_of_evaluator : Evaluator.t -> Source.t list
  val apply : Key.t -> Evaluator.t -> (#CouchDB.ctx,Json_type.t * Json_type.t option) Run.t

  val all_lines :
       Source.t
    -> from:Key.t option
    -> count:int
    -> (#CouchDB.ctx,Key.t list * Key.t option) Run.t

end

module Make = functor(T:TABULAR) -> struct

  (* General type and module definitions *)

  module Key       = T.Key
  type key         = T.Key.t

  module Source    = T.Source
  type source      = T.Source.t

  module Evaluator = T.Evaluator
  type evaluator   = T.Evaluator.t

  module Column    = T.Column
  type column      = T.Column.t

  module ListId : CouchDB.ID = Id
  module LineId : CouchDB.ID = Id

  type line = {
    cells : Json_type.t list ;
    key   : key ;
    id    : LineId.t ;
    hint  : bool
  }

  type pager = Json_type.t * LineId.t

  (* Grid-specific type definitions *)
      
  module ListData = struct
    module T = struct
      module Float = Fmt.Float
      type json t = {
	sources "cs" : Source.t list ;
	evals   "e"  : Evaluator.t list ;
	columns "c"  : (int * Column.t) list ;
	source  "s"  : Source.t ;
	filter  "f"  : Evaluator.t option ;
	version "v"  : int ;
	update  "u"  : [ `At     "l" of Id.t
		       | `Remove "r" of int 
		       | `Add    "a" of Id.t option ] list ;
	next    "n"  : Float.t ;
	process "p"  : [ `At     "l" of Id.t
		       | `Remove "r" of int 
		       | `Add    "a" of Id.t option ] option
      }
    end
    include T
    include Fmt.Extend(T)
  end

  module LineData = struct
    module T = struct
      module Json = Fmt.Json
      type json t = {
	list    "l" : Id.t ;
	version "v" : int ;
	key     "k" : Id.t ;
	hide    "h" : bool ;
	cells   "c" : Json.t list ;
	sort    "s" : Json.t list ;
	hint    "n" : bool 
      }
    end
    include T
    include Fmt.Extend(T)
  end

  (* Tables *)
    
  module ListDB = CouchDB.Database(T.ListDB)
  module ListTable = CouchDB.Table(ListDB)(ListId)(ListData)
  module ListDesign = struct
    module Database = ListDB
    let name = "list" 
  end

  module LineDB = CouchDB.Database(T.LineDB)
  module LineTable = CouchDB.Table(LineDB)(LineId)(LineData)
  module LineDesign = struct
    module Database = LineDB
    let name = "line" 
  end

  module UniqDB = CouchDB.Database(T.UniqDB)
  module LineByKey = OhmCouchUnique.Make(UniqDB)

  (* The id of a line based on a ListID and Key *)
  let uniq lid key = OhmCouchUnique.pair (ListId.to_id lid) (Key.to_id key)

  (* Data structure transforms *)
  let canonical_sources columns source filter =
    let cell_sources   = List.map (T.evaluator_of_column |- T.sources_of_evaluator) columns in
    let filter_sources = BatOption.(default [] $ map T.sources_of_evaluator filter) in
    BatList.sort_unique compare $ List.concat (filter_sources :: [source] :: cell_sources)

  let canonical_columns columns = 
    let evals = List.map T.evaluator_of_column columns in 
    let canonical_evals = BatList.sort_unique compare evals in 
    let columns = List.map (fun c -> 
      let eval = T.evaluator_of_column c in
      let pos  = BatOption.default 0 (BatList.index_of eval canonical_evals) in
      pos, c
    ) columns in 
    canonical_evals, columns

  let make_list ~columns ~source ~filter =
    let canonical_evals, cols = canonical_columns columns in 
    ListData.({
      sources = canonical_sources columns source filter ;
      columns = cols ;
      evals   = canonical_evals ;
      source  = source ;
      filter  = filter ;
      version = 0 ;
      update  = [ `Add None ] ;
      process = None ;
      next    = Unix.gettimeofday ()
    })

  let start_new_version list = 
    let is_removal = function 
      | `Remove _ -> true
      | `At _ | `Add _ -> false
    in
    ListData.({
      list with 
	version = list.version + 1 ;
	(* Place the `Remove after the `Add so that elements are upgraded, and the removal
	   only has to actually remove elements that were not upgraded. *)
	update  = `Add None :: `Remove list.version :: List.filter is_removal list.update ;
	next    = min (Unix.gettimeofday ()) list.next 
    })
    
  let update_list ~columns ~source ~filter list = 
    
    (* Place all sources and cells in canonical format. *)
    let canonical_sources = canonical_sources columns source filter in
    let canonical_evals, columns = canonical_columns columns in

    (* Only cause an update if the list specifications changed. *)

    if canonical_sources = list.ListData.sources
    && canonical_evals   = list.ListData.evals
    && filter            = list.ListData.filter
    && source            = list.ListData.source
    then 
      ListData.({ list with columns })
    else
      ListData.({ start_new_version list with 
	sources = canonical_sources ;
	columns = columns ;
	evals   = canonical_evals ;
	filter  = filter ;
	source  = source
      })

  let update_or_create_list ~columns ~source ~filter = function
    | None      -> make_list   ~columns ~source ~filter
    | Some list -> update_list ~columns ~source ~filter list

  let list_lock list =
    let locks_column = function `Add _ | `Remove _ -> true | `At _ -> false in
    if list.ListData.update = [] then `Unlocked else
      if List.exists locks_column list.ListData.update then `ColumnLocked else `LineLocked
  
  let schedule_update_at key list =
    (* If currently processing that specific key, then add a duplicate. If not processing
       yet, add it only if not present yet. *)
    let key = Key.to_id key in
    let insert = 
      if Some (`At key) = list.ListData.process then
	let insert list = `At key :: list in
	insert
      else
	let rec insert = function
	  | [] -> [`At key]
	  | `At k' as h :: t -> if key = k' then h :: t else h :: insert t
	  | h :: t -> h :: insert t
	in
	insert
    in
    ListData.({
      list with
	update = insert list.update ;
	next   = max (Unix.gettimeofday ()) list.next
    })

  let start_processing list = 
    (* By default always start with the first key. There is no semantic difference, 
       although from an optimization standpoint, `At orders are usually added at list end,
       so they will be performed after `Remove and `Add orders so that the list moves
       out of its `LockedColumn state faster *)
    match list.ListData.update with 
      | [] -> None
      | head :: tail -> Some (head, ListData.({ 
	list with 
	  next    = max (Unix.gettimeofday () +. 10.) list.next ;
	  process = Some head 
      }))

  let finish_processing what next list = 
    if List.mem what list.ListData.update then
      (* The task we finished processing is still in the list. Mark is as performed
	 and move on. *)
      let prepend l = match next with None -> l | Some task -> task :: l in
      ListData.({
	list with 
	  next    = Unix.gettimeofday () ; (* Schedule next process right now. *)
	  process = if list.process = Some what then None else list.process ;
	  update  = prepend $ BatList.remove list.update what
      })
    else
      (* Someone else finished or dropped the task (pesky race conditions!), do nothing. *)
      list

  (* A handful of useful views *)

  module BySourceListView = CouchDB.DocView(struct
    module Key    = Source
    module Value  = Fmt.Unit
    module Doc    = ListData
    module Design = ListDesign
    let name = "by_source"
    let map  = "for (var i in doc.cs) emit(doc.cs[i],null);"
  end)

  module VersionView = CouchDB.DocView(struct
    module Key = Fmt.Make(struct
      type json t = Id.t * int
    end)
    module Value  = Fmt.Unit
    module Doc    = LineData
    module Design = LineDesign
    let name = "by_version" 
    let map  = "emit ([doc.l,doc.v],null)"  
  end)

  module KeyView = CouchDB.MapView(struct
    module Key    = Id
    module Value  = Id
    module Design = LineDesign
    let name = "by_key"
    let map  = "emit(doc.k,doc.l);"
  end)

  module ProcessingListView = CouchDB.DocView(struct
    module Key    = Fmt.Float
    module Value  = Fmt.Unit
    module Doc    = ListData
    module Design = ListDesign
    let name = "by_next_time"
    let map  = "if (doc.u.length > 0) emit(doc.n,null);"
  end)

  module LineCountView = CouchDB.ReduceView(struct
    module Key     = Id
    module Value   = Fmt.Int
    module Reduced = Fmt.Int
    module Design  = LineDesign
    let name = "count" 
    let map = "if (!doc.h) emit(doc.l,null)" 
    let reduce = "_count" 
    let group = true
    let level = None
  end)

  module SortedView = CouchDB.DocView(struct
    module Key    = Fmt.Json
    module Value  = Fmt.Unit
    module Doc    = LineData
    module Design = LineDesign
    let name = "sort"
    let map  = "if (!doc.h) 
                  for (var i in doc.s) 
                    emit([doc.l,parseInt(i),doc.s[i]],null)"
  end)

  (* Database-accessing implementation *)

  let save_at_key lid ~visible ~hint ~sorted ~data ~version key = 

    let! id = ohm $ LineByKey.get (uniq lid key) in
    let line = LineData.({
      list    = ListId.to_id lid ;
      version = version ;
      key     = Key.to_id key ;
      hide    = not visible ;
      cells   = data ;
      sort    = sorted ;
      hint    = hint
    }) in

    let! _ = ohm $ LineTable.transaction (LineId.of_id id) (LineTable.insert line) in
    return ()

  let update_at_key lid ?(hint=false) ?evaluator key = 

    Util.log "Update list = '%s' ; key = '%s' " (ListId.to_id lid |> Id.str) (Key.to_id key |> Id.str) ;

    let! list      = ohm_req_or (return ()) $ ListTable.get lid in 
    let  version   = list.ListData.version in 
    let  evaluator = BatOption.default (T.apply key) evaluator in
    let  evals     = List.rev list.ListData.evals in
    let! applied   = ohm $ Run.list_map evaluator evals in 
 
    let sorted, data = List.fold_left begin fun (sorted,data) (cell_data, cell_sort_opt) ->
      match cell_sort_opt with 
	| Some cell_sort -> cell_sort :: sorted, cell_data :: data
	| None           -> cell_data :: sorted, Json_type.Null :: data
    end ([],[]) applied in 

    let! visible = ohm begin 
      match list.ListData.filter with 
	| None      -> return true
	| Some eval -> let! json, _ = ohm $ evaluator eval in
		       return (json = Json_type.Bool true)
    end in

    save_at_key lid ~visible ~hint ~sorted ~data ~version key
    
  let batch_size = 10

  let remove_lines_with_version lid version = 

    let key = ListId.to_id lid, version in 

    let! sample = ohm $ VersionView.doc_query 
      ~startkey:key ~endkey:key ~limit:batch_size ()
    in

    let remove item = 
      let id = LineId.of_id (item # id) in
      let checked_remove id = 
	(* Check whether the version is still the one to be removed! 
	   Maybe another process came around and updated the line to another version. *)
	let! line = ohm_req_or (return ((),`keep)) $ LineTable.get id in
	if line.LineData.version <> version then return ((),`keep) else return ((), `delete)
      in
      LineTable.transaction id checked_remove
    in
    
    let! _ = ohm $ Run.list_map remove sample in

    return (if sample = [] then None else Some (`Remove version))

  let add_lines lid key_opt = 

    let! list   = ohm_req_or (return None) $ ListTable.get lid in 
    let  source = list.ListData.source in

    let! add, next = ohm $ T.all_lines source ~from:key_opt ~count:batch_size in
    
    let! _ = ohm $ Run.list_map (update_at_key lid) add in
    
    match next with
      | None     -> return None
      | Some key -> return $ Some (`Add (Some (Key.to_id key)))

  let process_update lid = function 
    | `Remove version -> remove_lines_with_version lid version 
    | `At     id      -> let! () = ohm $ update_at_key lid (Key.of_id id) in return None
    | `Add    idopt   -> add_lines lid (BatOption.map Key.of_id idopt) 
 
  let process = 

    let! () = ohm $ return () in 
    
    (* Extract the next task from the data base *)
    let! next = ohm $ ProcessingListView.doc_query 
      ~endkey:(Unix.gettimeofday ()) ~limit:1 ()
    in
    
    let! next = req_or (return false) $ Util.first next in
    let  lid  = ListId.of_id (next # id) in 
    
    (* Determine what should be done, and lock the task. *)
    let! what = ohm_req_or (return false) $ ListTable.transaction lid begin fun lid ->
      let  abort = return (None, `keep) in
      let! list = ohm_req_or abort $ ListTable.get lid in
      if list.ListData.next > Unix.gettimeofday () then 
	(* Concurrent access : abort. *)
	abort
      else
	let! task, lock = req_or abort $ start_processing list in
	return (Some task, `put lock)
    end in
    
    (* Perform the task and determine if something should be done next. *)    
    let! continue = ohm $ process_update lid what in
      
      (* Unlock the task. *)
    let! () = ohm $ ListTable.transaction lid begin fun lid ->
      let! list = ohm_req_or (return ((), `keep)) $ ListTable.get lid in 
      return ((), `put (finish_processing what continue list))
    end in 
    
    return true

  let () = T.background process

  (* Publish the API *)

  let set_list lid ~columns ~source ~filter =
    ListTable.transaction lid begin fun lid -> 
      let! current = ohm $ ListTable.get lid in 
      let  updated = update_or_create_list ~columns ~source ~filter current in
      return ((), `put updated)
    end 

  let set_columns lid columns = 
    ListTable.transaction lid begin fun lid -> 
      let! current = ohm_req_or (return ((), `keep)) $ ListTable.get lid in 
      let  source  = current.ListData.source in
      let  filter  = current.ListData.filter in 
      let  updated = update_or_create_list ~columns ~source ~filter (Some current) in
      return ((), `put updated)
    end 

  let get_list lid = 
    let! list = ohm_req_or (return None) $ ListTable.get lid in 
    return $ Some ListData.( List.map snd list.columns, list.source, list.filter ) 

  let check_list lid = 
    let! list = ohm_req_or (return None) $ ListTable.get lid in
    return $ Some (list_lock list)

  let update key source = 

    let! lists = ohm $ BySourceListView.doc source in
 
    let schedule_list item =
      let lid = ListId.of_id (item # id) in
      ListTable.transaction lid begin fun lid ->
	let! current = ohm $ ListTable.get lid in 
	match current with 
	  | None      -> return ((), `keep)
	  | Some list -> return ((), `put (schedule_update_at key list)) 
      end
    in

    let! _ = ohm $ Run.list_map schedule_list lists in
    return ()

  let update_all key = 

    let! lists = ohm $ KeyView.by_key (Key.to_id key) in
    let  lists = List.map (#value |- ListId.of_id) lists in 

    let schedule_list lid =
      ListTable.transaction lid begin fun lid ->
	let! current = ohm $ ListTable.get lid in 
	match current with 
	  | None      -> return ((), `keep)
	  | Some list -> return ((), `put (schedule_update_at key list)) 
      end
    in

    let! _ = ohm $ Run.list_map schedule_list lists in
    return ()
    

  let hint lid key evaluator = 
    update_at_key lid ~hint:true ~evaluator key 

  let read lid ~sort_column ~start ~count ~descending = 

    let! the_list = ohm_req_or (return ([],[],None)) $ ListTable.get lid in 
    
    let permutation, columns = List.split the_list.ListData.columns in 
    
    let shuffle cells = 
      List.map (fun i -> try List.nth cells i with _ -> Json_type.Null) permutation
    in

    let real_sort_column = try List.nth permutation sort_column with _ -> 0 in

    let key ?start lid sort = 
      Json_type.Array (
	( Id.to_json (ListId.to_id lid) ) 
	:: ( Json_type.Int sort )
	:: ( match start with None -> [] | Some json -> [json] ))
    in 

    let smallest = key lid real_sort_column in
    let largest  = key lid (real_sort_column + 1) in
   
    let startkey, endkey, startid = 
      if descending then
	match start with 
	  | None            -> largest, smallest, None
	  | Some (start,id) -> key ~start lid real_sort_column, smallest, Some id
      else
	match start with
	  | None            -> smallest, largest, None
	  | Some (start,id) -> key ~start lid real_sort_column, largest, Some id
    in

    let startid = BatOption.map LineId.to_id startid in

    let limit = count + 1 in
    
    let! list = ohm $ SortedView.doc_query
      ~startkey ~endkey ?startid ~descending ~limit ()
    in

    let list, next = 
      try let first, following = BatList.split_at count list in
	  let next = BatList.first following  in
	  first, Some next
      with _ -> 
	(* An exception means there were not enough elements left *)
	list, None
    in

    let list =
      let line item =
	let cells = 
	  try BatList.map2
		(fun data sort -> if data = Json_type.Null then sort else data) 
		(item # doc).LineData.cells 
		(item # doc).LineData.sort
	  with _ -> (item # doc).LineData.sort 
	in
	{ 
	  cells = shuffle cells ;
	  key   = Key.of_id (item # doc).LineData.key ;
	  id    = LineId.of_id (item # id) ;
	  hint  = (item # doc).LineData.hint
	} 
      in

      List.map line list
    in
      
    let next = 
      match next with
	| None      -> None
	| Some item -> match item # key with 
	    | Json_type.Array [_;_;key] -> Some (key, LineId.of_id (item # id))
	    | _                         -> None
    in

    return (columns, list, next) 

  let count lid = 
    let! count_opt = ohm $ LineCountView.reduce (ListId.to_id lid) in
    return $ BatOption.default 0 count_opt

end

