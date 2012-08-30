(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

module type CONFIG = sig
  type whole
  module Whole : Fmt.FMT with type t = whole
  type piece 
  val empty : whole 
  val add : piece list -> Whole.t -> Whole.t    
end

module type EXPORT = sig
  type whole 
  type piece
  type id 
  val create : ?size:int -> ?init:whole -> unit -> (#CouchDB.ctx,id) Run.t
  val add : id -> ?steps:int -> piece list -> (#CouchDB.ctx,unit) Run.t	
  val finish : id -> (#CouchDB.ctx,unit) Run.t
  val finished : id -> (#Ohm.CouchDB.ctx,bool option) Ohm.Run.t
  val progress : id -> (#CouchDB.ctx,(int * int) option) Run.t
  val delete : id -> (#CouchDB.ctx,unit) Run.t
  val download : id -> (#CouchDB.ctx,whole option) Run.t
  type state = [ `Missing 
	       | `Incomplete of (int * int) option
	       | `Complete of whole ]
  val state : id -> (#CouchDB.ctx,state) Run.t 
  val sweep : count:int -> age:float -> (#CouchDB.ctx,int) Run.t
end

module Csv = struct
  type whole = string
  module Whole = Fmt.String
  type piece = string list
  let empty = "\xEF\xBB\xBF"
  let add pieces whole = 
    let line piece = 
      String.concat ";" (List.map (fun cell ->
	if String.contains cell '"' then
	  "\"" ^ String.concat "\"\"" (BatString.nsplit cell "\"") ^ "\""
	else if String.contains cell ';' then
	  "\"" ^ cell ^ "\""
	else
	  cell
      ) piece) ^ "\n"
    in
    String.concat "" (whole :: List.map line pieces) 
end

module JsonList = struct
  type whole = Json.t list
  module Whole = Fmt.Make(struct
    type t = whole
    let t_of_json = Json.to_array
    let json_of_t = Json.of_array 
  end)
  type piece = Json.t
  let empty = []
  let add pieces whole = 
    whole @ pieces    
end

module Make = 
  functor (Config:CONFIG) ->
    functor (MyId:CouchDB.ID) ->
      functor (Db:CouchDB.CONFIG) ->
struct

  type id    = MyId.t 
  type whole = Config.whole
  type piece = Config.piece
  type state = [ `Missing
	       | `Incomplete of (int * int) option
	       | `Complete of whole 
	       ]

  module Data = Fmt.Make(struct
    type json t = <
      size : (int * int) option ;
      data : Config.Whole.t ;
      time : float ;
      finished : bool 
    >
  end)

  module MyDB = CouchDB.Database(Db)
  module MyTable = CouchDB.Table(MyDB)(MyId)(Data) 

  module Design = struct
    module Database = MyDB
    let name = "export"
  end

  let create ?size ?(init=Config.empty) () = 
    let! time = ohmctx (#time) in
    MyTable.create (object
      method size = BatOption.map (fun s -> (0,s)) size
      method data = init
      method time = time
      method finished = false
    end)
    
  let add id ?steps pieces = 
    let! time = ohmctx (#time) in 
    let steps = match steps with 
      | None -> List.length pieces 
      | Some steps -> steps
    in 
    let f old = object
      method time = time
      method data = Config.add pieces (old # data)
      method finished = old # finished
      method size = BatOption.map (fun (n,s) -> n + steps, s) (old # size)
    end in
    Run.map ignore (MyTable.transaction id (MyTable.update f))

  let finish id = 
    let! time = ohmctx (#time) in
    let f old = object
      method time = time
      method finished = true
      method data = old # data
      method size = old # size
    end in
    Run.map ignore (MyTable.transaction id (MyTable.update f))

  let compute_progress data =
    match data # size with
      | Some (_,s) when data # finished -> Some (s,s)
      | Some (n,s) -> Some (max 0 (min s n), s)
      | None -> None	

  let progress id = 
    Run.map (BatOption.bind compute_progress) (MyTable.get id)

  let delete id = 
    Run.map ignore MyTable.(transaction id remove) 

  let finished id = 
    Run.map (BatOption.map (#finished)) (MyTable.get id) 

  let is_finished data = 
    if data # finished then Some (data # data) else None

  let download id = 
    Run.map (BatOption.bind is_finished) (MyTable.get id) 

  let get_state = function 
    | None -> `Missing 
    | Some data -> match is_finished data with 
	| Some whole -> `Complete whole
	| None -> `Incomplete (compute_progress data) 

  let state id = 
    Run.map get_state (MyTable.get id) 

  module LastTouchedView = CouchDB.MapView(struct
    module Key = Fmt.Float
    module Value = Fmt.Unit
    module Design = Design 
    let name = "last_touched"
    let map = "emit(doc.time)"
  end)  

  let recent age limit = 
    let! time = ohmctx (#time) in
    Run.map (List.map (#id |- MyId.of_id)) 
      (LastTouchedView.query ~startkey:(time -. age) ~descending:true ~limit ())

  let sweep ~count ~age = 
    let! list = ohm (recent age count) in
    let! ()   = ohm (Run.list_iter delete list) in  
    return (List.length list) 

end
