(* Ohm is © 2012 Victor Nicollet *)

(** Filling up a CouchDB object with data, so that it may be extracted
    and downloaded in one go. *)

(** The type of an export configuration. *)
module type CONFIG = sig

  (** What object is being exported ? *)
  type whole

  (** The JSON formatter for saving the object to the database. *)
  module Whole : Ohm.Fmt.FMT with type t = whole

  (** What pieces are being added to the whole object ?*)
  type piece 

  (** The empty, initial objevct. *)
  val empty : whole

  (** Add several pieces to the whole. *)
  val add : piece list -> Whole.t -> Whole.t    

end

(** The configuration for exporting CSV data. *)
module Csv : CONFIG with type whole = string and type piece = string list

(** The configuration for exporting a list of JSON values. *)
module JsonList : CONFIG with type whole = Ohm.Json.t list and type piece = Ohm.Json.t

(** The type of an export module. *)
module type EXPORT = sig

  (** The type of whole object being exported. *)
  type whole 

  (** The type of a piece of the object, as added. *)
  type piece

  (** The type of an identifier. *)
  type id 

  (** Create a new exportable object. You may provide a size, 
      which represents the number of pieces expected to be added.
      This size can then be used to display progress, but has 
      no other effect. You may also provide an initial value, to be
      used instead of the initial value specified in the configuration.
  *)
  val create : ?size:int -> ?init:whole -> unit -> (#Ohm.CouchDB.ctx,id) Ohm.Run.t

  (** Add new pieces to the whole. Each piece counts as one step 
      towards the total size, as far as progress is concerned. The
      optional steps argument lets you provide a different number 
      instead, which can be useful if a step did not cause a 
      piece to be added (or added several pieces). 
  *)
  val add : id -> ?steps:int -> piece list -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t	

  (** Mark the export as finished, meaning that there are no more
      pieces to be added. It is not recommended to keep adding
      pieces after an export has been marked as finished !
  *)
  val finish : id -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  (** Return the current progress of an object. If the object was not
      created with a total size, or if the object does not exist, 
      then nothing is returned. If the object was marked as finished, 
      then a 100% progress is returned. Progress will never exceed
      100% (or be below 0%). It is returned as [steps,size]. 
  *)
  val progress : id -> (#Ohm.CouchDB.ctx,(int * int) option) Ohm.Run.t

  (** Delete an object. *)
  val delete : id -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t
    
  (** Download an object. This will return nothing if the object is
      not finished, or if it does not exist. 
  *)
  val download : id -> (#Ohm.CouchDB.ctx,whole option) Ohm.Run.t

  (** The current state of an object. This is [`Missing] if 
      the object does not exist, [`Incomplete] if it exists but is
      not finished yet, or [`Complete] if it exists and is finished. 
  *)
  type state = [ `Missing 
	       | `Incomplete of (int * int) option
	       | `Complete of whole ]

  (** Get the current state of an object *)
  val state : id -> (#Ohm.CouchDB.ctx,state) Ohm.Run.t 

  (** Clean up to [count] objects older than [age] seconds. The age of an
      object is the time at which it was the target of {!val:add} or {!val:finish} 
      for the last time. Returns how many objects were cleaned.
  *)
  val sweep : count:int -> age:float -> (#Ohm.CouchDB.ctx,int) Ohm.Run.t

end

module Make : 
  functor (Config:CONFIG) ->
    functor (MyId:Ohm.CouchDB.ID) ->
      functor (Db:Ohm.CouchDB.CONFIG) -> 
	EXPORT with type whole = Config.whole 
	       and  type piece = Config.piece
	       and  type id    = MyId.t 
