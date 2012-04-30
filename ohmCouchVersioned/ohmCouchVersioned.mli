(* Ohm is © 2011 Victor Nicollet *)

module type VERSIONED = sig

  (** The name of this object type, used for various database-related naming needs. 
      Should be unique within the application and be database-ready. *)
  val name : string

  (** The database where the versioned data should be stored. This database should not
      be used for anything else. *)
  module DataDB : Ohm.CouchDB.CONFIG    

  (** The object identifier *)
  module Id : Ohm.CouchDB.ID

  (** The database where the versions themselves should be stored. This database should not
      be used for anything else. *)
  module VersionDB : Ohm.CouchDB.CONFIG

  (** The format of the versioned data *)
  module Data : Ohm.Fmt.FMT

  (** The format of the diffs applied to the versioned data *)
  module Diff : Ohm.Fmt.FMT

  (** Applying a diff to the versioned data. This function must only perform read-only database
      operations, but need not be idempotent. *)
  val apply : 
       Diff.t
    -> ( Ohm.CouchDB.ctx ,
	    Id.t 
	 -> float 
	 -> Data.t
	 -> ( Ohm.CouchDB.ctx, Data.t ) Ohm.Run.t ) Ohm.Run.t

  (** Data associated to diffs stored in databases. *)
  module VersionData : Ohm.Fmt.FMT
   
  (** Reflected data kept in the objects but not affected by versioning. *)
  module ReflectedData : Ohm.Fmt.FMT
    
  (** This function is called to generate the reflected data based on the current 
      main data. Must only perform read-only database operations. *)
  val reflect : Id.t -> Data.t -> ( Ohm.CouchDB.ctx, ReflectedData.t ) Ohm.Run.t

end

(** Create a versioned data module. *)
module Make : functor (Versioned:VERSIONED) -> sig

  (** Version identifier module *)
  module VersionId : Ohm.Id.PHANTOM

  (** Identifier module for the versioned objects *)
  module Id : Ohm.CouchDB.ID with type t = Versioned.Id.t

  (** Versioned data type. *)
  module Data : Ohm.Fmt.FMT with type t = Versioned.Data.t

  (** Diff data type. *)
  module Diff : Ohm.Fmt.FMT with type t = Versioned.Diff.t

  (** Version attached data type. *)
  module VersionData : Ohm.Fmt.FMT with type t = Versioned.VersionData.t

  (** Reflected data. *)
  module ReflectedData : Ohm.Fmt.FMT with type t = Versioned.ReflectedData.t

  (** The database containing the versioned objects. *)
  module DataDB : Ohm.CouchDB.DATABASE

  (** The database containing the versions. *)
  module VersionDB : Ohm.CouchDB.DATABASE

  (** The type of the complete versioned object *)
  type t 

  (** Get a versioned object by its identifier. *)
  val get : Id.t -> (#Ohm.CouchDB.ctx, t option) Ohm.Run.t

  (** Update the reflected data of an object. *)
  val reflect : Id.t -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t

  (** The identifier of a versioned object. *)
  val id : t -> Id.t

  (** The current versioned data of the versioned object. *)
  val current : t -> Data.t 

  (** The reflected data of the versioned object. *)
  val reflected : t -> ReflectedData.t

  (** A read-only JSON format, used for views *)
  module Raw : Ohm.Fmt.READ_FMT with type t = <
    current : Data.t ;
    reflected : ReflectedData.t
  >

  (** The type of a version. *)
  type version 

  (** Get the versions for a given object. They are sorted by time. *)
  val get_versions : Id.t -> (#Ohm.CouchDB.ctx, version list) Ohm.Run.t

  (** Get a specific version by id *)
  val get_version : VersionId.t -> (#Ohm.CouchDB.ctx, version option) Ohm.Run.t

  (** The identifier of a version. *)
  val version_id : version -> VersionId.t

  (** The time when a version was created. *)
  val version_time : version -> float

  (** The payload bound to a version when it was created. *)
  val version_data : version -> VersionData.t

  (** The diffs contained within a version. *)
  val version_diffs : version -> Diff.t list

  (** The identifier of the object to which the version applied. *)
  val version_object : version -> Id.t

  (** A snapshot of an object's data before and after a given version. *)
  val version_snapshot : version -> (#Ohm.CouchDB.ctx, (Data.t * Data.t) option) Ohm.Run.t 

  (** Creating a new version. This does nothing if the object is missing,
      updates it otherwise. *)
  val update : 
       id:Id.t 
    -> diffs:Diff.t list
    -> info:VersionData.t
    -> unit
    -> (#Ohm.CouchDB.ctx, t option) Ohm.Run.t

  (** Creating a new version, uses the provided initialization data 
      if the object is missing. *)
  val create : 
       id:Id.t
    -> init:Data.t
    -> diffs:Diff.t list
    -> info:VersionData.t
    -> unit
    -> (#Ohm.CouchDB.ctx, t) Ohm.Run.t

  (** Obliterate an object and all associated versions, forever. 
      This is intended for heavy-duty data removal, usually for privacy reasons,
      and is not expected to find much use in typical usage patterns. *)
  val obliterate : Id.t -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t

  (** Signals sent by this module. *)
  module Signals : sig

    (** Triggered after a new version has been saved to the database. *)
    val version_create : (version, (Ohm.CouchDB.ctx,unit) Ohm.Run.t) Ohm.Sig.channel

    (** Triggered after an explicit request to re-compute the reflected data. *)
    val explicit_reflect : (t, (Ohm.CouchDB.ctx, unit) Ohm.Run.t) Ohm.Sig.channel

    (** Triggered whenever the object might have changed, for any reason (new version,
	canceled version, or reflected data propagation) *)
    val update : (t, (Ohm.CouchDB.ctx, unit) Ohm.Run.t) Ohm.Sig.channel

  end

end
