(* Ohm is © 2012 Victor Nicollet *)

module type CONFIG = sig

  module Source    : Ohm.Fmt.FMT
  module Content   : Ohm.Fmt.FMT
  module PollDB    : Ohm.CouchDB.DATABASE
  module ContentDB : Ohm.CouchDB.DATABASE

  val poll : Source.t -> (string * Content.t Lazy.t) option 

end

module Make : functor(Config:CONFIG) -> sig

  module Signals : sig
    val change : (Ohm.Id.t * Config.Source.t * Config.Content.t, 
		   (Ohm.CouchDB.ctx,bool) Ohm.Run.t) Ohm.Sig.channel
  end

  val poll : delay:float -> Config.Source.t -> (#Ohm.CouchDB.ctx,Ohm.Id.t) Ohm.Run.t

  val disable : Ohm.Id.t -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  val insist : Ohm.Id.t -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  val get : Ohm.Id.t -> (#Ohm.CouchDB.ctx,Config.Content.t option) Ohm.Run.t

  val process : (Ohm.CouchDB.ctx,bool) Ohm.Run.t

end
