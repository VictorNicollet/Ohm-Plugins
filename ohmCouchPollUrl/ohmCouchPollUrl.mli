(* Ohm is © 2012 Victor Nicollet *)

module type CONFIG = sig

  module Source    : Ohm.Fmt.FMT
  module Content   : Ohm.Fmt.FMT
  module PollDB    : Ohm.CouchDB.DATABASE
  module ContentDB : Ohm.CouchDB.DATABASE

  type ctx 
  val couchDB : ctx -> Ohm.CouchDB.ctx

  val poll : Source.t -> (string * Content.t Lazy.t) option 

end

module Make : functor(Config:CONFIG) -> sig

  module Signals : sig
    val change : (Ohm.Id.t * Config.Source.t * Config.Content.t, 
		   (Config.ctx,bool) Ohm.Run.t) Ohm.Sig.channel
  end

  val poll : delay:float -> Config.Source.t -> (Config.ctx,Ohm.Id.t) Ohm.Run.t

  val disable : Ohm.Id.t -> (Config.ctx,unit) Ohm.Run.t

  val insist : Ohm.Id.t -> (Config.ctx,unit) Ohm.Run.t

  val get : Ohm.Id.t -> (Config.ctx,Config.Content.t option) Ohm.Run.t

  val process : float -> (Config.ctx,float option) Ohm.Run.t

end
