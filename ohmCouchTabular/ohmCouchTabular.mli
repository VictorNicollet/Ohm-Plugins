(* Ohm is © 2012 Victor Nicollet *)

module type TABULAR = sig

  module Key        : Ohm.CouchDB.ID

  module Source    : Ohm.Fmt.FMT 
  module Evaluator : Ohm.Fmt.FMT
  module Column    : Ohm.Fmt.FMT

  val evaluator_of_column : Column.t -> Evaluator.t

  module ListDB : Ohm.CouchDB.CONFIG
  module LineDB : Ohm.CouchDB.CONFIG
  module UniqDB : Ohm.CouchDB.CONFIG

  val background : (Ohm.CouchDB.ctx,bool) Ohm.Run.t -> unit 

  val sources_of_evaluator : Evaluator.t -> Source.t list
  val apply :
       Key.t
    -> Evaluator.t
    -> (#Ohm.CouchDB.ctx,Json_type.t * Json_type.t option) Ohm.Run.t

  val all_lines :
       Source.t
    -> from:Key.t option
    -> count:int
    -> (#Ohm.CouchDB.ctx,Key.t list * Key.t option) Ohm.Run.t

end

module Make : functor(T:TABULAR) -> sig

  type key       = T.Key.t
  type source    = T.Source.t
  type evaluator = T.Evaluator.t
  type column    = T.Column.t

  module ListId : Ohm.CouchDB.ID
  module LineId : Ohm.CouchDB.ID

  type line = {
    cells : Json_type.t list ;
    key   : key ;
    id    : LineId.t ;
    hint  : bool
  }

  type pager = Json_type.t * LineId.t

  val set_list   : 
        ListId.t 
    ->  columns : column list
    ->  source : source
    ->  filter : evaluator option
    ->  (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  val set_columns : 
       ListId.t
    -> column list 
    -> (#Ohm.CouchDB.ctx,unit) Ohm.Run.t

  val get_list :
       ListId.t 
    -> (#Ohm.CouchDB.ctx, (column list * source * evaluator option) option) Ohm.Run.t

  val check_list : 
       ListId.t
    -> (#Ohm.CouchDB.ctx, [ `Unlocked | `LineLocked | `ColumnLocked ] option) Ohm.Run.t

  val update : key -> source -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t

  val update_all : key -> (#Ohm.CouchDB.ctx, unit) Ohm.Run.t

  val hint : 
       ListId.t
    -> key
    -> (    evaluator
         -> (#Ohm.CouchDB.ctx as 'ctx, Json_type.t * Json_type.t option) Ohm.Run.t) 
    -> ('ctx, unit) Ohm.Run.t

  val read :
       ListId.t
    -> sort_column : int
    -> start: pager option
    -> count: int
    -> descending: bool
    -> (#Ohm.CouchDB.ctx, column list * line list * pager option) Ohm.Run.t

  val count : ListId.t -> (#Ohm.CouchDB.ctx,int) Ohm.Run.t    

end
