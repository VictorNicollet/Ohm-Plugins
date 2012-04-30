(* © 2012 Victor Nicollet *)

module type ARGS = sig

  module Key : Ohm.Fmt.FMT
  type data 

  val fetch : args:(string * Json_type.t) list -> string -> Ohm.JsCode.t
  val send  : Ohm.View.html -> (string * Json_type.t) list

end

module Make : functor (Args:ARGS) -> sig

  class type ['ctx,'input] source = object

    method page_contents : 
         Args.Key.t option
      -> ('ctx,Args.data list * Args.Key.t option) Ohm.Run.t

    method render_next_page : 
         bctx:'input O.Box.box_context
      -> more:Ohm.JsCode.t option 
      -> list:Args.data list
      -> ('ctx,Ohm.View.html) Ohm.Run.t

    method render_first_page :
         bctx:'input O.Box.box_context
      -> more:Ohm.JsCode.t option 
      -> list:Args.data list
      -> ('ctx,Ohm.View.html) Ohm.Run.t
  end

  val box : ('ctx,'a) #source -> ('ctx,'a) O.Box.t

end
