(* © 2012 Victor Nicollet *)

open Ohm.Universal
open BatPervasives

module type ARGS = sig

  module Key : Ohm.Fmt.FMT
  type data 

  val fetch : args:(string * Json_type.t) list -> string -> Ohm.JsCode.t
  val send  : Ohm.View.html -> (string * Json_type.t) list

end

module Make = functor (Args:ARGS) -> struct

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

  type data = Args.data

  let more_arg   = "next"

  let more_link bctx reaction next = 
    let url  = bctx # reaction_url reaction in 
    let arg  = Args.Key.to_json_string next in
    let args = [ more_arg , Json_type.Build.string arg ] in
    Args.fetch ~args url

  let more source = 
    O.Box.reaction "more" begin fun self bctx url response ->
      
      let respond html = O.Action.json (Args.send html) response in
      
      let start = BatOption.bind (Args.Key.of_json_string_safe) (bctx # post more_arg) in
      
      match start with 
	| None -> return (respond identity)
	| Some start ->        
	  let! (list,next) = ohm (source # page_contents (Some start)) in
	  let more = BatOption.map (more_link bctx self) next in
	  let! renderer = ohm (source # render_next_page ~bctx ~more ~list) in
	  return (respond renderer)
	    
  end

  let box source =
    
    let! react_more = more source in
    
    O.Box.leaf begin fun bctx url ->
      let! (list,next) = ohm (source # page_contents None) in
      let more = BatOption.map (more_link bctx react_more) next in
      source # render_first_page ~bctx ~more ~list
    end
      
end

