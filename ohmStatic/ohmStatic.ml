(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Ohm
open Ohm.Universal

type key = string
type renaming = key -> string
type page = <
  body  : renaming -> Ohm.Html.writer ;
  css   : renaming -> string list ;
  js    : renaming -> string list ;
  head  : renaming -> string ;
  bcls  : string list ;
  title : string option ;
>
type item = [ `Page of page | `File ] 
type site = (string,item) BatPMap.t

let export ?(rename=identity) ?(render=Html.print_page_ctx) ?(public="/public/") ~server ~title site = 

  let endpoints, definitions = 
    BatPMap.foldi begin fun key item (endpoints, definitions) -> 
      match item with `File -> (endpoints,definitions) | `Page page ->
	let endpoint, define = Action.declare server (rename key) Action.Args.none in
	BatPMap.add key endpoint endpoints, 
	(define,page) :: definitions
    end site (BatPMap.empty, [])
  in

  let url server key = 
    try Action.url (BatPMap.find key endpoints) server () 
    with Not_found -> public ^ key 
  in

  List.iter begin fun (define,page) ->

    define begin fun req res ->

      let rename = url (req # server) in 
      let body   = page # body  rename in
      let css    = page # css   rename in
      let js     = page # js    rename in
      let head   = page # head  rename in 
      let bcls   = page # bcls in
      let title  = BatOption.default title (page # title) in

      let! page = ohm $ render ~css ~js ~head ~body_classes:bcls ~title body in
      return $ Action.page page res 

    end 
  end definitions

