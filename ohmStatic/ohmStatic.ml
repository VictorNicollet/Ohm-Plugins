(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Ohm
open Ohm.Universal

exception Private

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
type item = [ `Page of page | `File of string ] 
type site = (string,item) BatPMap.t

let ends s t = BatString.ends_with s t

let clip t s = 
  if ends s t then 
    BatString.head s (String.length s - String.length t) 
  else
    s

let canonical = function 
  | "index.htm" | "index.html" | "index.md" -> ""
  | s when ends s "/index.html" -> clip "/index.html" s
  | s when ends s "/index.htm"  -> clip "/index.htm"  s
  | s when ends s "/index.md"   -> clip "/index.md"   s
  | s when ends s ".htm"  -> clip ".htm"  s
  | s when ends s ".html" -> clip ".html" s
  | s when ends s ".md"   -> clip ".md"   s
  | s -> s

type 'ctx renderer = key -> 'ctx Html.ctxrenderer

let default_render _ ?css ?js ?head ?favicon ?body_classes ~title writer = 
  return (O.page ?css ?js ?head ?favicon ?body_classes ~title writer)

let wrap template key ?css ?js ?head ?favicon ?body_classes ~title writer = 
  let! writer = ohm (template writer) in 
  return (O.page ?css ?js ?head ?favicon ?body_classes ~title writer) 

let prefixed_render ~default list key =
  let page = 
    try snd (List.find (fun (prefix,_) -> BatString.starts_with key prefix) list) 
    with _ -> default
  in
  page key 

let with_context ctx page key ?css ?js ?head ?favicon ?body_classes ~title writer = 
  Run.with_context ctx (page key ?css ?js ?head ?favicon ?body_classes ~title writer) 

let export ?(rename=canonical) ?(render=default_render) ?(public="/") ~server ~title site = 

  let files, endpoints, definitions = 
    BatPMap.foldi begin fun key item (files, endpoints, definitions) -> 
      match item with 
	| `File path -> BatPMap.add key path files, endpoints,definitions
	| `Page page ->
	  try 
	    let endpoint, define = Action.declare server (rename key) Action.Args.none in
	    files, 
	    BatPMap.add key endpoint endpoints, 
	    (define,page,key) :: definitions
	  with Private -> 
	    files, endpoints, definitions 
    end site (BatPMap.empty, BatPMap.empty, [])
  in

  let url server key = 
    try Action.url (BatPMap.find key endpoints) server () 
    with Not_found -> 
      try public ^ BatPMap.find key files 
      with Not_found -> public ^ key
  in

  List.iter begin fun (define,page,key) ->

    define begin fun req res ->

      let rename = url (req # server) in 
      let body   = page # body  rename in
      let css    = page # css   rename in
      let js     = page # js    rename in
      let head   = page # head  rename in 
      let bcls   = page # bcls in
      let title  = BatOption.default title (page # title) in

      let! page = ohm $ render key ?favicon:None ~css ~js ~head ~body_classes:bcls ~title body in
      return $ Action.page page res 

    end 
  end definitions

