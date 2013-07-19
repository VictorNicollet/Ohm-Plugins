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
  json  : renaming -> (string * Json.t) list ;
>
type item = [ `Page of page | `File of string ] 
type site = (string,item) BatMap.t

type ('server,'site) rec_pageinfo = <
  body  : Ohm.Html.writer ;
  css   : string list ;
  js    : string list ;
  head  : string ;
  bcls  : string list ;
  title : string ;
  key   : key ;
  url   : string ;
  json  : (string * Json.t) list ;
  req   : ('server, unit) Action.request ;
  site  : 'site
>

module Exported = struct
    
  type 'server t = {
    rename   : renaming ;
    url      : 'server -> key -> string option ;
    get_page : 'server t -> ('server,unit) Action.request  -> key -> ('server, 'server t) rec_pageinfo option 
  }

  let rename site = site.rename
  let url site = site.url

end

type 'server pageinfo = <
  body  : Ohm.Html.writer ;
  css   : string list ;
  js    : string list ;
  head  : string ;
  bcls  : string list ;
  title : string ;
  key   : key ;
  url   : string ;
  json  : (string * Json.t) list ;
  req   : ('server, unit) Action.request ;
  site  : 'server Exported.t
> ;;

let get_page site = site.Exported.get_page site

type ('s,'ctx) renderer = 's pageinfo -> ('ctx, JsCode.t -> string) Run.t

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

let generic_render ?writer (page:Html.renderer) (info:'s pageinfo) =

  let writer = BatOption.default (info # body) writer 
  and css    = info # css
  and js     = info # js 
  and head   = info # head 
  and body_classes = info # bcls
  and title  = info # title 
  in

  return (page ~css ~js ~head ~body_classes ~title writer)

let default_render info = generic_render O.page info 

let custom_render page info = generic_render page info 

let wrap ?(page=O.page) template info = 
  let! writer = ohm (template (info # body)) in 
  generic_render ~writer page info 

let extend ?(page=O.page) template info = 
  let! writer = ohm (template info) in
  generic_render ~writer page info

let prefixed_render ~default list info =
  let key = info # key in 
  let page = 
    try snd (List.find (fun (prefix,_) -> BatString.starts_with key prefix) list) 
    with _ -> default
  in
  page info  

let with_context make_ctx arg page info = 
  Run.with_context (make_ctx arg) (page info)

let info_builder key page title public url exported req = 
  let rename key = 
    match url (req # server) key with Some url -> url | None -> public ^ key
  in 
  (object 

    val url = lazy (rename key) 
    method url = Lazy.force url 
      
    val body = page # body rename
    method body = body 
      
    val css = page # css rename 
    method css = css
      
    val js = page # js rename 
    method js = js
      
    val head = page # head rename
    method head = head
      
    val bcls = page # bcls
    method bcls = bcls 
      
    val title = BatOption.default title (page # title) 
    method title = title
      
    val key = key
    method key = key
      
    val json = lazy (page # json rename) 
    method json = Lazy.force json 
      
    val req = req
    method req = req
      
    val site = exported
    method site = site
      
   end)



let export ?(rename=canonical) ?(render=default_render) ?(public="/") ~server ~title site = 

  let files, endpoints, pages, definitions = 
    BatMap.foldi begin fun key item (files, endpoints, pages, definitions) -> 
      match item with 
	| `File path -> BatMap.add key path files, endpoints, pages, definitions
	| `Page page ->
	  try 
	    let endpoint, define = Action.declare server (rename key) Action.Args.none in
	    let info = info_builder key page title public in
	    files, 
	    BatMap.add key endpoint endpoints, 
	    BatMap.add key info pages,
	    (define,page,key,info) :: definitions
	  with Private -> 
	    files, endpoints, pages, definitions 
    end site (BatMap.empty, BatMap.empty, BatMap.empty, [])
  in

  let url server key = 
    try Some (Action.url (BatMap.find key endpoints) server ()) 
    with Not_found -> 
      try Some (public ^ BatMap.find key files)
      with Not_found -> None
  in

  let get_page exported req key = 
    try Some (BatMap.find key pages url exported req)
    with Not_found -> None
  in

  let exported = { Exported.rename ; Exported.url ; Exported.get_page } in

  List.iter begin fun (define,page,key,info) ->

    define begin fun req res ->

      let info = info url exported req in 

      let! page = ohm (render info) in
      return $ Action.page page res 

    end 
  end definitions ;

  exported

