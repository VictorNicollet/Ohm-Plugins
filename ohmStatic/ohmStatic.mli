(* Ohm is © 2012 Victor Nicollet *)

(** Static page generator. This module provides a tool that builds a static site from 
    pages found in the /static directory, and which becomes available as a module
    called [Static]. This site can then be made available using the function below.
*)

(** The key of a site element. This is the relative path of that element
    within the static directory, so the key of [/static/index.htm] would be
    [index.htm].
*)
type key = string

(** Raise this exception from a renaming function to indicate that a given item
    should not be public. This is useful when, for instance, you need to include 
    a specific bit of HTML on several pages but do not want to make that HTML
    available in other places. 
*)
exception Private

(** The type of a renaming operation. Such operations turn the key of a site element
    into the public URL at which it may be found. A renaming function may also raise
    {!Private}, which means an item has no public URL.
*)
type renaming = key -> string

(** The type of a page. This determines what the title of the page is, what javascript 
    and CSS files are to be included, and so on. 
*)
type page = <
  body  : renaming -> Ohm.Html.writer ;
  css   : renaming -> string list ;
  js    : renaming -> string list ;
  head  : renaming -> string ;
  bcls  : string list ;
  title : string option ;
  json  : renaming -> (string * Ohm.Json.t) list ;
>
  
(** The type of a static. This is either a bit of HTML, or a standalone file that
    can become downloadable.
*)
type item = [ `Page of page | `File of string ] 

(** The type of a static site - it maps the key (which is the relative path of an 
    item within the /static directory) to its contents. 
*)
type site = (string,item) BatPMap.t

(** A module for working on exported sites. *)
module Exported : sig

  (** The type of an exported site. Since a site is exported on a server, the
      server parameter is also a parameter of the exported site. *)
  type 'server t 

  (** The internal renamer function used to export this site. *)
  val rename : 'any t -> renaming

  (** Generate the url based on a key and the server parameter. This may 
      return [None] if the key does not match a defined (and public) 
      page or file. 
  *)
  val url : 'server t -> 'server -> key -> string option 

end

(** Information about a page. Provided to renderers. 
    This includes the received HTTP request.  
*)
type 'server pageinfo = <
  body  : Ohm.Html.writer ;
  css   : string list ;
  js    : string list ;
  head  : string ;
  bcls  : string list ;
  title : string ;
  key   : key ;
  url   : string ;
  json  : (string * Ohm.Json.t) list ;
  req   : ('server, unit) Ohm.Action.request ;
  site  : 'server Exported.t
>

(** A page renderer. Behaves like an [Ohm.Html.ctxrenderer], but is provided with 
    all its arguments as a single {!type:pageinfo}. *)
type ('serv,'ctx) renderer = 'serv pageinfo -> ('ctx, Ohm.JsCode.t -> string) Ohm.Run.t

(** Canonical transformation of an URL : remove [.md], [.htm] and [.html] extensions, 
    then turn [foo/index] into [foo] (and ["index"] into [""]). 
*)
val canonical : key -> string

(** Create a renderer from a custom page renderer. This simply uses the selected 
    page renderer instead of [O.page]. 
*)
val custom_render : Ohm.Html.renderer -> ('s,'ctx) renderer

(** Create a renderer from a wrapper template : the page contents are passed to the
    wrapper template function, and then rendered with the vanilla [O.page]. *)
val wrap : 
     ?page:Ohm.Html.renderer
  -> (Ohm.Html.writer -> ('ctx, Ohm.Html.writer) Ohm.Run.t)  
  -> ('serv,'ctx) renderer

(** Create a renderer from a wrapper template that is also provided with the full
    pageinfo for the page being rendered. 
*)
val extend : 
     ?page:Ohm.Html.renderer
  -> ('serv pageinfo -> ('ctx, Ohm.Html.writer) Ohm.Run.t) 
  -> ('serv,'ctx) renderer

(** Combine multiple renderers : select which renderer to use based on the prefix
    of the key of the page being rendered. The first matching prefix wins.
*)
val prefixed_render : 
     default:('serv,'ctx) renderer
  -> (string * ('serv,'ctx) renderer) list 
  -> ('serv,'ctx) renderer

(** Provide a context for rendering. This turns a renderer with an arbitrary 
    context into a unit-context renderer as expected by the {!val:export}
    function. You are expected to provide a function that returns the context
    (so that the exporter can generate a new context on demand). 
*)
val with_context : ('arg -> 'ctx) -> 'arg -> ('s,'ctx) renderer -> ('s,unit) renderer

(** Export a static site. 
    @param rename A function that provides the path of each item. By default, 
    {!val:canonical} is applied to the key.
    @param server The server on which the site should run. 
    @param title The default title to be used, if no title is provided by the page. 
    @param render The function which is used to render the item. By default, 
    this uses the vanilla [O.page]. 
    @param public The url prefix for files that are available for public download. 
    By default, this is ["/public"] and points to the [www/public] directory.
*)
val export : 
     ?rename:renaming
  -> ?render:('s,unit) renderer
  -> ?public:string
  ->  server:('s Ohm.Action.server)
  ->  title:string
  ->  site
  ->  's Exported.t
