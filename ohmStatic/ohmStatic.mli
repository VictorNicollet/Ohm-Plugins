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

(** The type of a renaming operation. Such operations turn the key of a site element
    into the public URL at which it may be found.
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
>

(** The type of a static. This is either a bit of HTML, or a standalone file that
    can become downloadable.
*)
type item = [ `Page of page | `File ] 

(** The type of a static site - it maps the key (which is the relative path of an 
    item within the /static directory) to its contents. 
*)
type site = (string,item) BatPMap.t

(** Canonical transformation of an URL : remove [.md], [.htm] and [.html] extensions, 
    then turn [foo/index] into [foo] (and ["index"] into [""]). 
*)
val canonical : key -> string

(** Export a static site. 
    @param rename A function that provides the path of each item. By default, 
    {!val:canonical} is applied to the key.
    @param server The server on which the site should run. 
    @param title The default title to be used, if no title is provided by the page. 
    @param render The function which is used to render the item. By default, 
    this is the vanilla [Ohm.Html.print_page]. 
    @param public The url prefix for files that are available for public download. 
    By default, this is ["/public"] and points to the [www/public] directory.
*)
val export : 
     ?rename:renaming
  -> ?render:(key -> unit Ohm.Html.ctxrenderer)
  -> ?public:string
  ->  server:('s Ohm.Action.server)
  ->  title:string
  ->  site
  ->  unit
