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

(** The type of a static. This is either a bit of HTML, or a standalone file that
    can become downloadable.
*)
type item = [ `Page of (renaming -> Ohm.Html.writer)
	    | `File of string ] 

(** The type of a static site - it maps the key (which is the relative path of an 
    item within the /static directory) to its contents. 
*)
type site = (string,item) BatPMap.t
