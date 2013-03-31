(* Ohm is © 2013 Victor Nicollet *)

open Ohm
open Ohm.Universal

type aspect = [ `DIV | `TR of int | `LI ]

let render ?(aspect=`DIV) json = 
  match aspect with
    | `DIV ->   
      Asset_OhmInfiniteScroll_Trigger.render (object
	method tag = "div"
	method url = json
      end) 
    | `LI ->   
      Asset_OhmInfiniteScroll_Trigger.render (object
	method tag = "li"
	method url = json
      end) 
    | `TR n ->
      Asset_OhmInfiniteScroll_Table.render (object
	method cols = n
	method url  = json
      end) 

let of_endpoint ?aspect endpoint = 
  let json = JsCode.Endpoint.to_json endpoint in 
  render ?aspect json 

let of_url ?aspect url = 
  of_endpoint ?aspect (JsCode.Endpoint.of_url url) 

let respond writer res = 
  Action.json [ "more", Html.to_json writer ] res
