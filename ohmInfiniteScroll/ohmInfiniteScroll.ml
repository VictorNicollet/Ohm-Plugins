(* Ohm is © 2013 Victor Nicollet *)

open Ohm
open Ohm.Universal

let of_endpoint endpoint = 
  let json = JsCode.Endpoint.to_json endpoint in 
  Asset_OhmInfiniteScroll_Trigger.render (object
    method url = json
  end) 

let of_url url = 
  of_endpoint (JsCode.Endpoint.of_url url) 

let respond writer res = 
  Action.json [ "more", Html.to_json writer ] res
