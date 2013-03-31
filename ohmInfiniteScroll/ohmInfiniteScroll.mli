(* Ohm is © 2013 Victor Nicollet *)

val of_url : string -> ('a, Ohm.Html.writer) Ohm.Run.t
val of_endpoint : Ohm.JsCode.Endpoint.t -> ('a, Ohm.Html.writer) Ohm.Run.t

val respond : Ohm.Html.writer -> Ohm.Action.response -> Ohm.Action.response
