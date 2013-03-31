(* Ohm is © 2013 Victor Nicollet *)

type aspect = [ `DIV | `TR of int | `LI ]

val of_url : ?aspect:aspect -> string -> ('a, Ohm.Html.writer) Ohm.Run.t
val of_endpoint : ?aspect:aspect -> Ohm.JsCode.Endpoint.t -> ('a, Ohm.Html.writer) Ohm.Run.t

val respond : Ohm.Html.writer -> Ohm.Action.response -> Ohm.Action.response
