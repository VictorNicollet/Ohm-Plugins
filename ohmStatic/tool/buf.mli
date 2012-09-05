(* Ohm is © 2012 Victor Nicollet *)

class t : object ('self)
  val mutable buffer  : Buffer.t
  val mutable content : [ `URL of string | `BUF of Buffer.t] list
  method url : string -> unit
  method raw : string -> unit
  method esc : string -> unit
  method tag : string -> (string * string) list -> ('self -> unit) -> unit
  method contents : [`URL of string | `RAW of string ] list
end


