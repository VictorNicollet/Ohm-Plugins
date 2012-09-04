(* Ohm is © 2012 Victor Nicollet *)

class t : object ('self)
  val buffer : Buffer.t
  method raw : string -> unit
  method esc : string -> unit
  method tag : string -> (string * string) list -> ('self -> unit) -> unit
  method contents : string
end


