(* Ohm is © 2012 Victor Nicollet *)

(** The type of a tracking session. *)
type session = Ohm.Id.t

(** The type of a tracking context. *)
class type ctx = object
  method track_logs : session option 
  method time       : float 
end

(** The name of the tracking cookie. You may not change this value after
    you have started calling {!get_session} or {!log}. *)
val cookie_name : string ref

(** The prefix of the log files. If this is [/foo/bar], log files
    will be named [/foo/bar.1970-01-01.log] according to the date.
    You may not change this value after you have started calling {!log}. *)
val file_prefix : string ref

(** Use an existing session, extracted from a cookie named {!val:cookie_name}, or
    create a new one and add it to the response. A boolean indicates [false] if
    it's a new session, [true] if it already exists. *)
val get_session : ('a,'b) Ohm.Action.request -> Ohm.Action.response -> bool * session * Ohm.Action.response

(** Write a log entry. Log entries are formatted as JSON, and are output at a given time with a given
    session (those are extracted from the context). *)
val log : Ohm.Json.t -> (#ctx,unit) Ohm.Run.t


