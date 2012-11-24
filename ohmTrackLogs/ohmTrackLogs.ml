(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

type session = Id.t

class type ctx = object
  method track_logs : session option
  method time : float
end

let cookie_name = ref "OHMTRACK"

let get_session req res = 
  match req # cookie !cookie_name with 
    | None -> let session = Id.gen () in
	      let res = Action.with_cookie ~name:(!cookie_name) ~value:(Id.str session) ~life:0 res in
	      false, session, res 
    | Some session -> let session = Id.of_string session in 
		      true, session, res

let file_prefix = ref "/var/log/ozone/track"

let output_channel = ref None

let get_output_channel time = 

  let t = Unix.localtime time in
  let file = Printf.sprintf "%s.%04d-%02d-%02d.log" (!file_prefix)
    (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
  in

  (* Close output channel if it has expired. *)
  begin match !output_channel with 
    | Some (chan,file') when file <> file' -> close_out chan ; output_channel := None
    | _ -> ()
  end ;

  (* Grab output channel if it is present, otherwise create it. *)
  match !output_channel with 
    | Some (chan,_) -> chan
    | None -> let chan = open_out_gen [Open_wronly;Open_creat;Open_append;Open_binary] 0o644 file in
	      output_channel := Some (chan,file) ;
	      chan
	      
let log json = 
  let! time    = ohmctx (#time) in
  let! session = ohmctx (#track_logs) in
  match session with None -> return () | Some session -> 
    let  chan = get_output_channel time in 
    let  json = Json.Array [ Json.Float time ; Id.to_json session ; json ] in
    let  line = Json.serialize json in 
    output_string chan line ;
    output_char chan '\n' ;
    flush chan ;
    return () 
  
