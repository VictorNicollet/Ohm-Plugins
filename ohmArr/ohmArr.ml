(* OhmArr is © 2012 Victor Nicollet *)

open Ohm

module type CONFIG = sig
  val host : string
  val port : int
  val salt : string
end

type room = Ohm.Id.t
type key  = string
type url  = string
type user = Ohm.Id.t

module type SERVER = sig

  val stats : unit -> <
    rooms  : int ;
    active : int ;
    idle   : int
  > option

  val create   : room -> key -> unit

  val delete   : room -> unit

  val post     : room -> Json_type.t -> unit

  val user_url : room -> user -> key -> url
 
end

module Connect = functor(Config:CONFIG) -> struct

  let local     = "http://localhost:" ^ string_of_int Config.port ^ "/" 
  let user_root = "http://"^Config.host^":"^string_of_int Config.port^"/" 
    
  module Stats = Fmt.Make(struct
    type json t = <
      rooms  : int ;
      active : int ;
      idle   : int 
    >
  end)

  let stats () = 
    try let json = Http_client.Convenience.http_get local in 
	Stats.of_json_string_safe json
    with 
      |  Http_client.Http_error (status,_) -> Util.log "Arr! local:%d/ : HTTP status : %d" Config.port status ; None
      |  Http_client.Http_protocol error -> Util.log "Arr! local:%d/ : HTTP error : %s" Config.port (Printexc.to_string error) ; None

  let create room key = 
    let json = Json_io.string_of_json ~compact:true 
      (Json_type.Object [ "key", Json_type.String key ]) in
    let url  = local ^ Id.str room in
    let rec aux retries = 
      if retries = 0 then () else 
	try let _ = Http_client.Convenience.http_put url json in () 
	with 
	  | Http_client.Http_error (status,text) -> 
	    Util.log "Arr! PUT %s : HTTP status : %d %s" url status text;
	    aux (retries - 1)
	  | Http_client.Http_protocol error -> 
	    Util.log "Arr! PUT %s : HTTP error : %s" url (Printexc.to_string error) ;
	    aux (retries - 1)
    in
    aux 5
    
  let delete room = 
    let url  = local ^ Id.str room in
    let rec aux retries = 
      if retries = 0 then () else 
	try let _ = Http_client.Convenience.http_delete url in () 
	with 
	  | Http_client.Http_error (status,text) -> 
	    Util.log "Arr! DELETE %s : HTTP status : %d : %s" url status text ;
	    aux (retries - 1) 
	  | Http_client.Http_protocol error -> 
	    Util.log "Arr! DELETE %s : HTTP error : %s" url (Printexc.to_string error) ;
	    aux (retries - 1)
    in
    aux 5 

  let post room message =
    let json = match message with 
      | Json_type.Object _ -> message
      | _ -> Json_type.Object [ "data", message ] in
    let json_str = Json_io.string_of_json ~compact:true json in
    let url  = local ^ Id.str room in
    let rec aux retries = 
      if retries = 0 then () else begin
	Util.log "Arr! POST %s : %s" url json_str ;
	try let curl   = Curl.init () in
	    let buffer = Buffer.create 1763 in
	    Curl.set_url curl url ;
	    Curl.set_post curl true ;
	    Curl.set_writefunction curl 
	      (fun x -> Buffer.add_string buffer x ; String.length x) ;
	    Curl.set_postfields curl json_str ;
	    Curl.perform curl ;
	    Curl.global_cleanup () ;
	    Util.log "Arr! >> %s" (Buffer.contents buffer) 
	with other -> 
	  Util.log "Arr! POST %s : HTTP error : %s" url (Printexc.to_string other) ;
	  aux (retries - 1)
      end
    in
    aux 5
	 
  let user_url room user key =
    let hash = Digest.to_hex (Digest.string (Config.salt ^ ":" ^ Id.str user ^ ":" ^ key)) in
    user_root ^ Id.str room ^ "/" ^ Id.str user ^ "/" ^ hash


end
