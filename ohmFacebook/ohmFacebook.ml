(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open Util

include Id

type session = <
  access_token : string ;
  uid          : t
> ;;

type config  = <
  app_id     : string ;
  api_key    : string ;
  api_secret : string
> ;;

let get_session config req = 
  let cookie_name = "fbs_" ^ config # app_id in    
  match req # cookie cookie_name with None -> None | Some cookie ->
    let extract regex =       
      let r = Str.regexp regex in
      try ignore (Str.search_forward r cookie 0) ; Some (Str.matched_group 1 cookie)
      with _ -> None
    in
    match extract "access_token=\\([^&\"]*\\)" with None -> None | Some access_token ->
      match extract "uid=\\([^&\"]*\\)" with None -> None | Some uid -> 
	Some ( object 
	  method access_token = access_token
	  method uid          = Id.of_string uid
	end )

type details = <
  firstname : string ;
  lastname  : string ;
  email     : string ;
  pic_small : string ;
  pic_large : string ;
  gender    : [`m|`f] option
> ;;

let get session = 
  let url = "https://graph.facebook.com/me?access_token=" ^ (session # access_token) in
  try 

    let response = 
      let curl      = Curl.init () in
      let buffer    = Buffer.create 1763 in
      Curl.set_url curl url ;
      Curl.set_writefunction curl (fun x -> Buffer.add_string buffer x ; String.length x) ;
      Curl.perform curl ;
      Curl.global_cleanup () ;
      Buffer.contents buffer 
    in

    let json      = Json_io.json_of_string ~recursive:true response in 
    let objekt    = Json_type.Browse.objekt json in 
    let firstname = Json_type.Browse.string (List.assoc "first_name" objekt) in
    let lastname  = Json_type.Browse.string (List.assoc "last_name" objekt) in
    let email     = Json_type.Browse.string (List.assoc "email" objekt) in
    let picture   = "https://graph.facebook.com/" ^ Id.str (session # uid) ^ "/picture" in
    let verified  = Json_type.Browse.bool   (List.assoc "verified" objekt) in
    let gender    = 
      try Some (if Json_type.Browse.string (List.assoc "gender" objekt) = "male" then `m else `f)
      with Not_found -> None
    in
    if verified then `valid ( object
      method firstname = firstname
      method lastname  = lastname
      method email     = email
      method pic_small = picture
      method pic_large = picture ^ "?type=large"
      method gender    = gender
    end ) 
    else `invalid
  with exn -> 
    log "Facebook.get : for url : %s" url ;
    log "Facebook.get : ERROR : %s" (Printexc.to_string exn) ;
    `not_found

  
