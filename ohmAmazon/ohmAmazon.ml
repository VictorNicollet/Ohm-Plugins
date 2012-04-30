(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open Util
open BatPervasives

module type ACCOUNT = sig
  val id  : string
  val key : string
end

module type S3 = sig

  type upload 
      
  val upload : 
       ?acl:[`Private | `PublicRead | `PublicReadWrite | `AuthenticatedRead | `BucketOwnerRead
	    | `BucketOwnerFullControl ] 
    -> ?size:(int*int)
    -> ?life:float
    -> ?filename:string
    ->  bucket:string
    ->  key:string
    ->  redirect:string
    -> unit -> upload
	   
  val upload_form : upload -> string list -> ((#View.Context.text as 'ctx) View.t -> 'ctx View.t) -> 'ctx View.t

  val upload_url : upload -> string * ((string * string) list) 

  type uploaded = { name : string ; size : int }

  val find_upload : bucket:string -> prefix:string -> uploaded option

  val download : bucket:string -> key:string -> (string -> 'a option) -> 'a option

  val delete : bucket:string -> key:string -> bool

  val publish : bucket:string -> key:string -> file:string -> bool

  val qsa_auth : bucket:string -> key:string -> duration:int -> string

end

module S3 = functor(Account:ACCOUNT) -> struct

  let key = 
    let len       = String.length Account.key in
    let o_key_pad = BatString.init 64 (fun i -> Char.chr ((if i < len then Char.code Account.key.[i] else 0) lxor 0x5c)) in
    let i_key_pad = BatString.init 64 (fun i -> Char.chr ((if i < len then Char.code Account.key.[i] else 0) lxor 0x36)) in
    o_key_pad, i_key_pad

  let sign base64 = 
    let hmac = Util.sha1_hmac key base64 in
    hmac^"="

  type upload = {
    acl : [ `Private | `PublicRead | `PublicReadWrite | `AuthenticatedRead | `BucketOwnerRead
	  | `BucketOwnerFullControl ] ;
    size : int * int ;
    key : string ;
    bucket : string ;
    redirect : string ;
    life : float ;
    filename : string option 
  }

  let upload ?(acl=`Private) ?(size=(0,5*1024*1024)) ?(life=600.0) ?filename ~bucket ~key ~redirect () = {
    acl      = acl ;
    size     = size ;
    bucket   = bucket ;
    key      = key ;
    life     = life ;
    redirect = redirect ;
    filename = filename
  }

  let string_of_acl = function
    | `Private -> "private"
    | `PublicRead -> "public-read"
    | `PublicReadWrite -> "public-read-write"
    | `AuthenticatedRead -> "authenticated-read"
    | `BucketOwnerRead -> "bucket-owner-read"
    | `BucketOwnerFullControl -> "bucket-owner-full-control"

  let amzdate time = 
    let info = Unix.gmtime time in
    Printf.sprintf "%4d-%02d-%02dT%02d:%02d:%02d.000Z"
      (1900 + info.Unix.tm_year)
      (1 + info.Unix.tm_mon)
      (info.Unix.tm_mday)
      (info.Unix.tm_hour)
      (info.Unix.tm_min)
      (info.Unix.tm_sec)
      
  let httpdate time = 
    let info = Unix.gmtime time in
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
      [| "Sun" ; "Mon" ; "Tue" ;
	 "Wed" ; "Thu" ; "Fri" ; 
	 "Sat" |].(info.Unix.tm_wday)
      (info.Unix.tm_mday)
      [| "Jan" ; "Feb" ; "Mar" ; 
	 "Apr" ; "May" ; "Jun" ; 
	 "Jul" ; "Aug" ; "Sep" ; 
	 "Oct" ; "Nov" ; "Dec" |].(info.Unix.tm_mon)
      (1900 + info.Unix.tm_year)
      (info.Unix.tm_hour)
      (info.Unix.tm_min)
      (info.Unix.tm_sec)    

  let policy_of_upload upload =
    let expires  =
      let time = upload.life +. Unix.gettimeofday () in
      amzdate time
    in

    let conditions =  [
	Json_type.Build.objekt [ "bucket", Json_type.Build.string upload.bucket ] ;
	Json_type.Build.objekt [ "acl", Json_type.Build.string (string_of_acl upload.acl) ] ;
	Json_type.Build.objekt [ "redirect", Json_type.Build.string upload.redirect ] ;
	Json_type.Build.array [ 
	  Json_type.Build.string "starts-with" ;
	  Json_type.Build.string "$key" ;
	  Json_type.Build.string (upload.key^"/") ] ;
	Json_type.Build.array [
	  Json_type.Build.string "content-length-range" ;
	  Json_type.Build.int (fst upload.size) ;
	  Json_type.Build.int (snd upload.size) ]
      ] 
    in

    let conditions = 
      match upload.filename with 
	| None -> conditions
	| Some filename ->
	  Json_type.Build.objekt [ "Filename", Json_type.Build.string filename ]
	  :: conditions
    in

    let json = Json_type.Build.objekt [
      "expiration", Json_type.Build.string expires ;
      "conditions", Json_type.Build.array conditions
    ] in

    let string = Json_io.string_of_json ~recursive:true ~compact:true json in 

    let base64 = BatBase64.str_encode string in
    
    if String.length base64 mod 4 <> 0 then 
      base64 ^ String.make (4 - String.length base64 mod 4) '='
    else
      base64
  
  let signature_of_upload upload = 
    let base64 = policy_of_upload upload in 
    sign base64

  let upload_form upload accept inner ctx = 
    let url = "http://"^upload.bucket^".s3.amazonaws.com/" in
    ctx 
    |> View.str "<form action=\""
    |> View.esc url
    |> View.str "\" method=\"post\" enctype=\"multipart/form-data\"><input type=\"hidden\" name=\"key\" value=\""
    |> View.esc upload.key 
    |> View.str "/${filename}\"/><input type=\"hidden\" name=\"acl\" value=\""
    |> View.esc (string_of_acl upload.acl)
    |> View.str "\"/><input type=\"hidden\" name=\"redirect\" value=\""
    |> View.esc upload.redirect
    |> View.str "\"/><input type=\"hidden\" name=\"AWSAccessKeyId\" value=\""
    |> View.esc Account.id
    |> View.str "\"/><input type=\"hidden\" name=\"Policy\" value=\""
    |> View.esc (policy_of_upload upload)
    |> View.str "\"/><input type=\"hidden\" name=\"Signature\" value=\""
    |> View.esc (signature_of_upload upload)
    |> View.str "\"/>"
    |> inner begin fun ctx -> 
      ctx 
      |> View.str "<input type=\"file\" name=\"file\""
      |> (fun ctx -> if accept = [] then ctx else ctx 
	  |> View.str " accept=\""
	  |> View.implode (View.str ",") View.esc accept
	  |> View.str "\"")
      |> View.str "/>"
    end 
    |> View.str "</form>"

  let upload_url upload = 
    let url = "http://"^upload.bucket^".s3.amazonaws.com/" in
    url,  [ "key", upload.key ^ "/${filename}" ;
	    "acl", string_of_acl upload.acl ;
	    "redirect", upload.redirect ;
	    "AWSAccessKeyId", Account.id ;
	    "Policy", policy_of_upload upload ;
	    "Signature", signature_of_upload upload ]

  type uploaded = { name : string ; size : int }      

  let xml_extract tag string = 
    let regexp = Str.regexp ("<"^tag^">\\([^<]*\\)</"^tag^">") in
    try 
      let _ = Str.search_forward regexp string 0 in
      let str = Str.matched_group 1 string in
      let _,str = BatString.replace ~str ~sub:"&gt;" ~by:">" in
      let _,str = BatString.replace ~str ~sub:"&lt;" ~by:"<" in
      let _,str = BatString.replace ~str ~sub:"&quot;" ~by:"\"" in
      let _,str = BatString.replace ~str ~sub:"&amp;" ~by:"&" in
      Some str
    with Not_found -> None

  let qsa_auth ~bucket ~key ~duration = 
    let expires = string_of_int (duration + int_of_float (Unix.gettimeofday ())) in
    let stringToSign = "GET\n\n\n"^expires^"\n/"^bucket^"/"^key in
    let sign = sign (stringToSign) in
    let encoded_sign = BatString.replace_chars
      (function 
	| '=' -> "%3D"
	| '+' -> "%2B"
	|  c  -> String.make 1 c) sign
    in
    let qsa = String.concat "" [
      "?AWSAccessKeyId=" ;
      Account.id ;
      "&Expires=" ;
      expires ;
      "&Signature=" ;
      encoded_sign 
    ] in
    let url = "http://"^bucket^".s3.amazonaws.com/"^key^qsa in
    url

  let request ~verb ~bucket ?(key="") ?(qsa="") ?(storage=`Memory) callback =
    let verb_name = match verb with `GET -> "GET" | `PUT _ -> "PUT" | `DELETE -> "DELETE" in
    let date = httpdate (Unix.gettimeofday()) in
    let stringToSign = verb_name^"\n\n\n"^date^"\n/"^bucket^"/"^key in
    let sign = sign (stringToSign) in
    let url  = "http://"^bucket^".s3.amazonaws.com/"^key^qsa in
    let call = match verb with 
      | `GET -> Util.log "Amazon S3: GET %s" url ;
	(new Http_client.get url :> Http_client.http_call)
      | `PUT file -> 
	Util.log "Amazon S3: PUT %s" url ;
	let c = match Util.get_binary_contents file with 
	  | Some c -> c
	  | None -> log "Amazon.request:  [PUT] %s file not found" file ; ""
	in (new Http_client.put url c :> Http_client.http_call)	
      | `DELETE -> Util.log "Amazon S3: DELETE %s" url ;
	(new Http_client.delete url :> Http_client.http_call)
    in
    let head = call # request_header `Base in 
    head # update_field "Authorization" ("AWS "^Account.id^":"^sign) ;
    head # update_field "Date" date ;
    call # set_request_header head ;
    call # set_response_body_storage storage ;
    let pipe = new Http_client.pipeline in 
    pipe # add call ;
    pipe # run () ;
    callback call 

  let find_upload ~bucket ~prefix = 
    try 
      request  
	~verb:`GET 
	~bucket
	~qsa:("?prefix="^prefix)
	begin fun call ->
	  let chan = call # response_body # open_value_rd () in 
	  let rec aux sf = try aux (sf ^ chan # input_line ()) with _ -> chan # close_in () ; sf in
	  let back = aux "" in
	  match xml_extract "Size" back with 
	    | None -> None 
	    | Some size -> 
	      try 
		let size = int_of_string size in
		match xml_extract "Key" back with 
		  | None -> None
		  | Some key -> 
		    let _,name = BatString.replace ~str:key ~sub:(prefix^"/") ~by:"" in
		    Some { name = name ; size = size }
	      with _ -> None
	end
    with Http_client.Http_protocol e -> 
      log "Amazon.find_upload: %s" (Printexc.to_string e) ; None
    
  let download ~bucket ~key callback = 
    let file () = 
      let suffix = try snd (BatString.rsplit key ".") with Not_found -> key in
      Filename.temp_file "amz-" ("."^suffix)
    in
    try 
      request 
	~verb:`GET
	~bucket
	~key
	~storage:(`File file) 
	begin fun call ->
	    match call # response_body # store with 
	      | `File file -> callback file 
	      | `Memory ->
		log "Amazon.download: Ocamlnet stored in memory when file was requested" ; None
	end 
    with Http_client.Http_protocol e ->
      log "Amazon.download: %s" (Printexc.to_string e) ; None

  let delete ~bucket ~key = 
    try 
      request 
	~verb:`DELETE
	~bucket
	~key
	begin fun call -> true end
    with Http_client.Http_protocol e ->
      log "Amazon.delete: %s" (Printexc.to_string e) ; false

  let publish ~bucket ~key ~file = 
    try 
      request  
	~verb:(`PUT file) 
	~bucket
	~key
	begin fun call -> true end 
    with Http_client.Http_protocol e -> 
      log "Amazon.find_upload: %s" (Printexc.to_string e) ; false
    
    
end
