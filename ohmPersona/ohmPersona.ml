(* Ohm is © 2013 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

let validate = "https://verifier.login.persona.org/verify"

let script = "https://login.persona.org/include.js"

let head = "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=Edge\">"

let jsActivate ?current ~login ~logout () = 
  Js.ohmPersona ?current ~login ~logout () 

let button ?(theme=`Blue) label = 
  let theme = match theme with 
    | `Blue -> ""
    | `Dark -> " dark"
    | `Orange -> " orange"
    | `Custom s -> " " ^ s
  in
  Asset_OhmPersona_Button.render (object
    method theme = theme
    method label = label
  end)

type email = string
type token = string

type ('ctx,'server,'user) config = <
  login  : ('server,unit) Ohm.Action.endpoint ;
  logout : ('server,unit) Ohm.Action.endpoint ; 
  cookie : string ;
  user   : token -> ('ctx, 'user option) Ohm.Run.t ;
  email  : 'user -> ('ctx, email option) Ohm.Run.t ;
> ;;

module ApiResponse = Fmt.Make(struct
  type json t = <
    status   : [ `okay ] ;
    email    : string ; 
    audience : string ;
    expires  : float ;
    issuer   : string ; 
  >
end)

let init_full ?(cookie="PERSONA") ?(urlPrefix="persona/") ~server ~onLogin ~onLogout ~user ~email () = (object

  val login = Action.register server (urlPrefix ^ "login") Action.Args.none begin fun req res ->

    let! assertion = req_or (return res) 
      (Action.Convenience.get_json req |> BatOption.bind Fmt.String.of_json_safe) in

    let () = Util.log "Assertion: %s" assertion in

    let audience = 
      let domain = server # domain (req # server) in
      let port   = server # port (req # server) in
      let https  = match server # protocol (req # server) with
	| `HTTP -> false
	| `HTTPS -> true
      in
      Printf.sprintf "%s://%s:%d" (if https then "https" else "http") domain port 
    in

    let! api = req_or (return res) begin 
      try let curl = Curl.init () in
	  let data = Json.serialize (Json.Object [
	    "audience", Json.String audience ;
	    "assertion", Json.String assertion
	  ]) in
	  let response = 
	    let buffer = Buffer.create 1763 in
	    Curl.set_url curl validate ;
	    Curl.set_writefunction curl (fun x -> Buffer.add_string buffer x ; String.length x) ;
	    Curl.set_post curl true ;
	    Curl.set_httpheader curl [
	      "Content-type: application/json";
	    ] ;
	    Curl.set_postfields curl data ;
	    Curl.set_postfieldsize curl (String.length data) ;
	    Curl.perform curl ;
	    Curl.global_cleanup () ;
	    Buffer.contents buffer 
	  in
	  let () = Util.log "Response : %s" response in 
	  ApiResponse.of_json_string_safe response
      with exn -> Util.log "[FAIL] OhmPersona request : %s" (Printexc.to_string exn) ; None
    end in 

    let! token, js = ohm (onLogin (req # server) (api # email)) in

    let res = match token with 
      | None -> res
      | Some token -> Action.with_cookie ~name:cookie ~value:token ~life:0 res
    in

    return (Action.javascript js res) 
		      
  end 
  method login  = login 

  val logout = Action.register server (urlPrefix ^ "logout") Action.Args.none begin fun req res ->

    let! token = req_or (return res) (req # cookie cookie) in
    let! js = ohm (onLogout (req # server) token) in

    let  res = Action.with_cookie ~name:cookie ~value:"" ~life:0 res in
    let  js = JsCode.seq [ Js.ohmPersonaLogout () ; js ] in

    return (Action.javascript js res) 

  end
  method logout = logout

  method cookie = cookie
  method user   = user 
  method email  = email

end :('a,'b,'c) config)

let init ~server ~onLogin ~onLogout ~email = 
  init_full ~server ~onLogin ~onLogout ~user:email ~email:(fun e -> return (Some e)) () 

let token config req = 
  req # cookie (config # cookie) 

let user config req = 
  let! token = req_or (return None) (token config req) in
  config # user token 

let auth config req = 
  let! user  = ohm (user config req) in
  let! email = ohm (Run.opt_bind (config # email) user) in
  let  code  = jsActivate ?current:email 
    ~login:(Action.url (config # login) (req # server) ())
    ~logout:(Action.url (config # logout) (req # server) ()) 
    ()
  in
  return (object
    method code = code
    method who  = match user, email with Some user, Some email -> Some (user, email) | _ -> None
  end)
