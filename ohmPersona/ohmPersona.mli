(* Ohm is © 2013 Victor Nicollet *)

(** The URL of the Persona JavaScript source. Make sure this source file is 
    included in all paged that use Persona authentication ! 
*)
val script : string

(** A piece of HTML to be included in the page head, which ensures compatibility
    with IE.
*) 
val head : string

(** Render a persona login button 
*)
val button : 
     ?theme:[`Blue|`Dark|`Orange|`Custom of string]
  -> string
  -> ('ctx,Ohm.Html.writer) Ohm.Run.t

(** Run the JavaScript code that reacts to the user logging in or logging 
    out. Calls [login] URL as POST with the Persona assertion as its 
    JSON string payload when a login happens. Calls [logout] URL as POST with
    no payload when a logout happens. [current] should be the e-mail of the
    currently logged in user.
*)
val jsActivate : ?current:string -> login:string -> logout:string -> unit -> Ohm.JsCode.t

type email = string
type token = string 

(** A Persona login/logout configuration object. Represents a configured system
    that can be used for logging in and logging out users. 
*)
type ('ctx,'server,'user) config = <
  login  : ('server,unit) Ohm.Action.endpoint ;
  logout : ('server,unit) Ohm.Action.endpoint ; 
  cookie : string ;
  user   : token -> ('ctx, 'user option) Ohm.Run.t ;
  email  : 'user -> ('ctx, email option) Ohm.Run.t ;
>

(** Create a Persona login/logout configuration object. 

    [cookie] sets the name of the cookie used to store the current 
    authentication token. Default is ["PERSONA"].

    [urlPrefix] is prepended to the URLs for the login and logout
    endpoints. Default is ["persona/"].

    [server] is the server on which the login and logout actions
    will be listening. The server is also used to extract the
    "audience" parameter for the HTTP request. 
    
    [onLogin] is called with the valid e-mail whenever a new login
    occurs, and should return an authentication token and a piece of
    javascript to be run on the client (for instance, to redirect 
    to a new page). 

    [onLogout] is called whenever a logout occurs, along with the
    authentication token, and should return a piece of javascript
    to be run on the client (for instance, to redirect to a 
    login page). 

    [user] is called to retrieve an user identifier associated with
    a certain token. 

    [email] is called to retrieve the login e-mail associated with 
    a certain user. 

    Note that making this configuration will define two brand new
    actions (the login and logout endpoints). As such, it should
    only be called during initialization. 
*)
val init_full : 
     ?cookie:string
  -> ?urlPrefix:string
  -> server:'server Ohm.Action.server
  -> onLogin:('server -> email -> (unit, token option * Ohm.JsCode.t) Ohm.Run.t)
  -> onLogout:('server -> token -> (unit, Ohm.JsCode.t) Ohm.Run.t) 
  -> user:(token -> ('ctx, 'user option) Ohm.Run.t) 
  -> email:('user -> ('ctx, email option) Ohm.Run.t) 
  -> unit 
  -> ('ctx, 'server, 'user) config

(** As [init_full], but does not require to specify an user function (the user 
    identifier is assumed to be their email).
 *)
val init : 
     server:'server Ohm.Action.server
  -> onLogin:('server -> email -> (unit, token option * Ohm.JsCode.t) Ohm.Run.t)
  -> onLogout:('server -> token -> (unit, Ohm.JsCode.t) Ohm.Run.t) 
  -> email:(token -> ('ctx, email option) Ohm.Run.t) 
  -> ('ctx, 'server, email) config

(** Use a Persona configuration object to extract the current
    authentication token, if any. Note that said authentication token
    may be invalid, so it still
*)
val token : ('ctx, 's, 'u) config -> ('s,'p) Ohm.Action.request -> token option

(** Use a Persona configuration object to extract the current user id 
*)
val user : ('ctx, 's, 'u) config -> ('s,'p) Ohm.Action.request -> ('ctx,'u option) Ohm.Run.t

(** Runs [jsActivate] with the appropriate parameters and returns the code. Also includes 
    the user and their email. Ideal for calling on every page that requires login. 
*)
val auth : ('ctx, 's, 'u) config -> ('s,'r) Ohm.Action.request -> ('ctx, <
  code  : Ohm.JsCode.t ;
  who   : ('u * email) option ;
>) Ohm.Run.t
