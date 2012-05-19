(* Ohm is © 2012 Victor Nicollet *)

(** Ajax loading of page bits. *)

(** Segment management : segments are used to parse the box url-path, which is then
    used to determine what the box contains. Note that unlike action arguments, 
    a segment must always be parsed, even if it means returning a default value. *)
module Seg : sig

  (** A segment parser *)
  type 'a segment = ('a -> string) * (string -> 'a) 
  type 'a t = 'a segment

  (** A default segment parser for strings (default is [""]). *)
  val string : string t

  (** A default segment parser for integers (default is [0]). *)
  val int : int t

  module type JSON = sig
    type t 
    val t_of_json : Json_type.t -> t
    val json_of_t : t -> Json_type.t
    val default : t      
  end

  (** Build a default segment parser from a JSON converter. There is a strong
      assumption here that the underlying JSON type is always a string. *)    
  module OfJson : functor(Json:JSON) -> sig
    type t = Json.t
    val seg : t segment
  end

end

(** Generate an URL from a base (the URL of the page on which boxes appear)
    and segments. 

    {[
let url = OhmBox.url "http://localhost/boxes" [ "foo" ; "bar" ] in
assert (url = "http://localhost/boxes/#/foo/bar") 
    ]} 
*)
val url : string -> string list -> string

(** A box context. Serves as an execution environment for box-related
    functions. *)
type ctx

(** A box identifier. *)
type box

(** A reaction identifier which takes arguments of the parametric type. *)
type 'arg reaction

(** Configuring a box module to use a given context. The context type is 
    opaque, but there is a function for extracting a box-context from that
    type, and a function for changing the box-context within that type
    (in a non-mutable fashion). *)
module type CTX = sig
  type t 
  val get  : t -> ctx
  val set  : ctx -> t -> t
end

(** The box module configurator. *)
module Make : functor(Ctx:CTX) -> sig

  (** A box result. *)
  type result
    
  (** Parses an segment from the incoming URL by passing it to a function that 
      is run within a context where the segment has been removed. 
      
      {[
let! value = Box.parse segment in expr
      ]}

      This parses the next segment using [segment], which yields [value], and 
      evaluates [expr] in a context where that segment has been parsed. 
  *)
  val parse : 
       'seg Seg.t 
    -> ('seg -> (Ctx.t,'result) Ohm.Run.t)
    -> (Ctx.t,'result) Ohm.Run.t    

  (** Adds a sub-box to the current box. 

      {[
let! box = Box.add body in expr
      ]}

      This defines a new box, [box], with a body constructed by [body]. 
      It will be added to the box that will be returned [expr].
  *)
  val add : 
       (Ctx.t,result) Ohm.Run.t
    -> (box -> (Ctx.t,result) Ohm.Run.t)
    -> (Ctx.t,result) Ohm.Run.t

  (** Render a box. *)
  val render : box -> Ohm.Html.writer
    
  (** Fills a box with some HTML. If that box contains sub-boxes, then
      those sub-boxes should be rendered within that HTML. 
      
      It should be noted that, when working in AJAX mode, this HTML
      will only be rendered (and the corresponding functions executed)
      if it has not already been rendered on the client side. This is
      determined by looking at the parsed segments: if none of the 
      segments parsed so far have changed client-side, then no HTML
      is sent. 

      {[
Box.fill (Asset_Foo_Bar.render ())
      ]}
  *)
  val fill : 
    (Ctx.t,Ohm.Html.writer) Ohm.Run.t -> (Ctx.t,result) Ohm.Run.t
    
  (** Panics: instead of doing anything box-related, causes the box
      request handler to perform a single specific operation on the 
      response. Everything else will be ignored, and execution might
      or might not be completed.

      {[
Box.panic (Js.redirect "/")
      ]}
  *)
  val panic : Ohm.JsCode.t -> (Ctx.t,result) Ohm.Run.t

  (** Defines a reaction. 
      
      {[
let! reaction = Box.react fmt body in expr
      ]}
      
      When evaluated in render-box mode, this will define a reaction
      and let [expr] reference it by passing it args that 
      match JSON format [fmt]. 
      
      When evaluated in reaction mode, this will parse arguments using
      JSON format [fmt] and execute [body] (but [expr] will not be
      evaluated). 
      
      Reaction mode evaluation determines which reaction to run based
      on the definition order. 
  *)
  val react : 
       'arg Ohm.Fmt.fmt 
    -> (    'arg
	 -> Json_type.t
         -> 'arg reaction
	 -> Ohm.Action.response 
	 -> (Ctx.t,Ohm.Action.response) Ohm.Run.t )
    -> ( 'arg reaction -> (Ctx.t,result) Ohm.Run.t ) 
    -> (Ctx.t,result) Ohm.Run.t

  (** Respond to an HTTP request. Automatically determines whether
      the AJAX or standard HTML response should be sent, and whether 
      it is in reaction or render-box mode. 
      
      {[
Box.response ~prefix ~parents root build body req res
      ]}

      This creates an HTTP response based on request [req] and response 
      [res]. The [body] is evaluated within a context that is 
      constructed by [build] and returns a {!result} that corresponds 
      to the kind of request that was received. 

      [url] is the URL of the page on which the boxes appear. The box-path
      will be appended to this url, after a [/#/].

      The [prefix] and [parents] are optional, and only used in AJAX 
      render-box mode in order to determine animations. If none are
      provided, then no animation happens when the URL changes.      
  *)
  val response : 
       ?prefix:string
    -> ?parents:string list 
    -> string
    -> (ctx -> ('ctx,Ctx.t) Ohm.Run.t)
    -> (Ctx.t,result) Ohm.Run.t
    -> ('server,string list) Ohm.Action.request
    -> Ohm.Action.response 
    -> ('ctx,Ohm.Action.response) Ohm.Run.t

  (** Generate an URL based on segments. 

      {[
module MySeg = OhmBox.Seg.OfJson(struct
  type json t = [ `foo | `bar ]
  let default = `bar
end)

let url (s:MySeg.t) = Box.url [ fst MySeg.seg s ] 
      ]}
  *)
  val url : string list -> (Ctx.t, string) Ohm.Run.t

end

(** Builds some JSON representing a reaction with its expected 
    parameters. 
*)
val reaction_json : 'fmt reaction -> 'fmt -> Json_type.t

(** Builds a piece of JavaScript code that calls a given reaction 
    and executes the returned code, within the box context. 
*)
val reaction_js : 'fmt reaction -> 'fmt -> Ohm.JsCode.t

(** Render the hole where boxes are rendered. *)
val render : url:string -> default:string -> Ohm.Html.writer
