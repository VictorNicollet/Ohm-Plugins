(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

(* -------------------------------------------------------------------------------------------------------- *)

module Seg = struct

  type 'a segment = ('a -> string) * (string -> 'a) 
  type 'a t = 'a segment

  let string = identity, identity 
  let int = string_of_int, (fun s -> try int_of_string s with _ -> 0) 

  module type JSON = sig
    type t 
    val t_of_json : Json.t -> t
    val json_of_t : t -> Json.t
    val default : t      
  end

  module OfJson = functor(Json:JSON) -> struct
    type t = Json.t
    open Json
    let seg = 
      (fun t -> match json_of_t t with 
	| Ohm.Json.String s -> s 
	| _ -> assert false),
      (fun s -> try t_of_json (Ohm.Json.String s) 
	with _ -> default)
  end

end

(* -------------------------------------------------------------------------------------------------------- *)

type ctx = {
  ctx_new_segs  : string list ;
  ctx_old_segs  : string list ;
  ctx_box_name  : int list ;
  ctx_boxes     : int ref ;
  ctx_reactions : int ref ;
  ctx_url       : string * string list;
  ctx_root      : string ;
  ctx_response  : Action.response ;
  ctx_argjson   : Json.t ;
  ctx_morejson  : Json.t ;
  ctx_mode      : [ `Lazy | `Eager | `Reaction of int list ] ;
}

let make ?reaction ctx_argjson ctx_morejson ctx_new_segs ctx_old_segs ctx_url ctx_root ctx_response = {
  ctx_new_segs  ;
  ctx_old_segs  ;
  ctx_response  ;
  ctx_boxes     = ref 0 ;
  ctx_reactions = ref 0 ;
  ctx_box_name  = [] ;
  ctx_argjson   ;
  ctx_morejson  ;
  ctx_url       ;
  ctx_root      ;
  ctx_mode      = let r name = `Reaction name in
		  BatOption.(default (if ctx_old_segs = [] then `Eager else `Lazy) 
			       (map r reaction))
} 

let next_reaction ctx = 
  let id = ! (ctx.ctx_reactions) in
  incr ctx.ctx_reactions ; id

let next_box ctx = 
  let id = ! (ctx.ctx_boxes) in
  incr ctx.ctx_boxes ; id 

let parse seg ctx = 
  let new_first, ctx_new_segs = 
    match ctx.ctx_new_segs with [] -> "", [] | h :: t -> h, t in
  let old_first, ctx_old_segs = 
    match ctx.ctx_old_segs with [] -> "", [] | h :: t -> h, t in
  let ctx_mode = match ctx.ctx_mode with 
    | `Lazy -> if new_first = old_first then `Lazy else `Eager
    | other -> other
  in
  snd seg new_first, 
  { ctx with ctx_new_segs ; ctx_old_segs ; ctx_mode } 

let subbox ctx = 
  let id = next_box ctx in
  let ctx_box_name = id :: ctx.ctx_box_name in
  id, ctx_box_name, { 
    ctx with 
      ctx_box_name  ;
      ctx_boxes     = ref 0 ;
      ctx_reactions = ref 0
  } 

module type CTX = sig
  type t 
  val get  : t -> ctx
  val set  : ctx -> t -> t
end

type 'a reaction = {
  r_fmt  : 'a Fmt.fmt ;
  r_name : int list ;
  r_root : string
}

let reaction_endpoint reaction arg = 
  let arg  = reaction.r_fmt.Fmt.to_json arg in
  let name = Json.of_list Json.of_int reaction.r_name in
  let root = Json.String reaction.r_root in
  JsCode.Endpoint.of_js ~name:"ohmBox_endpoint" ~args:[ root ; name ; arg ] 

let reaction_json reaction arg = 
  let arg  = reaction.r_fmt.Fmt.to_json arg in
  let name = Json.of_list Json.of_int reaction.r_name in
  let root = Json.String reaction.r_root in
  Json.Array [ root ; name ; arg ] 

let reaction_js reaction arg = 
  let args = reaction.r_fmt.Fmt.to_json arg in
  Js.ohmBox_call ~url:reaction.r_root ~name:reaction.r_name ~args () 

type box = int list

let url base segs = 
  base ^ "/#/" ^ String.concat "/" 
    (List.map Netencoding.Url.encode segs)

module Make = functor(Ctx:CTX) -> struct

  type result = 
    | Reaction of Action.response  
    | Panic    of JsCode.t
    | Lazy     of result list
    | Eager    of box * (Ctx.t,Html.writer) Run.t * result list

  let parse seg callback = 
    let! ctx = ohmctx Ctx.get in
    let  parsed, ctx = parse seg ctx in
    Run.edit_context (Ctx.set ctx) (callback parsed) 
	
  let add body callback = 
    let! ctx = ohmctx Ctx.get in
    let  id, box, ctx = subbox ctx in
    match ctx.ctx_mode with 
      | `Reaction (h :: _) when h = id -> 
	(* If a reaction is to be found, it will be within the box. *)
	Run.edit_context (Ctx.set ctx) body
      | `Reaction _ -> 
        (* Looking for a reaction that cannot be within the box. *)
	callback box
      | _ -> 
	(* Eager or lazy evaluation of contents. *)
	let! sub_result = ohm $ Run.edit_context (Ctx.set ctx) body in
	let! box_result = ohm $ callback box in
	match box_result with 
	  | Lazy       l  -> return $ Lazy  (    sub_result :: l) 
	  | Eager (n,h,l) -> return $ Eager (n,h,sub_result :: l) 
	  | other         -> return other

  let render box = 
    let id = Id.gen () in
    Html.(concat [
      str "<div id=\"" ;
      esc (Id.str id) ;
      str "\"/>" ;
      run (Js.ohmBox_declare ~id:(Id.str id) ~path:box ())
    ])

  let fill html = 
    let! ctx = ohmctx Ctx.get in
    return $ Eager (List.rev ctx.ctx_box_name, html, [])

  let panic js = 
    return $ Panic js

  let react fmt body callback = 
    let! ctx  = ohmctx Ctx.get in
    let  id   = next_reaction ctx in
    let  name = List.rev (id :: ctx.ctx_box_name) in
    let  reac = { r_fmt = fmt ; r_name = name ; r_root = ctx.ctx_root } in
    match ctx.ctx_mode with 
      | `Reaction [x] when x = id ->
	(* This is the reaction we are looking for. Call it ! *)
	let res = ctx.ctx_response in 
	let! arg = req_or (return $ Reaction res) (fmt.Fmt.of_json ctx.ctx_argjson) in
	let! res = ohm (body arg ctx.ctx_morejson reac res) in
	return $ Reaction res
      | other -> 
	(* Not looking for this reaction right now. *)
	callback reac

  module ParseArgs = Fmt.Make(struct
    type json t = <
     ?same  : string list option ;
     ?react : int list option ;
     ?args  : Json.t = Json.Null ;
     ?more  : Json.t = Json.Null
    >
  end)

  let default_args = ParseArgs.of_json (Json.Object [])

  let response ?(prefix="/") ?(parents=[]) url build body req res = 

    (* Parse the arguments received from the client as JSON *)
    let json = BatOption.default (Json.Object []) (Action.Convenience.get_json req) in
    let args = BatOption.default default_args (ParseArgs.of_json_safe json) in


    (* If this is a first request, provide parents and prefix *)
    let json = 
      [ "prefix", Json.String prefix ;
	"parents", Json.of_list Json.of_string parents ]
    in
    
    let res = 
      if args # same = None && args # react = None then 
	Action.json json res else res 
    in
    
    let segs = BatString.nsplit prefix "/" in
    let segs = List.filter ((<>) "") segs in

    (* Build the context based on what was received *)
    let ctx = make 
      ?reaction:(args#react) 
      (args#args) 
      (args#more) 
      (req#args) 
      (BatOption.default [] (args#same)) 
      (url,segs)
      (Action.url (req#self) (req#server) (req#args)) 
      res
    in

    let! new_ctx = ohm (build ctx) in

    Run.with_context new_ctx begin

      (* Grab whatever the body has to say about this. *)
      let! result = ohm body in

      (* Determine what the response should be. *)
      let rec extract = function
	| Reaction res  -> [`Reaction res]
	| Lazy l        -> List.concat (List.map extract l)
	| Eager (n,h,l) -> `Eager (n,h) :: List.concat (List.map extract l) 
	| Panic js      -> [`Panic js] 
      in

      let result = extract result in

      try let js = BatList.find_map (function `Panic js -> Some js | _ -> None) result in
	  return (Action.javascript js res) 
      with _ -> 
	try let res = BatList.find_map (function `Reaction res -> Some res | _ -> None) result in
	    return res
	with _ -> 
	  let  list = BatList.filter_map (function `Eager x -> Some x | _ -> None) result in
	  let! list = ohm $ Run.list_map begin fun (name,html) -> 
	    let! html = ohm html in
	    return $ Js.ohmBox_fill ~path:name ~html () 
	  end list in
	  return (Action.javascript (JsCode.seq list) res) 

    end

  let url list =   
    let! ctx = ohmctx Ctx.get in
    let  root, more = ctx.ctx_url in
    return $ url root (more @ list)

end

let render ~url ~default = 
  let id = Id.gen () in
  Html.(concat [
    str "<div id=\"" ;
    esc (Id.str id) ;
    str "\"/>" ;
    run (Js.ohmBox_init ~id:(Id.str id) ~url ~default ())
  ])
