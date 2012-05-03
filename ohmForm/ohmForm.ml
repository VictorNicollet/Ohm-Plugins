(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Ohm
open Ohm.Universal

type field = int list

type selector = string
let here = ""

type ('ctx,'seed,'result) template = {
  template : Json_type.t ;
  init     : 'seed -> ('ctx,Json_type.t) Run.t ;
  parse    : Json_type.t -> field -> ('ctx,('result,(field * string) list) BatStd.result) Run.t ;  
}

let seed_map f t =
  {
    template = t.template ;
    parse    = t.parse ;
    init     = (fun x -> t.init (f x)) ;
  }

let result_map f t = 
  {
    template = t.template ;
    init     = t.init ;
    parse    = (fun json field -> 
      let! result = ohm $ t.parse json field in
      match result with Ok ok -> return (Ok (f ok)) | Bad bad -> return (Bad bad)
    )
  }

let constant value = 
  {
    template = (fun _ -> Json_type.Null) ;
    init     = (fun _ -> return Json_type.Null) ;
    parse    = (fun _ _ -> return (Ok value)) ;
  }

let wrap selector renderer t = 
  {
    t with 
      template = (Json_type.Object [
	"t", Json_type.String "html" ;
	"s", Json_type.String selector ;
	"i", t.template ;
	"h", Html.to_json renderer ;
      ])
  }
  
let string ~field ?label_html ?field_html ?error_html ?label ?error seed result = 
  {
    template = Json_type.Object (List.concat [
      
      [ "t", Json_type.String "string" ;
	"s", Json_type.String field ] ;
      
      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", Html.to_json html ;
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", Html.to_json html ;
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", Html.to_json html) ;
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String text ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ]) ;

    init = (fun source -> 
      let! string = ohm $ seed source in
      return $ Json_type.String string ) ;
    
    parse = (fun json field -> 
      let  string = match json with Json_type.String s -> s | _ -> "" in
      let! parsed = ohm $ result (List.rev field) string in
      return (match parsed with Ok ok -> Ok ok | Bad b -> Bad [b]) 
    )
  }

let json_of_select_list fmt list = 
  Json_type.Build.list (fun (value, label, html_opt) ->
    Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "value", Json_type.String label ;
      "html", (match html_opt with
	| None      -> Json_type.Null
	| Some html -> Json_type.String (Html.to_html_string html))
      )
    ])
  ) list

let select_return_list fmt list = 
  [ "list", json_of_select_list fmt list ]

let select_search_param fmt request = 
  match request # post "complete" with 
    | Some term -> `Complete term
    | None -> 
      let get = 
	try 
	  match request # post "get" with 
	    | Some str -> let json = Json_io.json_of_string ~recursive:true str in
			  fmt.Fmt.of_json json
	    | None -> None
	with _ -> None
      in
      match get with 
	| Some value -> `Get value
	| None -> `Complete ""
			

let select
    ~field ?label_html ?field_html ?error_html ?label ?error
    ~format ~source
    seed result = 
  {
    template = Json_type.Object (List.concat [
      
      [ "t", Json_type.String "select" ;
	"s", Json_type.String field ] ;

      ( match source with 
	| `Static list -> [ "ss", json_of_select_list format list ] ;
	| `Dynamic url -> [ "ds", Json_type.String url ] 
	| `Both (list,url) ->
	  [ "ss", json_of_select_list format list ;
	    "ds", Json_type.String url ] ) ;
      
      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", Html.to_json html ; 
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", Html.to_json html ; 
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", Html.to_json html ;
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String text ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ])) ;

    init = (fun source -> 
      let! init = ohm $ seed source in 
      return (match init with 
	| None      -> Json_type.Null
	| Some data -> format.Fmt.to_json data)
    ) ;
    
    parse = (fun json field -> 
      let  data   = format.Fmt.of_json json in 
      let! parsed = ohm $ result (List.rev field) data in
      return (match parsed with Ok ok -> Ok ok | Bad b -> Bad [b])
    )
  }

let json_of_choice_list fmt list = 
  Json_type.Build.list (fun (value, html) ->
    Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "html",     Json_type.String (Html.to_html_string html))
    ])
  ) list

let choice
    ~field ?label_html ?field_html ?error_html ?label ?error
    ~format ~source ~multiple
    seed result = 
  {
    template = Json_type.Object (List.concat [
      
      [ "t",   Json_type.String "choice" ;
	"s",   Json_type.String field ;
	"src", json_of_choice_list format source ;
        "m",   Json_type.Bool multiple ] ;

      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", Html.to_json html ; 
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", Html.to_json html ; 
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", Html.to_json Html ; 
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String text ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ])) ;

    init = (fun source -> 
      let! list = ohm (seed source) in
      return $ Json_type.Build.list format.Fmt.to_json list
    ) ;
    
    parse = (fun json field -> 
      let data = 
	try let list = Json_type.Browse.array json in
	    BatList.filter_map format.Fmt.of_json list
	with _ -> []
      in 
      let! parsed = ohm (result (List.rev field) data) in
      return (match parsed with Ok ok -> Ok ok | Bad b -> Bad [b]) 
    )
  }

let array ~list ~add ~item ~remove inner = 
  {
    template = Json_type.Object 
      [ "t", Json_type.String "array" ;
	"ls", Json_type.String list ;
	"rs", Json_type.String remove ;
	"as", Json_type.String add ;
	"ih", Html.to_json item ; 
	"i",  inner.template  
      ]
    ) ;

    init = (fun seed -> 
      let! list = ohm $ Run.list_map inner.init seed in
      return $ Json_type.Array list
    ) ;

    parse = (fun json field ->
      let  input  = match json with Json_type.Array list -> list | _ -> [] in
      let  input  = BatList.mapi (fun i json -> (i :: field),json) input in
      let! result = ohm $ Run.list_map (fun (field,json) -> inner.parse field json) in
      if List.exists (function Bad _ -> true | Ok _ -> false) results then
	return $ Bad (List.concat (BatList.filter_map (function
	  | Bad x -> Some x
	  | Ok  _ -> None) results
	))
      else
	return $ Ok (BatList.filter_map (function
	  | Ok  x -> Some x
	  | Bad _ -> None) results
	)
    ) ;
  }     

let option ~list ~add ~item ~remove inner = 
  {
    template = Json_type.Object 
      [ "t",   Json_type.String "array" ;
	"ls",  Json_type.String list ;
	"rs",  Json_type.String remove ;
	"as",  Json_type.String add ;
	"ih",  Html.to_json item ;
	"max", Json_type.Int 1 ;
	"i",   inner.template 
      ]
    ) ;

    init = (fun seed -> 
      let! list = ohm (match seed with 
	| None       -> return []
	| Some value -> let! inner = ohm (inner.init value) in
			return [inner]) in
      return (Json_type.Array list)
    )) ;

    parse = (fun json field ->
      let input = match json with Json_type.Array (h :: _) -> Some h | _ -> None in
      let! result = ohm $ Run.opt_map (fun json -> inner.parse json (0 :: field)) input in     
      return (match result with 
	| None         -> Ok None
	| Some (Ok  x) -> Ok (Some x)
	| Some (Bad x) -> Bad x)
    ) ;
  }     
    
let append combine b a = 
  {
    template = Json_type.Array [a.template ; b.template] ;
    
    init = (fun seed -> 
      let! a = ohm (a.init seed) in
      let! b = ohm (b.init seed) in
      return $ Json_type.Array [ a ; b ]) ;
    
    parse = (fun json field ->
      let a_json, b_json = match json with 
	| Json_type.Array (a :: b :: _) -> a, b 
	| Json_type.Array [a] -> a, Json_type.Null
	| _ -> Json_type.Null, Json_type.Null
      in
      let! a_result = ohm $ a.parse a_json (0 :: field) in
      let! b_result = ohm $ b.parse b_json (1 :: field) in
      match a_result, b_result with 
	| Ok a, Ok b -> let! c = ohm (combine a b) in
			return (Ok c)
	| Ok _, Bad l 
	| Bad l, Ok _ -> return (Bad l)
	| Bad a, Bad b -> return (Bad (a @ b))
    ) ;
  }

let begin_object = constant

let end_object ?html template = 
  match html with None -> template | Some (sel,html) -> wrap sel html template

type 'seed source = 
  [ `Json of Json_type.t * Json_type.t
  | `Seed of 'seed * Json_type.t
  ]

let empty = `Json (Json_type.Null, Json_type.Null)

let from_params params = `Json (Json_type.Null, params)

let from_seed ?(params=Json_type.Null) seed = `Seed (seed,params) 

let from_post_json json = 
  try let assoc  = Json_type.Browse.objekt json in 
      let data   = List.assoc "data"   assoc in
      let params = List.assoc "params" assoc in 
      `Json (data,params)
  with _ -> empty

let from_post post = 
  try let json = Json_io.json_of_string ~recursive:true post in
      from_post_json json
  with _ -> empty

type ('ctx,'result) form = {
  result : ('ctx,('result, (field * I18n.text) list) BatStd.result) Run.t ;
  errors : (field * string) list ;
  config : Json_type.t ;
  data   : ('ctx,Json_type.t) Run.t ;
  params : Json_type.t
}
    
let create ~template ~source = 
  
  let data, params = match source with 
    | `Seed (seed,params) -> Run.keep (template.init seed), params
    | `Json (json,params) -> return json, params
  in
  
  {
    result = Run.keep (template.parse data []) ;
    errors = [] ;
    config = template.template ;
    data   ;
    params ;
  }

let params form = form.params

let render form url =
  let! data = ohm form.data in
  let id = Id.gen () in 
  let html = Html.concat [    
    Html.str "<form action=\"" ;
    Html.esc url ;
    Html.str "\" method=\"POST\"><input type=\"hidden\" id=\"" ;
    Html.esc (Id.str id) ;
    Html.str "\" value=\"" ;
    Html.esc (Json_io.string_of_json ~recursive:true ~compact:true data) ;
    Html.str "\"/></form>" ;
    Html.run (JsCode.make ~name:"joy" ~args:[ Id.to_json id ; form.config ; form.params ])
  ] in
  return html

let response form =
  let! data = ohm form.data in 
  return [
    "data", data ;
    "errors", Json_type.Build.list (fun (field,text) -> Json_type.Array [
      Json_type.Build.list Json_type.Build.int field ;
      Json_type.String (I18n.translate form.i18n text)
    ]) form.errors
  ] 

let set_errors errors form = 
  { form with errors = errors }

let has_errors form = form.errors <> []

let result form = form.result

module Skin = struct

  let with_ok_button ~ok t = 
    wrap "" (Asset_OhmForm_WithOkButton.render (object method ok = ok end)) t

  let text ~label seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Input.render (object method kind = "text" end))
      (string
	 ~field:"input" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 seed parse)

  let password ~label seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Input.render (object method kind = "password" end))
      (string
	 ~field:"input" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 (fun _ -> return "") parse)
	 
end
