(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Ohm
open Ohm.Universal

type field = int list

type selector = string
let here = ""

type ('ctx,'seed,'result) template = {
  template : ('ctx,Json_type.t) Run.t ;
  init     : 'seed -> ('ctx,Json_type.t) Run.t ;
  parse    : Json_type.t -> field -> ('ctx,('result,(field * string) list) BatStd.result) Run.t ;  
}

let seed_map f t =
  {
    template = t.template ;
    parse    = t.parse ;
    init     = (fun x -> t.init (f x)) ;
  }

let seed_run_map f t = 
  {
    template = t.template ;
    parse    = t.parse ;
    init     = (fun x -> let! y = ohm (f x) in t.init y)
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
    template = return Json_type.Null ;
    init     = (fun _ -> return Json_type.Null) ;
    parse    = (fun _ _ -> return (Ok value)) ;
  }

let wrap selector renderer t = 
  {
    t with 
      template = let! renderer = ohm renderer in
		 let! inner    = ohm t.template in 
		 return (Json_type.Object [
		   "t", Json_type.String "html" ;
		   "s", Json_type.String selector ;
		   "i", inner ; 
		   "h", Html.to_json renderer ;
		 ])
  }
  
let string ~field ?label_html ?field_html ?error_html ?label ?error seed result = 
  {
    template = begin

      let! for_label_html = ohm begin
	match label_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "lh" , Html.to_json html ;
					"lhs", Json_type.String sel ]
      end in 

      let! for_field_html = ohm begin 
	match field_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "fh" , Html.to_json html ;
					"fhs", Json_type.String sel ]
      end in 

      let! for_error_html = ohm begin
	match error_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "eh" , Html.to_json html ;
					"ehs", Json_type.String sel ]
      end in 

      let! for_label = ohm begin
	match label with 
	  | None            -> return [] 
	  | Some (sel,text) -> let! text = ohm text in
			       return [ "ls", Json_type.String sel ;
					"lt", Json_type.String text ]
      end in 

      let for_error = 
	match error with 
	  | None     -> []
	  | Some sel -> [ "es", Json_type.String sel ] 
      in

      return $ Json_type.Object (List.concat [
	
	[ "t", Json_type.String "string" ;
	  "s", Json_type.String field ] ;
	
	for_label_html ; 
	for_field_html ;
	for_error_html ;
	for_label ;
	for_error 
      ]) 

    end ;

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
  let! list = ohm $ Run.list_map (fun (value, label, html_opt) ->
    
    let! html = ohm begin match html_opt with
      | None      -> return Json_type.Null
      | Some html -> let! html = ohm html in 
		     return $ Json_type.String (Html.to_html_string html)
    end in

    return $ Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "value",    Json_type.String label ;
      "html",     html 
    ])

  ) list in

  return $ Json_type.Array list

let select_return_list fmt list = 
  let! list = ohm $ json_of_select_list fmt list in
  return [ "list", list ]

let select_search_param fmt request = 
  match request # get "complete" with 
    | Some term -> `Complete term
    | None -> 
      let get = 
	try 
	  match request # get "get" with 
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
    template = begin 

      let! for_source = ohm begin 
	match source with 
	  | `Static list     -> let! list = ohm $ json_of_select_list format list in
				return [ "ss", list ] ;
	  | `Dynamic url     -> return [ "ds", Json_type.String url ] 
	  | `Both (list,url) -> let! list = ohm $ json_of_select_list format list in
				return [ "ss", list ;
					 "ds", Json_type.String url ]
      end in 

      let! for_label_html = ohm begin
	match label_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "lh" , Html.to_json html ;
					"lhs", Json_type.String sel ]
      end in 

      let! for_field_html = ohm begin 
	match field_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "fh" , Html.to_json html ;
					"fhs", Json_type.String sel ]
      end in 

      let! for_error_html = ohm begin
	match error_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "eh" , Html.to_json html ;
					"ehs", Json_type.String sel ]
      end in 

      let! for_label = ohm begin
	match label with 
	  | None            -> return [] 
	  | Some (sel,text) -> let! text = ohm text in 
			       return [ "ls", Json_type.String sel ;
					"lt", Json_type.String text ]
      end in 

      let for_error = 
	match error with 
	  | None     -> []
	  | Some sel -> [ "es", Json_type.String sel ] 
      in

      return $ Json_type.Object (List.concat [
      
	[ "t", Json_type.String "select" ;
	  "s", Json_type.String field ] ;
	
	for_source ;
	for_label_html ; 
	for_field_html ;
	for_error_html ;
	for_label ;
	for_error            

      ]) ;

    end ;

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

  let! list = ohm $ Run.list_map (fun (value, html) ->
    let! html = ohm html in
    return $ Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "html",     Json_type.String (Html.to_html_string html)
    ])
  ) list in

  return $ Json_type.Array list

let choice
    ~field ?label_html ?field_html ?error_html ?label ?error
    ~format ~source ~multiple
    seed result = 
  {
    template = begin

      let! list = ohm $json_of_choice_list format source in

      let! for_label_html = ohm begin
	match label_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "lh" , Html.to_json html ;
					"lhs", Json_type.String sel ]
      end in 

      let! for_field_html = ohm begin 
	match field_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "fh" , Html.to_json html ;
					"fhs", Json_type.String sel ]
      end in 

      let! for_error_html = ohm begin
	match error_html with 
	  | None            -> return [] 
	  | Some (sel,html) -> let! html = ohm html in 
			       return [ "eh" , Html.to_json html ;
					"ehs", Json_type.String sel ]
      end in 

      let! for_label = ohm begin
	match label with 
	  | None            -> return [] 
	  | Some (sel,text) -> let! text = ohm text in
			       return [ "ls", Json_type.String sel ;
					"lt", Json_type.String text ]
      end in 

      let for_error = 
	match error with 
	  | None     -> []
	  | Some sel -> [ "es", Json_type.String sel ] 
      in

      return $ Json_type.Object (List.concat [
	
	[ "t",   Json_type.String "choice" ;
	  "s",   Json_type.String field ;
	  "src", list ;
          "m",   Json_type.Bool multiple ] ;

	for_label_html ; 
	for_field_html ;
	for_error_html ;
	for_label ;
	for_error            
	
      ]) ;

    end ;

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
    template = let! item  = ohm item in
	       let! inner = ohm inner.template in
	       return $ Json_type.Object 
		 [ "t", Json_type.String "array" ;
		   "ls", Json_type.String list ;
		   "rs", Json_type.String remove ;
		   "as", Json_type.String add ;
		   "ih", Html.to_json item ; 
		   "i",  inner
		 ] ;

    init = (fun seed -> 
      let! list = ohm $ Run.list_map inner.init seed in
      return $ Json_type.Array list
    ) ;

    parse = (fun json field ->
      let  input  = match json with Json_type.Array list -> list | _ -> [] in
      let  input  = BatList.mapi (fun i json -> json,(i :: field)) input in
      let! result = ohm $ Run.list_map (fun (field,json) -> inner.parse field json) input in
      if List.exists (function Bad _ -> true | Ok _ -> false) result then
	return $ Bad (List.concat (BatList.filter_map (function
	  | Bad x -> Some x
	  | Ok  _ -> None) result
	))
      else
	return $ Ok (BatList.filter_map (function
	  | Ok  x -> Some x
	  | Bad _ -> None) result
	)
    ) ;
  }     

let option ~list ~add ~item ~remove inner = 
  {
    template = let! item = ohm item in
	       let! inner = ohm inner.template in
	       return $ Json_type.Object 
		 [ "t",   Json_type.String "array" ;
		   "ls",  Json_type.String list ;
		   "rs",  Json_type.String remove ;
		   "as",  Json_type.String add ;
		   "ih",  Html.to_json item ;
		   "max", Json_type.Int 1 ;
		   "i",   inner
		 ] ;

    init = (fun seed -> 
      let! list = ohm (match seed with 
	| None       -> return []
	| Some value -> let! inner = ohm (inner.init value) in
			return [inner]) in
      return (Json_type.Array list)
    ) ;

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
    template = let! a = ohm a.template in
	       let! b = ohm b.template in
	       return $ Json_type.Array [ a ; b ] ;
    
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

let keep field data = return (Ok data) 

let required error field string = 
  let string = BatString.strip string in 
  if string = "" then 
    let! error = ohm error in 
    return $ Bad (field,error)
  else
    return $ Ok string

let postpone handler field string = 
  let! result = ohm $ handler field string in
  match result with 
    | Bad something -> return $ Bad something
    | Ok  value     -> return $ Ok (value, field) 

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
  result : ('ctx,('result, (field * string) list) BatStd.result) Run.t ;
  errors : (field * string) list ;
  config : ('ctx,Json_type.t) Run.t ;
  data   : ('ctx,Json_type.t) Run.t ;
  params : Json_type.t
}
    
let create ~template ~source = 
  
  let data, params = match source with 
    | `Seed (seed,params) -> Run.memo (template.init seed), params
    | `Json (json,params) -> return json, params
  in
  
  {
    result = Run.memo (let! data = ohm data in 
		       template.parse data []) ;
    errors = [] ;
    config = Run.memo template.template ;
    data   ;
    params ;
  }

let params form = form.params

let render form url =
  let! data   = ohm form.data in
  let! config = ohm form.config in 
  let id = Id.gen () in 
  let html = Html.concat [    
    Html.str "<form action=\"" ;
    Html.esc url ;
    Html.str "\" method=\"POST\"><input type=\"hidden\" id=\"" ;
    Html.esc (Id.str id) ;
    Html.str "\" value=\"" ;
    Html.esc (Json_io.string_of_json ~recursive:true ~compact:true data) ;
    Html.str "\"/></form>" ;
    Html.run (JsCode.make ~name:"joy" ~args:[ Id.to_json id ; config ; form.params ])
  ] in
  return html

let response form =
  let! data = ohm form.data in 
  return [
    "data", data ;
    "errors", Json_type.Build.list (fun (field,text) -> Json_type.Array [
      Json_type.Build.list Json_type.Build.int field ;
      Json_type.String text
    ]) form.errors
  ] 

let set_errors errors form = 
  { form with errors = errors }

let has_errors form = form.errors <> []

let result form = form.result

module Skin = struct

  let with_ok_button ~ok t = 
    wrap "" (let! ok = ohm ok in
	     Asset_OhmForm_WithOkButton.render (object method ok = ok end)) t

  let text ~label seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Input.render (object 
	method kind = "text" 
	method css  = "" 
      end))
      (string
	 ~field:"input" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 seed parse)

  let textarea ~label seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Textarea.render ())
      (string
	 ~field:"textarea" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 seed parse)

  let widetext ~label seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Input.render (object 
	method kind = "text" 
	method css  = "-wide" 
      end))
      (string
	 ~field:"input" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 seed parse)

  let password ~label parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Input.render (object 
	method kind = "password" 
	method css  = ""
      end))
      (string
	 ~field:"input" 
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 (fun _ -> return "") parse)

  let option ~label ~remove_html ~add_html seed inner = 
 
    let remove_html = Ohm.Run.memo remove_html in 
    let item = Asset_OhmForm_Option_Item.render remove_html in
    
    seed_run_map seed 
      (wrap ".joy-fields"
	 (let! label = ohm label in 
	  Asset_OhmForm_Option.render (object
	   method label = label 
	   method add   = add_html 
	 end))
	 (option 
	    ~list:".joy-field-option ul"
	    ~add:".joy-option-add"
	    ~item
	    ~remove:".joy-option-remove"
	    inner))

  let radio ~label ~format ~source seed parse = 
    wrap ".joy-fields"
      (Asset_OhmForm_Radio.render ())
      (choice 
	 ~field:".joy-field-list"
	 ~label:(".joy-field-label label",label)
	 ~error:(".joy-field-error label")
	 ~format
	 ~source
	 ~multiple:false
	 (fun s -> let! s = ohm (seed s) in
		   match s with 
		     | None   -> return [ ]
		     | Some x -> return [x])
	 (fun i v -> let v = match v with [x] -> Some x | _ -> None in
		     parse i v))

end
