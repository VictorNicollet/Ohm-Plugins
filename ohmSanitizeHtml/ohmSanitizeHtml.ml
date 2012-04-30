(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal

type inline = [ `Text of string
	      | `A    of string * inline
	      | `I    of inline
	      | `B    of inline
	      | `BR
	      ] list

type doc = inline list

type tree = [ `Text of string
	    | `P    of tree
	    | `A    of string * tree
	    | `I    of tree
	    | `B    of tree 
	    | `BR ] list

module Clean = Fmt.Make(struct

  type t = doc 

  let rec inline_to_json = function 
    | `Text s -> Json_type.String s
    | `A (url,inline) -> Json_type.Object [ "a", Json_type.Build.list inline_to_json inline ;
					    "u", Json_type.String url ]
    | `B inline -> Json_type.Object [ "b", Json_type.Build.list inline_to_json inline ] 
    | `I inline -> Json_type.Object [ "i", Json_type.Build.list inline_to_json inline ] 
    | `BR       -> Json_type.Array []
  
  let rec json_to_inline = function 
    | Json_type.String s -> `Text s
    | Json_type.Array [] -> `BR
    | Json_type.Object [ "b", json ] -> `B (Json_type.Browse.list json_to_inline json) 
    | Json_type.Object [ "i", json ] -> `I (Json_type.Browse.list json_to_inline json) 
    | Json_type.Object [ "a", json ; 
			 "u", Json_type.String u ] 
    | Json_type.Object [ "u", Json_type.String u ;
			 "a", json ] -> `A (u, Json_type.Browse.list json_to_inline json) 
    | _ -> raise (Json_type.Json_error "Incorrect format for OhmSanitizeHtml.Clean") 

  let json_of_t = Json_type.Build.list (Json_type.Build.list inline_to_json)
  let t_of_json = Json_type.Browse.list (Json_type.Browse.list json_to_inline)

end)

let whitespace = Str.regexp "^\\([ \t\r\n]\\|\194\160\\|&nbsp;\\|&emsp;\\|&ensp;\\)*$"
let empty string = 
  Str.string_match whitespace string 0 && Str.match_end () = String.length string
  
let parse lexbuf = 

  let doclist = Nethtml.parse_document 
    ~dtd:Nethtml.relaxed_html40_dtd lexbuf
  in

  (* STEP 1 : Parse the document list into a simplified tree *) 

  let rec tree depth doclist =
    if depth = 50 then [] else
      let tree doclist = tree (depth + 1) doclist in 
      List.concat $ List.map Nethtml.(function
	| Data text -> if empty text then [] else [`Text text]
	| Element (("p"|"center"|"h1"|"h2"|"h3"|"h4"|"h5"|"h6"|"div"
			|"li"|"tr"|"blockquote"|"pre"),_,l) ->  [`P (tree l)] 
	| Element (("b"|"strong"),_,l) -> [`B (tree l)] 
	| Element (("i"|"em"),_,l) -> [`I (tree l)]
	| Element (("br"|"hr"),_,l) -> if l = [] then [`BR] else `BR :: tree l  
	| Element ("a",a,l) -> begin 
	  try let url = List.assoc "href" a in
	      [`A (url,tree l)] 
	  with _ -> tree l
	end  
	| Element (("u"|"table"|"td"|"th"|"tbody"|"thead"|"tfoot"
			|"label"|"code"|"tt"|"font"|"span"
			|"button"|"ul"|"ol"|"small"|"strike"|"kbd"
			|"ins"|"del"),_,l) -> tree l
	| _ -> []) doclist
  in
  
  let tree = tree 0 doclist in 

  (* STEP 2 : Prefix-traverse the tree, "printing" to the output. The result is a
     dirty but correct doc-block-inline structure *)

  let commit (doc,block) = if block = [] then (doc,[]) else (List.rev block :: doc, [] ) in

  let rec traverse (doc,block) ~b ~i ?a = function 
    | []           -> (doc,block) 
    | `Text t :: l -> let o = `Text t in
		      let o = if b then `B [o] else o in
		      let o = if i then `I [o] else o in
		      let o = match a with Some s -> `A (s,[o]) | None -> o in
		      traverse (doc,o :: block) ~b ~i ?a l 
    | `B list :: l -> let doc, block = traverse (doc,block) ~b:true ~i ?a list in
		      traverse (doc, block) ~b ~i ?a l 
    | `I list :: l -> let doc, block = traverse (doc,block) ~b ~i:true ?a list in
		      traverse (doc, block) ~b ~i ?a l 
    | `BR :: l     -> traverse (doc,`BR :: block) ~b ~i ?a l 
    | `A(u,t) :: l -> let doc, block = traverse (doc,block) ~b ~i ~a:u t in
		      traverse (doc, block) ~b ~i ?a l 
    | `P list :: l -> let doc, block = traverse (commit (doc,block)) ~b ~i ?a list in
		      traverse (commit (doc,block)) ~b ~i ?a l 
  in

  let revdoc, _ = commit $ traverse ([],[]) ~b:false ~i:false tree in
  let doc = List.rev revdoc in 
  
  (* STEP 3 : printing has created a lot of separate A/B/I tags. Merge adjacent tags.
     This step also removes duplicate BR tags, as well as `BR tags at the end of the
     list and empty or whitespace-filled tags. *)
  
  let rec dedup = function
    | []  -> [] 
    | [`BR] -> []  
    | `B a :: `B b :: t -> dedup (`B (a @ b) :: t) 
    | `I a :: `I b :: t -> dedup (`I (a @ b) :: t)
    | `BR :: `BR :: t -> dedup (`BR :: t)  
    | `BR :: t -> `BR :: dedup t 
    | `A (ua,a) :: `A (ub,b) :: t when ua = ub -> dedup (`A (ua, a @ b) :: t) 
    | `B l :: t -> let l = dedup l in if l = [] then dedup t else `B l :: dedup t
    | `I l :: t -> let l = dedup l in if l = [] then dedup t else `I l :: dedup t
    | `A (u,l) :: t -> let l = dedup l in if l = [] then dedup t else `A (u,l) :: dedup t  
    | `Text a :: t when empty a -> dedup t 
    | `Text a :: `Text b :: t -> dedup (`Text (a ^ b) :: t)
    | `Text a :: t -> `Text a :: dedup t 
  in


  let doc = List.map dedup doc in

  (* STEP 4 : some paragraphs may start with a BR (not several, because we 
     deduplicated them. Remove it. Also, if the resulting paragraph is empty, 
     drop it.
  *)

  let doc = BatList.filter_map (function 
    | [`BR] | [] -> None
    | `BR :: l | l -> Some l) doc
  in 
  
  doc 

let parse_string string = 
  let lexbuf = Lexing.from_string string in 
  parse lexbuf

let secure_link link = 
  if BatString.starts_with link "http://" then link else
    if BatString.starts_with link "https://" then link else
      if BatString.starts_with link "mailto:" then link else
	"http://" ^ link

(* Print a document as some HTML *) 
let html doc = 
  let b = Buffer.create 1024 in 

  let rec recprint = function 
    | `Text t -> Buffer.add_string b t 
    | `BR     -> Buffer.add_string b "<br/>" 
    | `B l    -> Buffer.add_string b "<b>" ; List.iter recprint l ; Buffer.add_string b "</b>"
    | `I l    -> Buffer.add_string b "<i>" ; List.iter recprint l ; Buffer.add_string b "</i>"
    | `A(u,l) -> let url = secure_link u in
		 Buffer.add_string b "<a rel=\"nofollow\" href=\"" ;
		 Buffer.add_string b url ;
		 Buffer.add_string b "\" target=\"_blank\">" ;
		 List.iter recprint l ;
		 Buffer.add_string b "</a>"
  in

  List.iter (fun block -> 
    Buffer.add_string b "<p>" ;
    List.iter recprint block ;
    Buffer.add_string b "</p>" ;
  ) doc ;

  Buffer.contents b 

(* Print a document to text-only format. *)
let text doc = 
  let b = Buffer.create 1024 in 

  let rec recprint = function 
    | `Text t -> Buffer.add_string b t 
    | `BR     -> Buffer.add_string b "\n"
    | `B l    -> List.iter recprint l
    | `I l    -> List.iter recprint l
    | `A(u,l) -> let url = secure_link u in
		 List.iter recprint l ;
		 Buffer.add_string b " (" ;
		 Buffer.add_string b url ;
		 Buffer.add_string b ")"
  in
  
  let first = ref true in 

  List.iter (fun block -> 
    if not !first then Buffer.add_string b "\n\n" ;
    List.iter recprint block ;
    first := false 
  ) doc ;

  Buffer.contents b   

(* Length of a document *)
let length doc = 
  let rec block_size block = List.fold_left (fun acc item -> acc + item_size item) 0 block
  and item_size = function
    | `Text t -> String.length t
    | `BR -> 1
    | `A(_,l) | `B l | `I l -> block_size l
  in
  List.fold_left (fun acc block -> acc + block_size block) 0 doc

(* Cut a clean piece after a certain number of characters. *)
let cut_text count text =
  let regex = Str.regexp "[^\n\t ]+" in
  let rec search acc i = 
    try 
      ignore (Str.search_forward regex text i) ;
      let matched = Str.matched_string text in
      if acc = "" then 
	search matched (Str.match_end ())
      else if String.length acc + String.length matched > count then 
	acc ^ " ..."
      else 
	search (acc ^ " " ^ matched) (Str.match_end ())
    with Not_found ->
      acc
  in
  search "" 0

let cut ~max_lines ~max_chars (clean:doc) = 

  let rec subcut max_l max_c (l:inline) = match l with 

    | _ when max_l <= 0 || max_c < 0 -> (max_l,max_c,[])
    | _ when max_c = 0 -> (max_l,-1,[`Text " ..."])

    | []           -> (max_l,max_c,[]) 

    | `Text t :: l -> let max_c' = max_c - String.length t in 
		      if max_c' < 0 then (max_l,max_c',[`Text (cut_text max_c t)]) 
		      else let max_l, max_c, list = subcut max_l max_c' l in
			   max_l, max_c, `Text t :: list

    | `A(u,i) :: l -> let max_l, max_c, i = subcut max_l max_c i in
		      let max_l, max_c, l = subcut max_l max_c l in
		      max_l, max_c, `A(u,i) :: l
			
    | `I i    :: l -> let max_l, max_c, i = subcut max_l max_c i in
		      let max_l, max_c, l = subcut max_l max_c l in
		      max_l, max_c, `I i :: l
			
    | `B i    :: l -> let max_l, max_c, i = subcut max_l max_c i in
		      let max_l, max_c, l = subcut max_l max_c l in
		      max_l, max_c, `B i :: l
			
    | `BR     :: l -> let max_l, max_c, l = subcut (max_l - 1) max_c l in
		      max_l, max_c, `BR :: l
  in

  let rec cut max_l max_c = function 
    | [] -> [] | block :: list -> 
      if max_l < -1 || max_c < 0 then [] else 
	if max_l <= 0 || max_c = 0 then [[`Text "[...]"]] else
	  let max_l, max_c, block = subcut (max_l - 2) max_c block in
	  let rest = cut max_l max_c list in
	  if block = [] then rest else block :: rest
  in

  cut max_lines max_chars clean
