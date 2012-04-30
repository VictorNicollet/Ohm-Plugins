(* Ohm is © 2012 Victor Nicollet *) 

open Ohm.Universal
open BatPervasives

(* Some basic parser definitions ------------------------------------------------------------ *)

(* Convention: non-aux functions which read the start tag 
   MUST read the corresponding end tag. *)

type ('a,'b) reader = Xmlm.input -> ('a -> 'b) -> 'b
type ('a,'b) setter = Xmlm.input -> 'a -> ('a -> 'b) -> 'b 

(* Read the contents of the next tag as string, recursively descending
   into children tags. If the next signal is not a tag start, return
   nothing and parse nothing. *)

let rec string_aux : (string,'any) setter = fun i s c -> 
  match Xmlm.input i with 
    | `El_start _ -> let! s = string_aux i s in
		     string_aux i s c
    | `El_end     -> c s 
    | `Data     d -> string_aux i (s ^ d) c
    | `Dtd      _ -> string_aux i s c

let string : (string,'any) reader = fun i c -> 
  match Xmlm.peek i with 
    | `El_start _ -> let _ = Xmlm.input i in string_aux i "" c
    | _           -> c "" 

(* Skip the next tag. *)
let rec skip_aux : (unit,'any) reader = fun i c -> 
  match Xmlm.input i with 
    | `El_start _ -> let! () = skip_aux i in skip_aux i c
    | `El_end     -> c ()
    | `Data     _ 
    | `Dtd      _ -> skip_aux i c 

let skip : (unit,'any) reader = fun i c -> 
  match Xmlm.peek i with 
    | `El_start _ -> let _ = Xmlm.input i in skip_aux i c
    | _           -> c ()

(* Explore the descendants of the next tag, looking for a tag that has the provided
   name, and apply the provided reader on it to grab its value. *)
let rec find_aux : Xmlm.name -> ('a,'b) reader -> ('a option,'b) setter = fun n r i d c -> 
  match Xmlm.peek i with 
    | `El_end     -> let _ = Xmlm.input i in c d
    | `Data     _
    | `Dtd      _ -> let _ = Xmlm.input i in find_aux n r i d c 
    | `El_start t -> 
      if d = None && fst t = n then let! v = r i in c (Some v) 
      else let  _ = Xmlm.input i in 
	   let! d = find_aux n r i d in
	   find_aux n r i d c

let find : Xmlm.name -> ('a,'b) reader -> ('a option,'b) reader = fun n r i c -> 
  match Xmlm.peek i with 
    | `El_start _ -> let _ = Xmlm.input i in find_aux n r i None c
    | _           -> c None

(* Explore the descendants of the next tag, looking for ALL tags that have the provided
   name, and apply the provided reader on them to grab their value. *)
let rec filter_aux : Xmlm.name -> ('a,'b) reader -> ('a list,'b) setter = fun n r i l c -> 
  match Xmlm.peek i with 
    | `El_end     -> let _ = Xmlm.input i in c l
    | `Data     _
    | `Dtd      _ -> let _ = Xmlm.input i in filter_aux n r i l c 
    | `El_start t -> 
      if fst t = n then let! v = r i in filter_aux n r i (v :: l) c 
      else let  _ = Xmlm.input i in 
	   let! l = filter_aux n r i l in
	   filter_aux n r i l c

let filter : Xmlm.name -> ('a,'b) reader -> ('a list,'b) reader = fun n r i c -> 
  match Xmlm.peek i with 
    | `El_start _ -> let _ = Xmlm.input i in filter_aux n r i [] c
    | _           -> c []

(* Explore the children of the next tag, and use a tag-setter mapping function to 
   update a provided value. *)
let rec mutate_aux : (Xmlm.name -> ('a,'b) setter option) -> ('a,'b) setter = fun f i x c -> 
  match Xmlm.peek i with 
    | `El_end     -> let _ = Xmlm.input i in c x
    | `Data     _
    | `Dtd      _ -> let _ = Xmlm.input i in mutate_aux f i x c 
    | `El_start t -> 
      match f (fst t) with 
	| None   -> let! _ = skip i in mutate_aux f i x c
	| Some r -> let! x = r i x  in mutate_aux f i x c

let mutate : (Xmlm.name -> ('a,'b) setter option) -> ('a,'b) setter = fun f i x c -> 
  match Xmlm.peek i with 
    | `El_start _ -> let _ = Xmlm.input i in mutate_aux f i x c
    | _           -> c x

(* Seek the first element node in the stream *)

let rec seek_node i = 
  match Xmlm.peek i with 
    | `El_start _ -> () 
    | _           -> let _ = Xmlm.input i in seek_node i 

(* Turn a setter into a reader by providing an initial value *)

let set x s i c = s i x c 

(* Turn a reader into a setter by using an application function *)

let apply f r i x c = 
  let! v = r i in
  c (f v x) 

(* Defining the actual RSS parser. --------------------------------------------------------- *)

module Item = struct

  type t = { 
    title : string option ;
    link  : string option ;
    guid  : string option ;
    description : string option ;
    pubdate : string option ;
    author : string option ;
  } 
      
  let empty_item = { 
    title = None ;
    link  = None ;
    guid  = None ;
    description = None ;
    pubdate = None ;
    author = None;
  } 
    
  let set_title title i = { i with title = Some title }
  let set_link link i = { i with link = Some link } 
  let set_guid guid i = { i with guid = Some guid } 
  let set_description d i = match i.description with 
    | None -> { i with description = Some d } 
    | Some s when String.length s < String.length d -> { i with description = Some d } 
    | Some s -> i 
  let set_pubdate d i = { i with pubdate = Some d } 
  let set_author author i = { i with author = Some author } 

end

let rss i = 
  filter ("","item") 
    (set Item.empty_item 
       (mutate (function 
	 | "","title"       -> Some (apply Item.set_title string)
	 | "","link"        -> Some (apply Item.set_link string)
	 | "","guid"        -> Some (apply Item.set_guid string)

	 | "","description"
	 | "http://purl.org/rss/1.0/modules/content/","encoded" ->
	   Some (apply Item.set_description string)

	 | "","pubDate"     -> Some (apply Item.set_pubdate string)
	 | "","author"      -> Some (apply Item.set_author string)
	 | _                -> None)))
    i identity 

let parse xml = 
  let input = Xmlm.make_input (`String (0,xml)) in 
  let () = seek_node input in
  rss input


