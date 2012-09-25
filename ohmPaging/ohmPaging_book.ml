(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

type ('key,'data) page = <
  first : bool ; 
  last  : bool ;
  key   : 'key ;
  data  : 'data ;
  pos   : int ;
  sel   : bool ;
  url   : string option ;
  prev  : 'key option ;
  next  : 'key option ; 
>

type ('key,'data) node = <
  prev : ('key,'data) page option ;
  next : ('key,'data) page option ;
  page : ('key,'data) page 
>

type ('key,'data) prev_next =  <
  prev : ('key,'data) page option ;
  next : ('key,'data) page option ;
>

type ('key,'data) t = ('key,'data) node list * ('key, ('key,'data) node) BatPMap.t

let no_url _ = None

let selected mkurl key page = object
  method first = page # first
  method last  = page # last
  method key   = page # key
  method data  = page # data
  method pos   = page # pos 
  method sel   = page # key = key 
  method prev  = page # prev
  method next  = page # next
  method url   = mkurl (page # key)
end

let with_url mkurl page = object
  method first = page # first
  method last  = page # last
  method key   = page # key
  method data  = page # data
  method pos   = page # pos 
  method sel   = page # sel
  method prev  = page # prev
  method next  = page # next
  method url   = mkurl (page # key)
end

let make list = 
  let array  = Array.of_list list in
  let n      = Array.length array in
  
  let linked = Array.init n (fun i ->
    let key, data = array.(i) in
    (object
      method first = i = 0
      method last  = i = n - 1
      method key   = key
      method data  = data
      method pos   = i 
      method sel   = false
      method prev  = if i > 0 then Some (fst array.(i-1)) else None
      method next  = if i < n - 1 then Some (fst array.(i+1)) else None
      method url   = None
     end)
  ) in

  let nodes = Array.init n (fun i ->
    let page = linked.(i) in
    (object
      method prev = if i > 0 then Some linked.(i-1) else None
      method next = if i < n - 1 then Some linked.(i+1) else None
      method page = page
     end)
  ) in

  Array.to_list nodes,
  Array.fold_left (fun map item -> BatPMap.add (item # page # key) item map) BatPMap.empty nodes 

let full_list ?(url=no_url) (t,_) = 
  List.map (#page |- with_url url) t

let list ?(url=no_url) key (t,m) =
  if BatPMap.mem key m then 
    Some (List.map (#page |- selected url key) t)
  else
    None

let prev_next ?(url=no_url) key (_,m) =
  try 
    let node = BatPMap.find key m in 
    Some (object
      method prev = BatOption.map (with_url url) (node # prev)
      method next = BatOption.map (with_url url) (node # next) 
    end)
  with Not_found -> 
    None
