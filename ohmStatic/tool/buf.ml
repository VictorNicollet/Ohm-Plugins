(* Ohm is © 2012 Victor Nicollet *)

let extract content = 
  List.rev_map (function 
    | `URL u -> `URL u 
    | `BUF b -> `RAW (Buffer.contents b)) content 
    
class t = object (self)

  val mutable buffer  = Buffer.create 1024 
  val mutable content = ([] : [`BUF of Buffer.t|`URL of string] list) 
  val mutable json    = ([] : (string * [`BUF of Buffer.t | `URL of string] list) list) 

  method url k = 
    if Buffer.length buffer > 0 then (
      content <- `BUF buffer :: content ;
      buffer  <- Buffer.create 1024
    ) ;
    content <- `URL k :: content 

  method raw s = 
    Buffer.add_string buffer s

  method esc s = 
    self # raw 
      (String.concat "&amp;" 
	 (List.map 
	    (fun s -> String.concat "&lt;" 
	      (List.map 
		 (fun s -> String.concat "&quot;" (BatString.nsplit s "\""))
		 (BatString.nsplit s "&")))
	    (BatString.nsplit s "<")))
	    
  method tag tag attrs f = 
    self # raw "<" ;
    self # raw (String.lowercase tag) ;
    List.iter (fun (key,value) -> 
      self # raw " " ;
      self # raw (String.lowercase key) ;
      self # raw "=\"" ;
      self # esc value ;
      self # raw "\"") attrs ;
    self # raw ">" ;
    (f self : unit) ;
    self # raw "</" ;
    self # raw (String.lowercase tag) ;
    self # raw ">" 

  method contents : [ `URL of string | `RAW of string ] list = 
    if Buffer.length buffer > 0 then content <- `BUF buffer :: content ;
    extract content

  method json : (string * [`URL of string | `RAW of string] list) list = 
    List.map (fun (key,json) -> key, extract json) json 

end
