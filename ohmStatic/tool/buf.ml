(* Ohm is © 2012 Victor Nicollet *)

class t = object (self)

  val buffer = Buffer.create 1024

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

  method contents = Buffer.contents buffer

end
