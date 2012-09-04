(* Ohm is © 2012 Victor Nicollet *)

type lexer = t -> Buf.t -> Lexing.lexbuf -> unit
and t = Hub of (string -> lexer option) 

let empty = Hub (fun _ -> None) 

let make f = Hub f 

let of_assoc list = Hub (fun k -> try Some (List.assoc k list) with Not_found -> None)

let (+) (Hub a) (Hub b) = 
  Hub (fun x -> match a x with None -> b x | Some y -> Some y ) 

let get (Hub a) key = a key

let descend hub dflt key buf lexbuf = 
  BatOption.default dflt (get hub key) hub buf lexbuf ;
  dflt hub buf lexbuf
