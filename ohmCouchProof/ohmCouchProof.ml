(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

let sign key what = 
  
  let key = 
    let len       = String.length key in
    let o_key_pad = BatString.init 64 (fun i -> Char.chr ((if i < len then Char.code key.[i] else 0) lxor 0x5c)) in
    let i_key_pad = BatString.init 64 (fun i -> Char.chr ((if i < len then Char.code key.[i] else 0) lxor 0x36)) in
    o_key_pad, i_key_pad
  in
  
  let hmac = Util.sha1_hmac key what in
  
  hmac
    
let timed_key key days_ago = 
  let time = Util.string_of_time (Unix.time () -. 3600. *. 24. *. (float_of_int days_ago)) in
  let time = String.sub time 0 8 in
  sign key time 

let rec timed_keys key max_age = 
  if max_age < 0 then [] else
    timed_key key max_age :: timed_keys key (pred max_age)
    
let generate () = 
  let rec mix n = 
    if n = 0 then 
        Printf.sprintf "%x%x%x" 
	  (Random.int (1 lsl 16))
	  (Random.int (1 lsl 16))
	  (Random.int (1 lsl 16))
    else 
      let raw = mix (pred n) in (sign raw raw) 
  in
  mix 10 ^ mix 10

module Make = functor(Db:CouchDB.DATABASE) -> struct

  module Key = Fmt.Make(struct
    type json t = < key : string >
  end)
    
  module Tbl = CouchDB.Table(Db)(Id)(Key)

  let id = Id.of_string "key"

  let key = 
    Run.eval (new CouchDB.init_ctx) begin
      let! found = ohm $ Tbl.get id in
      match found with Some found -> return (found # key) | None -> 
	let key = generate () in
	let! () = ohm $ Tbl.set id (object method key = key end) in
	return key 
    end

  let prove ?(timed=false) ~reason ~thing = 
    let plaintext = reason ^ thing in
    let key = if timed then timed_key key 0 else key in
    sign key plaintext

  let check ?timed ~reason ~thing ~proof = 
    let plaintext = reason ^ thing in
    let keys = match timed with 
      | Some max_age -> timed_keys key max_age
      | None         -> [key]
    in
    List.exists (fun key -> proof = sign key plaintext) keys

  let passhash pass = prove ~timed:false ~reason:"password" ~thing:pass

  module Id = struct

    type t = Id.t
    let delay = 3

    let reason = "identifier"

    let prove id = 
      let thing = Id.to_string id in 
      thing, prove ~timed:true ~reason ~thing

    let check (thing,proof) = 
      if check ~timed:delay ~reason ~thing ~proof then
	Some (Id.of_string thing) 
      else
	None

  end

end
