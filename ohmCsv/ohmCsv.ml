(* Ohm is © 2011 Victor Nicollet *)

open Ohm
open BatPervasives

let _bom   = 
  let bom = String.create 3 in
  bom.[0] <- Char.chr 239 ;
  bom.[1] <- Char.chr 187 ;
  bom.[2] <- Char.chr 191 ;
  View.str bom

let _comma = View.str ";"
let _endl  = View.str "\n"
let _quot  = View.str "\"" 

let _cell_to_csv cell = 
  let should_escape = 
    try let _ = BatString.find cell "\n" in true with _ -> 
      try let _ = BatString.find cell "\"" in true with _ ->
	try let _ = BatString.find cell ";" in true with _ ->
	  BatString.trim cell <> cell 
  in
  
  if should_escape then 
    let _, escaped = BatString.replace ~str:cell ~sub:"\"" ~by:"\"\"" in
    _quot |- View.str escaped |- _quot
  else
    View.str cell 

let _row_to_csv ctx row =
  match row with 
    | [] -> ctx
    | h :: t ->
      List.fold_left 
	(fun ctx cell -> ctx |> _comma |> _cell_to_csv cell) 
	(_cell_to_csv h ctx) t 
      |> _endl
	
let to_csv columns data ctx = 
  List.fold_left _row_to_csv (_bom ctx) (columns :: data)

let datetime t = 
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d/%02d/%02d %d:%02d"
    (1900 + tm.Unix.tm_year)
    (1 + tm.Unix.tm_mon)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min

let mime = "text/csv"
