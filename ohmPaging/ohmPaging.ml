(* Ohm is © 2012 Victor Nicollet *)

let slice ~count list = 
  try match BatList.split_at count list with 
    | list, [] -> list, None
    | list, h :: _ -> list, Some h 
  with _ -> list, None

