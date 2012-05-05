(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

(* Preliminary definitions and utility functions *)

let tab_spaces = 4
let is_tab     = function '\t' -> true | _ -> false

let length string = 
  String.length string + (tab_spaces - 1) * String.length (BatString.filter is_tab string)

type initial = 
  [ `ITALICS         of string
  | `BOLD            of string
  | `START_IMAGE     of string
  | `START_LINK      of string
  | `URL_INLINE      of string
  | `INLINE_END      of string
  | `QUOTE           of string
  | `ESC             of string
  | `ORDERED_LIST    of string
  | `NEWLINE         of string
  | `HASHES          of string
  | `WHITESPACE      of string
  | `BLOCKQUOTE      of string
  | `CODE            of string
  | `CODE_ESC        of string
  | `UNORDERED_LIST  of string
  | `HTML            of string
  | `ENT             of string
  | `STR             of string
  | `AMP             of string
  | `EOF  
  ]

type final = 
  [ `I_OPEN
  | `B_OPEN
  | `A_OPEN
  | `H_OPEN of int
  | `UL_OPEN
  | `OL_OPEN
  | `QUOTE_OPEN
  | `PRE_OPEN
  | `PAR
  | `I_CLOSE
  | `B_CLOSE
  | `A_CLOSE
  | `EOF
  | `CODE_OPEN
  | `CODE_CLOSE
  | `A_CLOSE_REAL of (string * string option) 
  | `UL_CLOSE
  | `OL_CLOSE
  | `H_CLOSE of int
  | `QUOTE_CLOSE
  | `PRE_CLOSE    
  | `STR of string
  | `HTML of string
  | `IMG of (string * string * string option)
  | `ENT of string
  ]

let print = function
  | `ITALICS         _ -> "[ITALICS]"
  | `BOLD            _ -> "[BOLD]"
  | `START_IMAGE     _ -> "[BEG_IMG]"
  | `START_LINK      _ -> "[BEG_LNK]"
  | `URL_INLINE      _ -> "[URL_INL]"
  | `INLINE_END      _ -> "[END_INL]"
  | `QUOTE           _ -> "[QUOTE]"
  | `ESC             _ -> "[ESC]"
  | `ORDERED_LIST    _ -> "[OLIST]"
  | `NEWLINE         _ -> "[NEWLINE]"
  | `HASHES          _ -> "[HASHES]"
  | `WHITESPACE      _ -> "[WHTSPC]"
  | `BLOCKQUOTE      _ -> "[BQUOTE]"
  | `CODE            _ -> "[CODE]"
  | `CODE_ESC        _ -> "[CODEESC]"
  | `UNORDERED_LIST  _ -> "[ULIST]"
  | `HTML            _ -> "[HTML]"
  | `ENT             _ -> "[ENTITY]"
  | `STR             s -> s
  | `AMP             _ -> "[AMP]"
  | `EOF               -> "[EOF]"
    
  | `LINE            _ -> "[LINE]"
  | `PARSKIP         _ -> "\n[PARSKIP]\n"
  | `PAR               -> "\n[PAR]\n"
  | `PRE             _ -> "[PRE]"
  | `UL_OPEN           -> "<ul>"
  | `UL_CLOSE          -> "</ul>"
  | `OL_OPEN           -> "<ol>"
  | `OL_CLOSE          -> "</ol>"
  | `QUOTE_OPEN        -> "<bq>"
  | `QUOTE_CLOSE       -> "</bq>"
  | `PRE_OPEN          -> "<pre>"
  | `PRE_CLOSE         -> "</pre>"
  | `H_OPEN          n -> "<h" ^ string_of_int n ^ ">"
  | `H_CLOSE         n -> "</h" ^ string_of_int n ^ ">"
    
  | `CODE_OPEN         -> "<code>"
  | `CODE_CLOSE        -> "</code>"
  | `I_OPEN            -> "<em>"
  | `I_CLOSE           -> "</em>"
  | `B_OPEN            -> "<strong>"
  | `B_CLOSE           -> "</strong>"
  | `A_OPEN            -> "<a>"
  | `A_CLOSE           -> "</a>"
  | `A_CLOSE_REAL(u,t) -> "</a href='"^u^"' title='"^BatOption.default "null" t^"'>"
  | `IMG(u,a,t)        -> "<img src='"^u^"' alt='"^a^"' title='"^BatOption.default "null" t^"'/>"


let tokprint token lexbuf = 
  let tok = token lexbuf in
  Util.log "%s" (print tok) ; tok 

let stream (source : Lexing.lexbuf -> initial) lexbuf = 

  let token lexbuf = source lexbuf in 

  (* Turn newlines followed by whitespace, blockquotes, unordered and 
     ordered list starts into newlines followed by the sequence of
     whitespace, blockquote and list starts. Also converts the unused 
     symbols back to their original values. *)

  let lookahead = ref (`NEWLINE "\n") in
  let memory    = ref None in 
  let token lexbuf = 

    match !memory with Some tok -> (memory := None ; tok) | None -> 

      let current = !lookahead in
      lookahead := token lexbuf ;
      
      match current with 
	| `ITALICS         s -> `ITALICS s 
	| `BOLD            s -> `BOLD s
	| `AMP             s -> `AMP s
	| `START_IMAGE     s -> `START_IMAGE s
	| `START_LINK      s -> `START_LINK s
	| `URL_INLINE      s -> `URL_INLINE s
	| `INLINE_END      s -> `INLINE_END s
	| `QUOTE           s -> `QUOTE s
	| `ESC             s -> `ESC s
	| `ORDERED_LIST    s -> `STR s
	| `HASHES          s -> `HASHES s
	| `WHITESPACE      s -> `STR (String.make (length s) ' ' )
	| `BLOCKQUOTE      s -> `STR ">"
	| `CODE            s -> `CODE s
	| `CODE_ESC        s -> `CODE_ESC s
	| `UNORDERED_LIST  s -> if s.[0] = '*' then 
	                          ( memory := Some (`STR (String.make (length s - 1) ' ')) ; `ITALICS "*" )
                                else
	                          `STR s
	| `HTML            s -> `HTML s
	| `ENT             s -> `ENT s
	| `STR             s -> `STR s
	| `EOF               -> `EOF
	| `NEWLINE         _ -> begin
	  let rec count_start w l = function
	    | `WHITESPACE s -> count_start (w + length s) l (token lexbuf)
	    | `HASHES s -> let l = if w = 0 then l else `W w :: l in
			   let len = String.length s in
			   if len < 7 then begin
			     let l = `H len :: l in
			     count_start 0 l (token lexbuf) 
			   end else begin 			     
			     lookahead := `HASHES s ;
			     List.rev l 
			   end
	    | `UNORDERED_LIST s -> let l = if w = 0 then l else `W w :: l in
				   let l = `U s :: l in
				   count_start 0 l (token lexbuf)
	    | `ORDERED_LIST s -> let l = if w = 0 then l else `W w :: l in
				 let l = `O s :: l in
				 count_start 0 l (token lexbuf)
	    | `BLOCKQUOTE s -> let l = if w = 0 then l else `W w :: l in
			       let l = `Q s :: l in
			       count_start 0 l (token lexbuf)
	    | other         -> let l = if w = 0 then l else `W w :: l in
			       lookahead := other ;
			       List.rev l 
	  in
	  let start = count_start 0 [] !lookahead in 
	  `LINE start
	end 
	  
  in     

  (* Turn consecutive sequences of newlines into a `PARSKIP followed
     by the last newline in the sequence. *)
  
  let lookahead = ref (token lexbuf) in
  let memory    = ref None in 
  let token lexbuf = 

    match !memory with Some t -> (memory := None ; t ) | None -> 
      
      let current = !lookahead in
      lookahead := token lexbuf ;
      
      match current with 
	| `ITALICS         s -> `ITALICS s 
	| `BOLD            s -> `BOLD s
	| `AMP             s -> `AMP s
	| `START_IMAGE     s -> `START_IMAGE s
	| `START_LINK      s -> `START_LINK s
	| `URL_INLINE      s -> `URL_INLINE s
	| `INLINE_END      s -> `INLINE_END s
	| `QUOTE           s -> `QUOTE s
	| `ESC             s -> `ESC s
	| `HASHES          s -> `HASHES s
	| `CODE            s -> `CODE s
	| `CODE_ESC        s -> `CODE_ESC s
	| `HTML            s -> `HTML s
	| `ENT             s -> `ENT s
	| `STR             s -> `STR s
	| `EOF               -> `EOF
	| `LINE            l -> begin
	  let rec drop_newlines count last = function 
	    | `LINE l -> drop_newlines (count + 1) l (token lexbuf) 
	    | other   -> lookahead := other ; count, `LINE last
	  in
	  let count, tok = drop_newlines 0 l !lookahead in
	  if count > 0 then ( memory := Some tok ; `PARSKIP count) else tok 
	end
  in

  (* Extract verbatim code inserts based on the current depth of the line, along
     with `UL_OPEN, `UL_CLOSE, `OL_OPEN, `OL_CLOSE, `QUOTE_OPEN and `QUOTE_CLOSE
     instructions for managing blocks. *)

  let stack     = ref (`Weak []) in
  let memory    = ref [] in
  let had_pre   = ref false in
  let token lexbuf = 

    let rec excess_to_string = function    
      | `W w :: l -> String.make w ' ' ^ excess_to_string l 
      | `Q s :: l -> s ^ excess_to_string l 
      | `O s :: l -> s ^ excess_to_string l 
      | `U s :: l -> s ^ excess_to_string l 
      | `H n :: l -> String.make n '#' ^ excess_to_string l 
      | []        -> ""
    in

    let rec diff weak add rem = function 

      | [], [] -> add, rem

      | [], `W w :: l when weak && add = [] || w < 4 -> diff weak add rem ([],l) 
      | [], `W w :: l -> let excess = `W (w-4) :: l in
			 `PRE (excess_to_string excess) :: add, rem 
     
      | [], `Q _ :: l -> diff weak (`Q :: add) rem ([],l) 
      | [], `U _ :: l -> diff weak (`U :: add) rem ([],l)
      | [], `O _ :: l -> diff weak (`O :: add) rem ([],l)
      | [], `H n :: l -> diff weak (`H n :: add) rem ([],l)

      | `Q :: l, `W w :: l' when w < 4 -> diff weak add rem (`Q :: l, l') 
      | `Q :: l, `Q _ :: l' -> diff weak add rem (l,l')
      | `Q :: l, rest       -> diff weak add (List.length l + 1) ([],rest)

      | `U :: l, `W w :: l' when w < 4 -> diff weak add rem (`U :: l, l') 
      | `U :: l, `W w :: l' when w = 4 -> diff weak add rem (l, l') 
      | `U :: l, `W w :: l' -> diff weak add rem (l, `W (w-4) :: l') 
      | `U :: l, [] when weak -> diff weak add rem (l,[])
      | `U :: l, rest -> diff weak add (List.length l + 1) ([],rest)

      | `O :: l, `W w :: l' when w < 4 -> diff weak add rem (`U :: l, l') 
      | `O :: l, `W w :: l' when w = 4 -> diff weak add rem (l, l') 
      | `O :: l, `W w :: l' -> diff weak add rem (l, `W (w-4) :: l') 
      | `O :: l, [] when weak -> diff weak add rem (l,[])
      | `O :: l, rest -> diff weak add (List.length l + 1) ([],rest)      

      | `H n :: l, rest -> diff weak add (List.length l + 1) ([],rest)

    in

    let do_close = function 
      | `H n -> `H_CLOSE n 
      | `O   -> `OL_CLOSE
      | `U   -> `UL_CLOSE
      | `Q   -> `QUOTE_CLOSE
    in 

    let do_open = function
      | `PRE s -> `PRE s
      | `H n   -> `H_OPEN n
      | `O     -> `OL_OPEN
      | `U     -> `UL_OPEN
      | `Q     -> `QUOTE_OPEN
    in

    let do_clean = function
      | `PRE s -> None
      | `H n   -> Some (`H n)
      | `O     -> Some `O
      | `U     -> Some `U
      | `Q     -> Some `Q
    in

    let is_pre = function `PRE _ -> true | _ -> false in

    let update prefix = 

      let weak, current = match !stack with 
	| `Weak l -> true, l
	| `Strong l -> false, l 
      in

      let add, rem = diff (weak && not !had_pre) [] 0 (current,prefix) in

      let kept, cut = BatList.split_at (List.length current - rem) current in       

      memory := (List.rev_map do_close cut @ List.map do_open add) ;          
 
      stack := `Weak (kept @ BatList.filter_map do_clean add) ;

      had_pre := List.exists is_pre add 
	
    in

    let rec next lexbuf = 

      match !memory with h :: t -> (memory := t ; h ) | [] -> 
	
	match token lexbuf with 
	  | `ITALICS         s -> `ITALICS s 
	  | `BOLD            s -> `BOLD s
	  | `AMP             s -> `AMP s
	  | `START_IMAGE     s -> `START_IMAGE s
	  | `START_LINK      s -> `START_LINK s
	  | `URL_INLINE      s -> `URL_INLINE s
	  | `INLINE_END      s -> `INLINE_END s
	  | `QUOTE           s -> `QUOTE s
	  | `ESC             s -> `ESC s
	  | `HASHES          s -> `HASHES s
	  | `CODE            s -> `CODE s
	  | `CODE_ESC        s -> `CODE_ESC s
	  | `HTML            s -> `HTML s
	  | `ENT             s -> `ENT s
	  | `STR             s -> `STR s

	  | `EOF               -> let l = match !stack with `Strong l | `Weak l -> l in 
				  if l = [] then `EOF else (
				    stack := `Strong [] ;
				    memory := (List.rev_map do_close l) ;
				    next lexbuf)

	  | `PARSKIP         c -> let l = match !stack with `Strong l | `Weak l -> l in
				  stack := `Strong l ;
				  `PARSKIP c	  

	  | `LINE            p -> update p ; next lexbuf
    in

    next lexbuf

  in
  
  (* Convert the contents of a PRE to an OPEN_PRE, several STR, and a CLOSE_PRE *)

  let lookahead = ref (token lexbuf) in 
  let memory    = ref None in 
  let is_pre    = ref false in
  let token lexbuf = 
    match !memory with Some tok -> ( memory := None ; tok ) | None ->
      
      let current = !lookahead in
      lookahead := token lexbuf ;

      if !is_pre then begin

	match current with 
	  | `ITALICS         s 
	  | `BOLD            s	    
	  | `START_IMAGE     s
	  | `START_LINK      s
	  | `URL_INLINE      s
	  | `INLINE_END      s
	  | `QUOTE           s
	  | `ESC             s
	  | `HASHES          s
	  | `CODE            s
	  | `CODE_ESC        s
	  | `STR             s -> `STR s	 
	  | `AMP             _ -> `STR "&amp;"
	  | `HTML            _ -> `STR "&lt;"
	  | `ENT             s -> `STR ("&amp;" ^ BatString.lchop s) 
	  | `H_CLOSE n         -> (memory := Some (`H_CLOSE n) ; is_pre := false ; `PRE_CLOSE) 
	  | `OL_CLOSE          -> (memory := Some `OL_CLOSE ; is_pre := false ; `PRE_CLOSE) 
	  | `UL_CLOSE          -> (memory := Some `UL_CLOSE ; is_pre := false ; `PRE_CLOSE) 
	  | `QUOTE_CLOSE       -> (memory := Some `QUOTE_CLOSE ; is_pre := false ; `PRE_CLOSE) 
	  | `H_OPEN n          -> (memory := Some (`H_OPEN n); is_pre := false ; `PRE_CLOSE) 
	  | `OL_OPEN           -> (memory := Some `OL_OPEN ; is_pre := false ; `PRE_CLOSE) 
	  | `UL_OPEN           -> (memory := Some `UL_OPEN ; is_pre := false ; `PRE_CLOSE) 
	  | `QUOTE_OPEN        -> (memory := Some `QUOTE_OPEN ; is_pre := false ; `PRE_CLOSE) 
	  | `EOF               -> (is_pre := false ; `PRE_CLOSE )
	  | `PARSKIP         c -> begin
	    match !lookahead with 
	      | `PRE s -> `STR (String.make c '\n')
	      | other  -> is_pre := false ; memory := Some `PAR ; `PRE_CLOSE
	  end
	  | `PRE             s -> begin
	    memory := Some (`STR s) ; `STR "\n"
	  end

      end else begin

	match current with 
	  | `ITALICS         s -> `ITALICS s 
	  | `BOLD            s -> `BOLD s
	  | `AMP             s -> `AMP s
	  | `START_IMAGE     s -> `START_IMAGE s
	  | `START_LINK      s -> `START_LINK s
	  | `URL_INLINE      s -> `URL_INLINE s
	  | `INLINE_END      s -> `INLINE_END s
	  | `QUOTE           s -> `QUOTE s
	  | `ESC             s -> `ESC s
	  | `HASHES          s -> `HASHES s
	  | `CODE            s -> `CODE s
	  | `CODE_ESC        s -> `CODE_ESC s
	  | `HTML            s -> `HTML s
	  | `ENT             s -> `ENT s
	  | `STR             s -> `STR s
	  | `PARSKIP         c -> `PAR
	  | `H_CLOSE         n -> `H_CLOSE n 
	  | `OL_CLOSE          -> `OL_CLOSE 
	  | `UL_CLOSE          -> `UL_CLOSE 
	  | `QUOTE_CLOSE       -> `QUOTE_CLOSE 
	  | `H_OPEN          n -> `H_OPEN n 
	  | `OL_OPEN           -> `OL_OPEN 
	  | `UL_OPEN           -> `UL_OPEN 
	  | `QUOTE_OPEN        -> `QUOTE_OPEN 
	  | `EOF               -> `EOF
	  | `PRE             s -> begin
	    is_pre := true ; memory := Some (`STR s) ; `PRE_OPEN
	  end
      end
  in

  (* Move `PAR after the various `_CLOSE tags. If this would cause two
     `PAR to become consecutive (which should in theory never happen) then
     all consecutive `PAR save one are discarded. *)

  let lookahead = ref (token lexbuf) in
  let token lexbuf = 

    let rec next lexbuf = 

      let current = !lookahead in
      lookahead := token lexbuf ; 

      if current = `PAR then
	match !lookahead with 
	  | ( `QUOTE_CLOSE | `H_CLOSE _ | `OL_CLOSE | `UL_CLOSE ) as close ->
	    (lookahead := `PAR ; close )
	  | `PAR  -> next lexbuf 
	  | other -> `PAR
      else current
    in

    next lexbuf

  in

  (* Remove `HASHES followed by a `H_CLOSE. *)

  let lookahead = ref (token lexbuf) in
  let token lexbuf = 

    let rec next lexbuf = 

      let current = !lookahead in
      lookahead := token lexbuf ; 

      match current with 
	| `ITALICS         s -> `ITALICS s 
	| `BOLD            s -> `BOLD s
	| `AMP             s -> `AMP s
	| `START_IMAGE     s -> `START_IMAGE s
	| `START_LINK      s -> `START_LINK s
	| `URL_INLINE      s -> `URL_INLINE s
	| `INLINE_END      s -> `INLINE_END s
	| `QUOTE           s -> `QUOTE s
	| `ESC             s -> `ESC s
	| `CODE            s -> `CODE s
	| `CODE_ESC        s -> `CODE_ESC s
	| `HTML            s -> `HTML s
	| `ENT             s -> `ENT s
	| `STR             s -> `STR s
	| `PAR               -> `PAR
	| `PRE_CLOSE         -> `PRE_CLOSE 
	| `H_CLOSE         n -> `H_CLOSE n 
	| `OL_CLOSE          -> `OL_CLOSE 
	| `UL_CLOSE          -> `UL_CLOSE 
	| `QUOTE_CLOSE       -> `QUOTE_CLOSE 
	| `PRE_OPEN          -> `PRE_OPEN
	| `H_OPEN          n -> `H_OPEN n 
	| `OL_OPEN           -> `OL_OPEN 
	| `UL_OPEN           -> `UL_OPEN 
	| `QUOTE_OPEN        -> `QUOTE_OPEN 
	| `EOF               -> `EOF
 	| `HASHES          s -> begin
	  match !lookahead with 
	    | `H_CLOSE _ -> next lexbuf
	    | other      -> `STR s 
	end
    in
    
    next lexbuf
      
  in

  (* Turn `CODE and `CODE_ESC into appropriate `CODE_OPEN and `CODE_CLOSE *)

  let code_type = ref `NONE in
  let memory = ref None in 
  let token lexbuf = 
    let flush tok = memory := Some tok ; code_type := `NONE ; `CODE_CLOSE in
    match !memory with Some m -> (memory := None ; m) | None -> 
      match !code_type with 
	| `NONE -> begin
	  match token lexbuf with 
	    | `ESC             q -> `ESC q
	    | `PRE_CLOSE         -> `PRE_CLOSE
	    | `H_CLOSE         n -> `H_CLOSE n
	    | `OL_CLOSE          -> `OL_CLOSE
	    | `UL_CLOSE          -> `UL_CLOSE
	    | `QUOTE_CLOSE       -> `QUOTE_CLOSE
	    | `PRE_OPEN          -> `PRE_OPEN
	    | `H_OPEN          n -> `H_OPEN n
	    | `OL_OPEN           -> `OL_OPEN
	    | `UL_OPEN           -> `UL_OPEN
	    | `QUOTE_OPEN        -> `QUOTE_OPEN
	    | `PAR               -> `PAR
	    | `EOF               -> `EOF
	    | `URL_INLINE      s -> `URL_INLINE s
	    | `ITALICS         s -> `ITALICS s
	    | `BOLD            s -> `BOLD s
	    | `START_LINK      s -> `START_LINK s
	    | `STR             s -> `STR s
	    | `ENT             s -> `ENT s
	    | `START_IMAGE     s -> `START_IMAGE s
	    | `HTML            s -> `HTML s
	    | `AMP             s -> `AMP s 
	    | `INLINE_END      s -> `INLINE_END s 
	    | `QUOTE           s -> `QUOTE s
	    | `CODE            s -> ( code_type := `NORMAL ; `CODE_OPEN ) 
	    | `CODE_ESC        s -> ( code_type := `ESCAPE ; `CODE_OPEN ) 
	end
	| other -> begin
	  match token lexbuf with 
	    | `PRE_CLOSE         -> flush `PRE_CLOSE
	    | `H_CLOSE         n -> flush (`H_CLOSE n)
	    | `OL_CLOSE          -> flush `OL_CLOSE
	    | `UL_CLOSE          -> flush `UL_CLOSE
	    | `QUOTE_CLOSE       -> flush `QUOTE_CLOSE
	    | `PRE_OPEN          -> flush `PRE_OPEN
	    | `H_OPEN          n -> flush (`H_OPEN n)
	    | `OL_OPEN           -> flush `OL_OPEN
	    | `UL_OPEN           -> flush `UL_OPEN
	    | `QUOTE_OPEN        -> flush `QUOTE_OPEN
	    | `PAR               -> flush `PAR
	    | `EOF               -> flush `EOF
	    | `URL_INLINE      s 
	    | `ITALICS         s 
	    | `BOLD            s 
	    | `START_LINK      s 
	    | `START_IMAGE     s
	    | `INLINE_END      s
	    | `QUOTE           s
	    | `STR             s -> `STR s
	    | `ENT             s -> `STR ("&amp;"  ^BatString.lchop s)
	    | `HTML            s -> `STR "&lt;"
	    | `AMP             s -> `STR "amp;" 
	    | `ESC             q -> `STR (String.make 1 q.[1]) 
	    | `CODE            s -> 
	      if other = `NORMAL then ( code_type := `NONE ; `CODE_CLOSE ) 
	      else `STR "`"
	    | `CODE_ESC        s ->
	      if other = `ESCAPE then ( code_type := `NONE ; `CODE_CLOSE ) 
	      else `STR "`"	      
	end
  in
  
  (* Use a state machine to parse the inline blocks. *)

  let stack  = ref [] in
  let memory = ref [] in
  let token lexbuf =     

    let do_close = function 
      | `A   -> `A_CLOSE
      | `B _ -> `B_CLOSE
      | `I _ -> `I_CLOSE
    in

    let flush token = 
      match List.rev_map do_close !stack @ [token] with 
	| [] -> assert false (* There's at least the token we provided *)
	| h :: t -> memory := t ; stack := [] ; h
    in

    match !memory with h :: t -> (memory := t ; h) | [] ->     
      match token lexbuf with 
	| `ESC             q -> `STR (String.make 1 q.[1])
	| `AMP             s -> `STR "&amp;"
	| `QUOTE           s -> `STR s
	| `STR             s -> `STR s 
	| `INLINE_END      s -> `STR s
	| `ENT             s -> `ENT s
	| `HTML            h -> `HTML h 
	| `CODE_OPEN         -> `CODE_OPEN
	| `CODE_CLOSE        -> `CODE_CLOSE
	| `START_IMAGE     s -> begin 

	  let rec url acc = 
	    match token lexbuf with 
	      | `ESC             q -> url (acc ^ String.make 1 q.[1]) 
	      | `PRE_CLOSE         
	      | `H_CLOSE         _ 
	      | `OL_CLOSE          
	      | `UL_CLOSE         
	      | `QUOTE_CLOSE      
	      | `PRE_OPEN         
	      | `H_OPEN          _
	      | `OL_OPEN          
	      | `UL_OPEN          
	      | `QUOTE_OPEN       
	      | `PAR              
	      | `CODE_OPEN         
	      | `CODE_CLOSE        
	      | `EOF               -> acc, None
	      | `URL_INLINE      s 
	      | `ITALICS         s 
	      | `BOLD            s 
	      | `START_LINK      s
	      | `STR             s 
	      | `ENT             s 
	      | `START_IMAGE     s -> url (acc ^ s)
	      | `HTML            s -> url (acc ^ "&lt;")
	      | `AMP             s -> url (acc ^ "&amp;") 
	      | `INLINE_END      _ -> acc, None
	      | `QUOTE           q -> 
		let rec grab acc = 
		  match token lexbuf with 
		    | `ESC             q -> grab (acc ^ String.make 1 q.[1]) 
		    | `PRE_CLOSE         
		    | `H_CLOSE         _ 
		    | `OL_CLOSE          
		    | `UL_CLOSE         
		    | `QUOTE_CLOSE      
		    | `PRE_OPEN         
		    | `H_OPEN          _
		    | `OL_OPEN          
		    | `UL_OPEN          
		    | `QUOTE_OPEN       
		    | `PAR              
		    | `EOF              
		    | `CODE_OPEN         
		    | `CODE_CLOSE        
		    | `INLINE_END      _ -> acc
		    | `URL_INLINE      s
		    | `ITALICS         s 
		    | `BOLD            s 
		    | `START_LINK      s
		    | `STR             s 
		    | `ENT             s 
		    | `START_IMAGE     s -> grab (acc ^ s)
		    | `HTML            s -> grab (acc ^ "&lt;")
		    | `AMP             s -> grab (acc ^ "&amp;") 			  
		    | `QUOTE           s -> if s = q then acc else grab (acc ^ s) 
		in
		let rec skip () = 
		  match token lexbuf with 
		    | `ITALICS         _ 
		    | `BOLD            _ 
		    | `START_LINK      _
		    | `STR             _ 
		    | `ENT             _ 
		    | `HTML            _ 
		    | `START_IMAGE     _
		    | `QUOTE           _ 
		    | `URL_INLINE      _
		    | `AMP             _
		    | `CODE_OPEN         
		    | `CODE_CLOSE        
		    | `ESC             _ -> skip () 
		    | `PRE_CLOSE         
		    | `H_CLOSE         _ 
		    | `OL_CLOSE          
		    | `UL_CLOSE         
		    | `QUOTE_CLOSE      
		    | `PRE_OPEN         
		    | `H_OPEN          _
		    | `OL_OPEN          
		    | `UL_OPEN          
		    | `QUOTE_OPEN       
		    | `PAR              
		    | `EOF              
		    | `INLINE_END      _ -> ()
		in
		let title = grab "" in 
		( skip () ; acc, Some title) 		  
	  in

	  let rec img acc = 
	    match token lexbuf with 
	      | `ESC             q -> img (acc ^ String.make 1 q.[1]) 
	      | `PRE_CLOSE         
	      | `H_CLOSE         _ 
	      | `OL_CLOSE          
	      | `UL_CLOSE         
	      | `QUOTE_CLOSE      
	      | `PRE_OPEN         
	      | `H_OPEN          _
	      | `OL_OPEN          
	      | `UL_OPEN          
	      | `QUOTE_OPEN       
	      | `PAR              
	      | `CODE_OPEN         
	      | `CODE_CLOSE        
	      | `EOF               -> None
	      | `INLINE_END      s 
	      | `ITALICS         s 
	      | `BOLD            s 
	      | `START_LINK      s
	      | `STR             s 
	      | `QUOTE           s
	      | `ENT             s 
	      | `START_IMAGE     s -> img (acc ^ s)
	      | `HTML            s -> img (acc ^ "&lt;")
	      | `AMP             s -> img (acc ^ "&amp;") 
	      | `URL_INLINE      _ -> let url, title = url "" in
				      Some (BatString.trim url, acc, title)					
	  in

	  match img "" with Some img -> `IMG img | None -> `STR ""	    
	end
	| `PRE_CLOSE         -> flush `PRE_CLOSE 
	| `H_CLOSE         n -> flush (`H_CLOSE n) 
	| `OL_CLOSE          -> flush `OL_CLOSE 
	| `UL_CLOSE          -> flush `UL_CLOSE 
	| `QUOTE_CLOSE       -> flush `QUOTE_CLOSE 
	| `PRE_OPEN          -> flush `PRE_OPEN
	| `H_OPEN          n -> flush (`H_OPEN n) 
	| `OL_OPEN           -> flush `OL_OPEN 
	| `UL_OPEN           -> flush `UL_OPEN 
	| `QUOTE_OPEN        -> flush `QUOTE_OPEN 
	| `PAR               -> flush `PAR
	| `EOF               -> flush `EOF
	| `ITALICS         s -> 
	  let close = BatList.index_of (`I s) !stack in
	  begin match close with 
	    | None   -> ( stack := !stack @ [`I s] ; `I_OPEN ) 
	    | Some i -> let keep, rem = BatList.split_at i !stack in
			let rem = BatList.drop 1 rem in 
			( stack := keep ; memory := List.rev_map do_close rem ; `I_CLOSE ) 
	  end
	| `BOLD            s -> 
	  let close = BatList.index_of (`B s) !stack in
	  begin match close with 
	    | None   -> ( stack := !stack @ [`B s] ; `B_OPEN ) 
	    | Some i -> let keep, rem = BatList.split_at i !stack in
			let rem = BatList.drop 1 rem in 
			( stack := keep ; memory := List.rev_map do_close rem ; `B_CLOSE ) 
	  end
	| `START_LINK      _ -> (stack := !stack @ [`A] ; `A_OPEN) 
	| `URL_INLINE      s -> 
	  let close = BatList.index_of (`A) !stack in
	  begin match close with 
	    | None   -> `STR s
	    | Some i -> 
	      let keep, rem = BatList.split_at i !stack in
	      let rem = BatList.drop 1 rem in 	      	      
	      let rec grab acc = 
		match token lexbuf with 
		  | `ESC             q -> grab (acc ^ String.make 1 q.[1]) 
		  | `PRE_CLOSE         
		  | `H_CLOSE         _ 
		  | `OL_CLOSE          
		  | `UL_CLOSE         
		  | `QUOTE_CLOSE      
		  | `PRE_OPEN         
		  | `H_OPEN          _
		  | `OL_OPEN          
		  | `UL_OPEN          
		  | `QUOTE_OPEN       
		  | `PAR              
		  | `EOF              
		  | `CODE_OPEN         
		  | `CODE_CLOSE        
		  | `INLINE_END      _ -> BatString.trim acc, None
		  | `URL_INLINE      s
		  | `ITALICS         s 
		  | `BOLD            s 
		  | `START_LINK      s
		  | `STR             s 
		  | `ENT             s 
		  | `START_IMAGE     s -> grab (acc ^ s)
		  | `HTML            s -> grab (acc ^ "&lt;")
		  | `AMP             s -> grab (acc ^ "&amp;") 
		  | `QUOTE           q -> 
		    let rec grab acc = 
		      match token lexbuf with 
			| `ESC             q -> grab (acc ^ String.make 1 q.[1]) 
			| `PRE_CLOSE         
			| `H_CLOSE         _ 
			| `OL_CLOSE          
			| `UL_CLOSE         
			| `CODE_OPEN         
			| `CODE_CLOSE        
			| `QUOTE_CLOSE      
			| `PRE_OPEN         
			| `H_OPEN          _
			| `OL_OPEN          
			| `UL_OPEN          
			| `QUOTE_OPEN       
			| `PAR              
			| `EOF              
			| `INLINE_END      _ -> acc
			| `URL_INLINE      s
			| `ITALICS         s 
			| `BOLD            s 
			| `START_LINK      s
			| `STR             s 
			| `ENT             s 
			| `START_IMAGE     s -> grab (acc ^ s)
			| `HTML            s -> grab (acc ^ "&lt;")
			| `AMP             s -> grab (acc ^ "&amp;") 			  
			| `QUOTE           s -> if s = q then acc else grab (acc ^ s) 
		    in
		    let rec skip () = 
		      match token lexbuf with 
			| `ITALICS         _ 
			| `BOLD            _ 
			| `START_LINK      _
			| `STR             _ 
			| `ENT             _ 
			| `HTML            _ 
			| `START_IMAGE     _
			| `QUOTE           _ 
			| `URL_INLINE      _
			| `AMP             _
			| `ESC             _ -> skip () 			  
			| `CODE_OPEN         
			| `CODE_CLOSE        
			| `PRE_CLOSE         
			| `H_CLOSE         _ 
			| `OL_CLOSE          
			| `UL_CLOSE         
			| `QUOTE_CLOSE      
			| `PRE_OPEN         
			| `H_OPEN          _
			| `OL_OPEN          
			| `UL_OPEN          
			| `QUOTE_OPEN       
			| `PAR              
			| `EOF              
			| `INLINE_END      _ -> ()
		    in
		    let title = grab "" in 
		    skip () ; BatString.trim acc, Some title
	      in
	      
	      begin
		stack := keep ;
		memory := List.rev_map do_close rem ; 
		`A_CLOSE_REAL (grab "")
	      end
	    
	end
	  
  in
  
  fun () -> ( token lexbuf : final ) 

type inline = 
  [ `CODE of string 
  | `SUB  of inline list
  | `B    of inline list
  | `I    of inline list
  | `A    of (string * string option * inline list) 
  | `IMG  of string * string * string option 
  | `STR  of string   
  ]

type tree = 
  [ `P   of inline list
  | `R   of inline list
  | `UL  of tree list list
  | `OL  of tree list list
  | `PRE of string
  | `IMG of string * string * string option
  | `BQ  of tree list
  | `H   of int * inline list
  ]

let parse lexbuf = 
  let get = stream OhmMarkdown_Token.token lexbuf in 
  let rec root raw = function
    | `PAR         -> root false (get ())
    | `UL_OPEN     -> let t, n = ul `UL_OPEN in 
		      let l, n = root raw n in
		      `UL (list_clean t) :: l, n
    | `OL_OPEN     -> let t, n = ol `OL_OPEN in
		      let l, n = root raw n in
		      `OL (list_clean t) :: l, n
    | `QUOTE_OPEN  -> let t, n = quote (get ()) in
		      let l, n = root raw n in
		      `BQ (bq_clean t) :: l, n
    | `H_OPEN i    -> let t, n = h i (get ()) in
		      let l, n = root raw n in
		      `H (i,t) :: l, n
    | `PRE_OPEN    -> let t, n = pre (get ()) in
		      let l, n = root raw n in
		      `PRE t :: l, n 
    | `IMG x       -> let l, n = root raw (get ()) in 
		      `IMG x :: l, n
    | ( `UL_CLOSE    
      | `OL_CLOSE    
      | `QUOTE_CLOSE 
      | `H_CLOSE _   
      | `PRE_CLOSE   
      | `CODE_CLOSE
      | `A_CLOSE
      | `A_CLOSE_REAL _
      | `B_CLOSE
      | `I_CLOSE
      | `EOF 
      ) as next    -> [], next
    | next         -> let inline, n = inline next in
		      let t = if raw then `R inline else `P inline in
		      let l, n = root raw n in
		      t :: l, n
  and pre = function
    | `PRE_CLOSE -> "", (get ()) 
    | `STR s     -> let t, n = pre (get ()) in
		    s ^ t, n
    | _          -> pre (get ())
  and quote next = 
    let l, n = root true next in
    l, (if n = `QUOTE_CLOSE then get () else n) 
  and h i next = 
    let inline, n = inline next in
    inline, (if n = `H_CLOSE i then get () else n)
  and ul next = 
    match next with 
    | `UL_OPEN -> let t, n = root true (get ()) in
		  if n = `UL_CLOSE then 
		    let l, n = ul (get ()) in
		    t :: l, n
		  else
		    [t], n
    | next -> [], next
  and ol next = 
    match next with 
    | `OL_OPEN -> let t, n = root true (get ()) in
		  if n = `OL_CLOSE then 
		    let l, n = ol (get ()) in
		    t :: l, n
		  else
		    [t], n
    | next -> [], next
  and inline next = 
    match next with  
    | `B_OPEN -> let t, n = b (get ()) in
		 let l, n = inline n in
		 t :: l, n
    | `A_OPEN -> let t, n = a (get ()) in
		 let l, n = inline n in
		 t :: l, n
    | `I_OPEN -> let t, n = i (get ()) in
		 let l, n = inline n in
		 t :: l, n
    | `CODE_OPEN -> let t, n = code (get ()) in
		    let l, n = inline n in
		    `CODE t :: l, n
    | `STR  s
    | `ENT  s
    | `HTML s -> let l, n = inline (get ()) in
		 `STR s :: l, n
    | `IMG  x -> let l, n = inline (get ()) in
		 `IMG x :: l, n
    | next -> [], next
  and b next = 
    let l, n = inline next in
    `B l, (if n = `B_CLOSE then get () else n)
  and i next = 
    let l, n = inline next in
    `I l, (if n = `I_CLOSE then get () else n)
  and code next = 
    match next with 
      | `CODE_CLOSE -> "", get ()
      | `STR s      -> let t, n = code (get ()) in
		       s ^ t, n
      | _           -> code (get ())
  and a next = 
    let l, n = inline next in
    match n with 
      | `A_CLOSE -> `SUB l, get ()
      | `A_CLOSE_REAL (url,title) -> `A (url,title,l), get ()
      | n -> `SUB l, n
  and list_nonraw_exists list = 
    List.exists (List.exists (function `P _ -> true | _ -> false)) list 
  and list_paragraph_wrap list = 
    List.map (List.map (function `R l -> `P l | o -> o)) list
  and list_clean list = 
    if list_nonraw_exists list then list_paragraph_wrap list else list
  and bq_nonraw_exists list = 
    List.exists (function `P _ -> true | _ -> false) list
  and bq_paragraph_wrap list = 
    List.map (function `R l -> `P l | o -> o) list
  and bq_clean (list : tree list) = 
    if bq_nonraw_exists list then bq_paragraph_wrap list else list
  in
    
  let tree, _ = root true (get ()) in
  bq_clean tree

let to_html trees = 
  let rec inline = function 
    | `CODE  s  -> Html.str s 
    | `SUB   l  -> Html.concat (List.map inline l) 
    | `B     l  -> Html.concat [ Html.str "<strong>" ;
				 Html.concat (List.map inline l) ;
				 Html.str "</strong>" ]
    | `I     l  -> Html.concat [ Html.str "<em>" ;
				 Html.concat (List.map inline l) ;
				 Html.str "</em>" ]
    | `A(u,t,l) -> Html.concat [ Html.str "<a href=\"" ;
				 Html.str u ;
				 ( match t with 
				   | None -> Html.str "\">"
				   | Some t -> Html.concat [ Html.str "\" title=\"" ;
							     Html.str t ;
							     Html.str "\">" ] ) ;
				 Html.concat (List.map inline l) ;
				 Html.str "</a>" ]
    | `IMG   i  -> img i 
    | `STR   s  -> Html.str s 
  and img (url,alt,title) = 
    Html.concat [ Html.str "<img src=\"" ;
		  Html.str url ;
		  Html.str "\" alt=\"" ;
		  Html.str alt ;
		  ( match title with 
		    | None -> Html.str "\" />"
		    | Some t -> Html.concat [ Html.str "\" title=\"" ;
					      Html.str t ;
					      Html.str "\" />" ] );
		]
  and tree = function 
    | `R l -> Html.concat (List.map inline l)
    | `P l -> Html.concat [ Html.str "<p>" ;
			    Html.concat (List.map inline l) ;
			    Html.str "</p>" ] 
    | `UL l -> Html.concat [ Html.str "<ul>" ;
			     Html.concat (List.map
			       (fun l -> Html.concat [ Html.str "<p>" ;
						       Html.concat (List.map tree l) ;
						       Html.str "</p>" ]) l) ;
			     Html.str "</ul>" ]      
    | `OL l -> Html.concat [ Html.str "<ol>" ;
			     Html.concat (List.map
			       (fun l -> Html.concat [ Html.str "<p>" ;
						       Html.concat (List.map tree l) ;
						       Html.str "</p>" ]) l) ;
			     Html.str "</ol>" ]	
    | `PRE   p -> Html.concat [ Html.str "<pre><code>" ;
				Html.str p ;
				Html.str "</code></pre>" ]
    | `IMG   i  -> img i 
    | `BQ l -> Html.concat [ Html.str "<blockquote>" ;
			     Html.concat (List.map tree l) ;
			     Html.str "</blockquote>" ] 
    | `H (i,l) ->  Html.concat [ Html.str ("<h"^string_of_int i^">" );
				 Html.concat (List.map inline l) ;
				 Html.str ("</h"^string_of_int i^">" ); ] 
  in
  Html.concat (List.map tree trees)
