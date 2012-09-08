{ (* Ohm is © 2012 Victor Nicollet *) }

let unalpha = [ '(' '[' ']' ')' '.' '?' '/' ':' ';' ',' '{' '=' '<' '>' 
		  '!' '-' '+' '*' '~' '&' '|' ]

rule lex hub buf = parse 
| "}>" { () }
| "<{" ([ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' '-' ] + as s) ':'  
    { BatOption.default lex (Hub.get hub s) hub buf lexbuf ; lex hub buf lexbuf }

| ('\'' _ '\'' | '"' ([^ '"' '\\'] | '\\' _) * '"') as s
    { buf # raw "<span class=string>" ;
      buf # esc s ;
      buf # raw "</span>" ;  
      lex hub buf lexbuf }

| ("open" | "module" | "struct" | "in" | "let" | "let!" | "class"
      | "type" | "end" | "begin" | "and" | "rec" | "json"
      | "object" | "sig" | "functor") as s
    { buf # raw "<span class=keyword>" ;
      buf # esc s ;
      buf # raw "</span>" ; 
      lex hub buf lexbuf }

| unalpha + as s 
    { buf # raw "<span class=light>" ;
      buf # esc s ;
      buf # raw "</span>" ;
      lex hub buf lexbuf }

| ("function" | "fun" | "match" | "with" | "ref" | "try" | "new"
      | "lazy" | "as" | "val" | "method" | "mutable" | "inherit"
      | "private" | "for" | "while" | "to" | "done" | "downto" ) as s
    { buf # raw "<span class=key2>" ;
      buf # esc s ;
      buf # raw "</span>" ;  
      lex hub buf lexbuf }

| ("(*" ( [^ '*'] | "*" [^ ')'] ) * "*)") as s  
    { buf # raw "<span class=comment>" ;
      buf # esc s ;
      buf # raw "</span>" ; 
      lex hub buf lexbuf }

| (['A' - 'Z'] ['a' - 'z' '0' - '9' 'A' - 'Z' '_'] * as s)
    { buf # raw "<span class=name>" ;
      buf # esc s ;
      buf # raw "</span>" ;
      lex hub buf lexbuf }

| ([ '~' '?' ] as c) (['a'-'z'] ['a' - 'z' '0' - '9' 'A' - 'Z' '_'] * as s)
    { buf # raw "<span class=light>" ;
      buf # raw (String.make 1 c) ;
      buf # raw "</span><span class=atom>" ;
      buf # esc s ;
      buf # raw "</span>" ;
      lex hub buf lexbuf }    

| [ 'a' - 'z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_'] * as s
    { buf # esc s ;
      lex hub buf lexbuf }

|  '`' (['a' - 'z' 'A' - 'Z'] ['a' - 'z' '0' - '9' 'A' - 'Z' '_'] * as s)
    { buf # raw "<span class=light>`</span><span class=atom>" ;
      buf # esc s ;
      buf # raw "</span>" ;
      lex hub buf lexbuf }

| _ as c { buf # esc (String.make 1 c) ; lex hub buf lexbuf }
| eof { () }

