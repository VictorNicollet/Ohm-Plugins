{ (* Ohm is © 2012 Victor Nicollet *) }

rule lex hub buf = parse 
| "}>" { () }
| "<{" ([ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' '-' ] + as s) ':'  
    { BatOption.default lex (Hub.get hub s) hub buf lexbuf ; lex hub buf lexbuf }

| '<' [^ '{'] as s { buf # raw s ; lex hub buf lexbuf }
| '}' [^ '>'] as s { buf # raw s ; lex hub buf lexbuf }
| [^ '}' '<' ] * as s { buf # raw s ; lex hub buf lexbuf }
| [ '<' '}' ] as c { buf # raw (String.make 1 c) ; lex hub buf lexbuf }
| eof { () }
