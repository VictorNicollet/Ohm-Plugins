{ (* Ohm is © 2012 Victor Nicollet *) }

rule lex hub buf = parse 
| ([^ '}'] | '}' [^ '>']) + as s { buf # url (BatString.trim s) ; lex hub buf lexbuf }
| "}>" { () }
| eof { () }
