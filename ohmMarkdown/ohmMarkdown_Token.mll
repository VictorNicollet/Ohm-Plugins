{

}

let d  = ['0' - '9']
let h  = ['0' - '9' 'a' - 'f' 'A' - 'F' ]
let sp = [ ' ' '\t' ]
let rest = [^ '*' '_' '-' '!' '[' ']' ')' '\n' ' ' '\t' '-' '#' '`' '\\' '\"' '<' '&' '\'' '>' '+'] +
let an = ['a' - 'z' '0' - '9' 'A' - 'Z' ]

rule token = parse
  | ("*"  | "_" )  as c { `ITALICS (String.make 1 c) }
  | ("**" | "__")  as s { `BOLD s }
  | "!["           as s { `START_IMAGE s }
  | "["            as c { `START_LINK (String.make 1 c) }
  | "]("           as s { `URL_INLINE s }
  | ")"            as c { `INLINE_END (String.make 1 c) }
  | ("\"" | "'")   as c { `QUOTE (String.make 1 c) }
  | ('\\' _)       as s { `ESC s }
  | (d+ '.' sp)    as s { `ORDERED_LIST s }
  | "\n"           as c { `NEWLINE (String.make 1 c) }
  | '#'+           as s { `HASHES s }
  | [ ' ' '\t' ]+  as s { `WHITESPACE s }
  | ">"            as c { `BLOCKQUOTE (String.make 1 c) }
  | "`"            as c { `CODE (String.make 1 c) }
  | "``"           as s { `CODE_ESC s }
  | ['*''+''-'] sp as s { `UNORDERED_LIST s }
  | "<"            as c { `HTML (String.make 1 c) }
  | '&'            as c { `AMP (String.make 1 c) }
  | ("&" an+ ';')  as s { `ENT s }
  | ("&#" d+ ';')  as s { `ENT s }
  | ("&#x" h+';')  as s { `ENT s }
  | rest           as s { `STR s }
  | _              as c { `STR (String.make 1 c) }
  | eof                 { `EOF   }

{
}

