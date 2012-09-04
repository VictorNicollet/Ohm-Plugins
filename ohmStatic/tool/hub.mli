(* Ohm is © 2012 Victor Nicollet *)

type t

type lexer = t -> Buf.t -> Lexing.lexbuf -> unit

val empty : t
val make  : (string -> lexer option) -> t
val of_assoc : (string * lexer) list -> t
val (+) : t -> t -> t

val get : t -> string -> lexer option 

val descend : t -> lexer -> string -> Buf.t -> Lexing.lexbuf -> unit
