(* Ohm is © 2011 Victor Nicollet *)

include Ohm.Fmt.FMT with type t = Ohm.Id.t

type session = <
  access_token : string ;
  uid          : t
>

type config  = <
  app_id     : string ;
  api_key    : string ;
  api_secret : string
>

val get_session : config -> < cookie : string -> string option ; .. > -> session option

type details = <
  firstname : string ;
  lastname  : string ;
  email     : string ;
  pic_small : string ;
  pic_large : string ;
  gender    : [`m|`f] option
> ;;

val get : session -> [ `valid of details | `invalid | `not_found ]

