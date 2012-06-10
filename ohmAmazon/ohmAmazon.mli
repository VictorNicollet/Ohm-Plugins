(* Ohm is © 2012 Victor Nicollet *)

module type ACCOUNT = sig
  val id  : string
  val key : string
end

module type S3 = sig

  type upload 
      
  val upload : 
       ?acl:[`Private | `PublicRead | `PublicReadWrite | `AuthenticatedRead | `BucketOwnerRead
	    | `BucketOwnerFullControl ] 
    -> ?size:(int*int)
    -> ?life:float
    -> ?filename:string
    ->  bucket:string
    ->  key:string
    ->  redirect:string
    -> unit -> upload
	   
  val upload_form : 
       upload
    -> (Ohm.Html.writer -> ('ctx,Ohm.Html.writer) Ohm.Run.t)
    -> ('ctx,Ohm.Html.writer) Ohm.Run.t

  val upload_url : upload -> string * ((string * string) list) 

  type uploaded = { name : string ; size : int }

  val find_upload : bucket:string -> prefix:string -> uploaded option

  val download : bucket:string -> key:string -> (string -> 'a option) -> 'a option

  val delete : bucket:string -> key:string -> bool

  val publish : bucket:string -> key:string -> file:string -> bool

  val qsa_auth : bucket:string -> key:string -> duration:int -> string

end

module S3 : functor(Account:ACCOUNT) -> S3
