(* Ohm is © 2011 Victor Nicollet *)

module Config : Ohm.Fmt.FMT with type t = <
  api_username : string ;
  api_password : string ;
  signature    : string 
>

module Status : Ohm.Fmt.FMT with type t = 
  [ `None
  | `CanceledReversal
  | `Completed
  | `Denied
  | `Expired
  | `Failed
  | `InProgress
  | `PartiallyRefunded
  | `Pending of [ `none
		| `address
		| `authorization
		| `echeck
		| `intl
		| `multiCurrency
		| `order
		| `paymentreview
		| `unilateral
		| `verify
		| `other ]
  | `Refunded
  | `Reversed of [ `none
		 | `chargeback
		 | `guarantee
		 | `buyerComplaint
		 | `refund
		 | `other ]
  | `Processed 
  | `Voided
  | `CompletedFundsHeld of [ `newSeller
			   | `none ]
  ] 

val summary : Status.t option -> [> `Yes|`No|`Pending]

module Error : Ohm.Fmt.FMT with type t = <
  short : string ;
  long : string ;
  code : string ;
  severity : string 
>

module type CONFIG = sig
    
  module MainDB : Ohm.CouchDB.CONFIG
  module VersionDB : Ohm.CouchDB.CONFIG
  module Id : Ohm.CouchDB.ID
  module Reason : Ohm.Fmt.FMT
    
  val testing : bool

end

module Diff : sig 

  type t = 
    [ `payer of <
        id : string option ;
        status : [ `verified | `unverified ] ;
	email : string option ;
	phone : string option ; 
	salutation : string option ;
	firstname : string option ;
	middlename : string option ;
	lastname : string option ;
	suffix : string option ; 
	country : string option ;
	business : string option ; 
      > 
    | `transaction of string 
    | `status of Status.t
    | `error of Error.t
    ]	  

end 

module Make : functor (C:CONFIG) -> sig    

  module Payment : sig

    type t = {
      amount   : int ;
      tax      : int ;
      currency : [ `EUR ] ;
      invoice  : string ;
      reason   : C.Reason.t ;
      token    : string ;
      payer : <
        id : string option ;
        status : [ `verified | `unverified ] ;
	email : string option ;
	phone : string option ; 
	salutation : string option ;
	firstname : string option ;
	middlename : string option ;
	lastname : string option ;
	suffix : string option ; 
	country : string option ;
	business : string option ; 
      > option ;
      transaction : string option ; 
      status : < time : float ; value : Status.t > option ; 
      error : Error.t option 
    }

  end

  module Signals : sig
    val update : (C.Id.t * Payment.t, (Ohm.CouchDB.ctx,unit) Ohm.Run.t) Ohm.Sig.channel
  end
  
  val get : C.Id.t -> (#Ohm.CouchDB.ctx,Payment.t option) Ohm.Run.t 

  val setExpressCheckout : 
       id:C.Id.t
    -> amount:int 
    -> tax:int
    -> invoice:string
    -> returnurl:string
    -> cancelurl:string
    -> locale:[`AU|`AT|`BE|`CA|`CH|`CN|`DE|`ES|`GB|`FR|`IT|`NL|`PL|`US]
    -> config:Config.t
    -> reason:C.Reason.t
    -> (#Ohm.CouchDB.ctx,string option) Ohm.Run.t

  val getExpressCheckoutDetails :
       C.Id.t
    -> config:Config.t
    -> (#Ohm.CouchDB.ctx,bool) Ohm.Run.t

  val doExpressCheckoutPayment  :
       C.Id.t 
    -> config:Config.t 
    -> (#Ohm.CouchDB.ctx,Status.t option) Ohm.Run.t
   
end
