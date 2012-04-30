(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open BatPervasives
open Universal

module Config = Fmt.Make(struct
  type json t = < 
    api_username : string ;
    api_password : string ;
    signature    : string 
  >
end)

module Status = Fmt.Make(struct
  type json t = 
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
end)

let summary = function
  | None 
  | Some (     `None
	     | `CanceledReversal
	     | `Denied
	     | `Expired
	     | `PartiallyRefunded
	     | `Failed 
	     | `Refunded 
	     | `Reversed _ 
	     | `Voided) -> `No

  | Some (     `Completed
	     | `Processed) -> `Yes

  | Some (     `InProgress
	     | `Pending _
	     | `CompletedFundsHeld _) -> `Pending

module Error = Fmt.Make(struct
  type json t = <
    short : string ;
    long : string ;
    code : string ;
    severity : string 
  >
end)

module Diff = Fmt.Make(struct
  type json t = 
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
	business : string option 
      > 
    | `transaction of string 
    | `status of Status.t
    | `error of Error.t
    ]
end)	   
    
module type CONFIG = sig
    
  module MainDB : CouchDB.CONFIG
  module VersionDB : CouchDB.CONFIG
  module Id : CouchDB.ID
  module Reason : Fmt.FMT
    
  val testing : bool

end
  
module Make = functor(C:CONFIG) -> struct

  (* Type and module definitions *)


  module Payment = struct
    module T = struct
      module Float = Fmt.Float
      module Reason = C.Reason
      type json t = {
	amount   : int ;
	tax      : int ;
	currency : [ `EUR ] ;
	invoice  : string ;
	reason   : Reason.t ;
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
  	  business : string option 
        > option ;
	transaction : string option ; 
	status : < time : Float.t ; value : Status.t > option ; 
	error : Error.t option 
      }
    end
    include T
    include Fmt.Extend(T)
  end
  
  module VersionedConfig = struct
    let name = "paypal"
    module DataDB = C.MainDB
    module Id = C.Id
    module VersionDB = C.VersionDB
    module Data = Payment
    module Diff = Diff
    module VersionData = Fmt.Unit
    module ReflectedData = Fmt.Unit
            
    let apply = function 
      | `payer payer -> return (fun id time data ->
	return { data with Data.payer = Some payer }
      )
      | `transaction t -> return (fun id time data ->
	return { data with Data.transaction = Some t }
      )
      | `status status -> return (fun id time data ->
	return { data with 
	  Data.status = Some (object
	    method time  = time
	    method value = status
	  end) ;
	}
      )
      | `error error -> return (fun id time data ->
	return { data with Data.error = Some error }
      )

    let reflect _ _ = return ()
  end

  module DB = OhmCouchVersioned.Make(VersionedConfig) 

  module Signals = struct
    let update_call, update = Sig.make (Run.list_iter identity)
    let _ = Sig.listen DB.Signals.update (fun t -> update_call (DB.id t, DB.current t))
  end

  let get id = 
    DB.get id |> Run.map (BatOption.map DB.current)

  (* Sending PayPal requests *)

  let api_url = 
    if C.testing
    then "https://api-3t.sandbox.paypal.com/nvp"
    else "https://api-3t.paypal.com/nvp"

  let paypal_url = 
    if C.testing 
    then "https://www.sandbox.paypal.com/webscr?cmd=_express-checkout&token="
    else "https://www.paypal.com/webscr?cmd=_express-checkout&token="

  let version = "76.0"

  let nvp args = 

    let postfields = Netencoding.Url.mk_url_encoded_parameters args in

    Util.log "-> PAYPAL: [%s] %s" api_url postfields ;

    try 
      let response = 
	let curl = new Curl.handle in
	let buffer = Buffer.create 1763 in
	
	curl # set_sslversion 3 ;
	curl # set_url api_url ;
	curl # set_post true ; 
	curl # set_postfields postfields ;
	curl # set_postfieldsize (String.length postfields) ;
	curl # set_writefunction (fun x -> Buffer.add_string buffer x ; String.length x) ;
	curl # perform ;
	curl # cleanup ;
	
	Buffer.contents buffer
      in
      
      try let () = Util.log "<- PAYPAL: %s" response in
	  Some (Netencoding.Url.dest_url_encoded_parameters response)
      with _ -> let () = Util.log "PAYPAL: ERROR PARSING RESPONSE!"in
		None    

    with Curl.CurlException (reason, code, str) ->
      let () = Util.log "PAYPAL : cURL ERROR! `%s`" str in
      None

  let args config methodName more = 
    ( "USER", config # api_username ) ::
      ( "PWD", config # api_password ) ::
      ( "SIGNATURE", config # signature ) ::
      ( "VERSION", version ) ::
      ( "METHOD", methodName ) :: more

  let locale_code = function 
    | `AU -> "AU"
    | `AT -> "AT"
    | `BE -> "BE"
    | `CA -> "CA"
    | `CH -> "CH"
    | `CN -> "CN"
    | `DE -> "DE"
    | `ES -> "ES"
    | `GB -> "GB"
    | `FR -> "FR"
    | `IT -> "IT"
    | `NL -> "NL"
    | `PL -> "PL"
    | `US -> "US"

  let currency_code = function
    | `EUR -> "EUR"

  let money amount = Printf.sprintf "%d.%02d" (amount / 100) (amount mod 100) 

  let parse_error arg = 

    [ `error (object
      method short    = arg "L_SHORTMESSAGE0"
      method long     = arg "L_LONGMESSAGE0"
      method code     = arg "L_ERRORCODE0"
      method severity = arg "L_SEVERITYCODE0"
    end) ]

  let is_failed arg = 
    arg "ACK" = "Failure"

  let setExpressCheckout ~id ~amount ~tax ~invoice ~returnurl ~cancelurl ~locale ~config ~reason = 

    (* Construct the Express Checkout *)

    let item = amount - tax in

    let currency = `EUR in 

    let args = args config "SetExpressCheckout" [
      "RETURNURL", returnurl ;
      "CANCELURL", cancelurl ;
      "REQCONFIRMSHIPPING", "0" ;
      "NOSHIPPING", "1" ;
      "LOCALECODE", locale_code locale ;
      "SOLUTIONTYPE", "Sole" ;
      "LANDINGPAGE", "Billing" ;
      "PAYMENTREQUEST_0_AMT", money amount ;
      "PAYMENTREQUEST_0_CURRENCYCODE", currency_code currency ;
      "PAYMENTREQUEST_0_ITEMAMT", money item ;
      "PAYMENTREQUEST_0_TAXAMT", money tax ;
      "PAYMENTREQUEST_0_INVNUM", invoice ;
      "PAYMENTREQUEST_0_PAYMENTACTION", "Sale" ;
      "PAYMENTREQUEST_0_PAYMENTREQUESTID", Id.str (C.Id.to_id id) ;
    ] in

    let token_opt = 
      match nvp args with None -> None | Some response ->
	try Some (List.assoc "TOKEN" response) with Not_found -> None
    in

    (* Paypal replied with a token, so let's use it. *)

    match token_opt with None -> return None | Some token ->      

      let init = Payment.({
	amount      = amount ;
	tax         = tax ;
	currency    = currency ;
	invoice     = invoice ;
	reason      = reason ;
	token       = token ;
	payer       = None ;
	transaction = None ;
	status      = None ;
	error       = None
      }) in

      let! _ = ohm $ DB.create ~id ~init ~diffs:[] ~info:() () in
      return (Some (paypal_url ^ token))

  let getExpressCheckoutDetails id ~config = 
    let! payment = ohm_req_or (return false) $ get id in
    
    let token = payment.Payment.token in
    
    let args = args config "GetExpressCheckoutDetails" [ "TOKEN", token ] in
    
    match nvp args with None -> return false | Some response -> 
      
      let arg_opt n = try Some (List.assoc n response) with Not_found -> None in
      let arg n = arg_opt n |> BatOption.default "" in 
      
      let success, diffs = 
	
	if is_failed arg then
	  false, parse_error arg
	else 
	  
	  let payer = object
	    method id     = arg_opt "PAYERID"
	    method status = 
	      if arg_opt "PAYERSTATUS" = Some "verified" then `verified else `unverified
	    method phone      = arg_opt "PHONENUM"
	    method email      = arg_opt "EMAIL"
	    method salutation = arg_opt "SALUTATION"
	    method firstname  = arg_opt "FIRSTNAME"
	    method lastname   = arg_opt "LASTNAME"
	    method middlename = arg_opt "MIDDLENAME"
	    method suffix     = arg_opt "SUFFIX"
	    method country    = arg_opt "COUNTRYCODE"
	    method business   = arg_opt "BUSINESS"
	  end in
	  
	  payer # id <> None, [`payer payer]
      in
      
      let! _ = ohm $ DB.update ~id ~diffs ~info:() () in
      return success
    

  let doExpressCheckoutPayment id ~config = 
    let! payment = ohm_req_or (return None) $ get id in

    let payer_opt = payment.Payment.payer |> BatOption.bind (fun payer -> payer # id) in
    
    match payer_opt with None -> return None | Some payerid -> 
      
      let args = args config "DoExpressCheckoutPayment" Payment.([
	"TOKEN", payment.token ;
	"PAYERID", payerid ;
	"PAYMENTREQUEST_0_AMT", money payment.amount ;
	"PAYMENTREQUEST_0_CURRENCYCODE", currency_code payment.currency ;
	"PAYMENTREQUEST_0_TAXAMT", money payment.tax ;
	(* "PAYMENTREQUEST_0_NOTIFYURL", notifyurl ; *)
	"PAYMENTREQUEST_0_ITEMAMT", money (payment.amount - payment.tax) ;	
      ]) in
      
      match nvp args with None -> return None | Some response ->
	
	let result, diffs = 	 
	  
	  let arg_opt n = try Some (List.assoc n response) with Not_found -> None in
	  let arg n = arg_opt n |> BatOption.default "" in

	  if is_failed arg then begin 

	    None, parse_error arg

	  end

	  else begin 

	    let transaction = match arg_opt "PAYMENTINFO_0_TRANSACTIONID" with 
	      | None -> Util.log "PAYPAL: Transaction ID missing!" ; []
	      | Some tid -> [ `transaction tid ]
	    in
	    	    
	    let paymentstatus = match arg "PAYMENTINFO_0_PAYMENTSTATUS" with 
	      | "Canceled-Reversal" -> `CanceledReversal
	      | "Completed" -> `Completed
	      | "Denied" -> `Denied
	      | "Expired" -> `Expired
	      | "Failed" -> `Failed
	      | "In-Progress" -> `InProgress
	      | "Partially-Refunded" -> `PartiallyRefunded
	      | "Pending" -> `Pending begin
		match arg "PAYMENTINFO_0_PENDINGREASON" with
		  | "none" -> `none
		  | "address" -> `address
		  | "authorization" -> `authorization
		  | "echeck" -> `echeck
		  | "intl" -> `intl
		  | "multi-currency" -> `multiCurrency
		  | "order" -> `order
		  | "paymentreview" -> `paymentreview
		  | "unilateral" -> `unilateral
		  | "verify" -> `verify
		  | _ -> `other
	      end
	      | "Refunded" -> `Refunded
	      | "Reversed" -> `Reversed begin
		match arg "PAYMENTINFO_0_REASONCODE" with 
		  | "none" -> `none
		  | "chargeback" -> `chargeback
		  | "guarantee" -> `guarantee
		  | "buyer-complaint" -> `buyerComplaint
		  | "refund" -> `refund
		  | _ -> `other
	      end
	      | "Processed" -> `Processed
	      | "Voided" -> `Voided
	      | "Completed-Funds-Held" -> `CompletedFundsHeld begin
		match arg "PAYMENTINFO_0_HOLDDECISION" with
		  | "newsellerpaymenthold" -> `newSeller
		  | _ -> `none
	      end
	      | _ -> `None
	    in
	    
	    Some paymentstatus, transaction @ [ `status paymentstatus ]

	  end 
	in

	let! _ = ohm $ DB.update ~id ~diffs ~info:() () in
	return result
	
end
