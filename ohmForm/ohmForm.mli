(* Ohm is © 2012 Victor Nicollet *)

(** The type of a field identifier. These are provided to the deserialization functions 
    defined in the template, and can be used to add errors to those fields in the 
    form response.
*)
type field

(** A CSS selector used by the javascript code to select a piece of the generated HTML to
    bind behavior to it. Most of the rendering of Joy forms involves providing an HTML 
    renderer and a set of selectors to pick the relevant items from within it. Those 
    selectors are relative to the HTML's root, not the root of the document.
*)
type selector = string

(** A selector equal to [""], which the JS library interprets as "current context". *)
val here : selector

(** Describes the structure of a form. Forms can be initialized from an application-provided
    type called the seed (the second type parameter) and can be parsed into an 
    application-provided type called the result (the third type parameter), where both
    parsing and seeding happen in a given context (the first type parameter)
*)
type ('ctx,'seed,'result) template

(** Applies a map function to the seed part of the template *)
val seed_map : ('a -> 'b) -> ('ctx,'b,'result) template -> ('ctx,'a,'result) template

(** Applies a map function t the result part of the template *)
val result_map : ('a -> 'b) -> ('ctx,'seed,'a) template -> ('ctx,'seed,'b) template

(** A template that always returns the same result. *)
val constant : 'result -> ('ctx,'seed,'result) template

(** Specify the HTML context for this object. *)
val wrap : 
     selector
  -> ('ctx,Ohm.Html.writer) Ohm.Run.t
  -> ('ctx,'seed,'result) template
  -> ('ctx,'seed,'result) template

(** A single string field.
    
    Rendering involves several HTML contexts: the parent context is
    the HTML of the parent Joy template (or the Joy root, if none), the
    field context is the context of the input element's HTML (if missing, use the
    full context) and the label context is the context of the label element's HTML 
    (if missing, use the full context).
    
    Missing HTML fields have no effect. If the label parameter is not provided, the
    label HTML is not rendered.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
*)
val string : 
  field:selector ->
 ?label_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?field_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?error_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?label:(selector * ('ctx,string) Ohm.Run.t) ->
 ?error:selector ->
  ('seed -> ('ctx,string) Ohm.Run.t) ->
  (field -> string -> ('ctx,('result,field * string) BatStd.result) Ohm.Run.t) ->
  ('ctx,'seed,'result) template

(** A JSON selector/autocomplete field.

    Rules for rendering are (almost) the same as for the normal string selector.
    
    Instead of reading and writing string, the selector relies on a JSON formatter to
    read and write values of an arbitrary type. 

    The list of available values is provided by one or two sources. A static source
    is a list of values with their labels and (optional) HTML. A dynamic source
    is an URL which, when provided with a prefix (sent under the GET parameter "term"),
    returns an appropriately formatted list of values-labels-(optional)-HTML.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
    @param format The JSON formatter to be used.
    @param static The static source.
    @param dynamic The dynamic source.
*)
val select : 
  field:selector ->
 ?label_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?field_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?error_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?label:(selector * ('ctx,string) Ohm.Run.t)  ->
 ?error:selector ->
  format:'data Ohm.Fmt.fmt ->
  source:[`Static of ('data * string * ('ctx,Ohm.Html.writer) Ohm.Run.t option) list 
	 |`Dynamic of string
	 |`Both of ('data * string * ('ctx,Ohm.Html.writer) Ohm.Run.t option) list * string] ->
  ('seed -> ('ctx,'data option) Ohm.Run.t) ->
  (field -> 'data option  -> ('ctx,('result,field * string) BatStd.result) Ohm.Run.t) ->
  ('ctx,'seed,'result) template


(** A JSON single/multiple-select field.

    Rules for rendering are (almost) the same as for the normal string selector.
    
    Instead of reading and writing string, the selector relies on a JSON formatter to
    read and write values of an arbitrary type. 

    The list of available values is provided by one or two sources. A static source
    is a list of values with their labels and (optional) HTML. A dynamic source
    is an URL which, when provided with a prefix (sent under the GET parameter "term"),
    returns an appropriately formatted list of values-labels-(optional)-HTML.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
    @param format The JSON formatter to be used.
*)
val choice : 
  field:selector ->
 ?label_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?field_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?error_html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
 ?label:(selector * ('ctx,string) Ohm.Run.t) ->
 ?error:selector ->
  format:'data Ohm.Fmt.fmt ->
  source:('data * ('ctx,Ohm.Html.writer) Ohm.Run.t) list ->
  multiple:bool ->
  ('seed -> ('ctx,'data list) Ohm.Run.t) ->
  (field -> 'data list  -> ('ctx,('result,field * string) BatStd.result) Ohm.Run.t) ->
  ('ctx,'seed,'result) template


(** An array.

    Two HTML contexts are involved: the root context, which contains an ul-type element
    (ul, ol, table, div...) and an item context, which is usually the li-type element
    (li, tr, div...) .    

    @param list A selector for finding the ul-type element.
    @param add A selector for finding the "add" link/button.
    @param item Renders an item.
    @param remove A selector for finding the "remove" link/button.
*)

val array : 
  list:selector ->
  add :selector ->
  item:('ctx,Ohm.Html.writer) Ohm.Run.t ->
  remove:selector ->
  ('ctx,'seed,'result) template ->
  ('ctx,'seed list,'result list) template 

(** An optional value. 

    Two HTML contexts are involved: the root context, which contains an ul-type element
    (ul, ol, table, div...) and an item context, which is usually the li-type element
    (li, tr, div...). The system works exactly like an array, but only one value may be
    added. 

    @param list A selector for finding the ul-type element.
    @param add A selector for finding the "add" link/button.
    @param item Renders an item.
    @param remove A selector for finding the "remove" link/button.
*)

val option : 
  list:selector ->
  add :selector ->
  item:('ctx,Ohm.Html.writer) Ohm.Run.t ->
  remove:selector ->
  ('ctx,'seed,'result) template ->
  ('ctx,'seed option,'result option) template 
    
(** Extracts the search parameter for a JSON selector.

    This merely reads out the "term" GET parameter.
*)
val select_search_param : 
  'data Ohm.Fmt.fmt ->
  < get : string -> string option ; .. > -> 
  [ `Complete of string | `Get of 'data ]

(** Return a formatted list of search results for a JSON selector. *)
val select_return_list : 
  'data Ohm.Fmt.fmt -> 
  ('data * string * ('ctx,Ohm.Html.writer) Ohm.Run.t option) list ->
  ('ctx, (string * Ohm.Json.t) list) Ohm.Run.t

(** A field concatenator.
    
    Appends field A to field B and constructs a new result from their combined results using
    a combinator function. May optionally define a brand new context, otherwise the parent
    rendering context is used by both fields.    
*)
val append : 
  ('a -> 'b -> ('ctx,'c) Ohm.Run.t ) ->
  ('ctx,'seed,'b) template ->
  ('ctx,'seed,'a) template ->
  ('ctx,'seed,'c) template

(** Begin specifying an object.

    This template is provided as a helper to which one can append the fields of an object.
    It returns the result provided upon creation, which is usually a function with labels,
    so that every appended field can use a combinator like [fun f a -> f ~a], making it
    easier to combine all the fields into one object.

    As you might have guessed, this is an alias for [constant]
*)
val begin_object : 'result -> ('ctx,'seed,'result) template

(** Finish specifying an object. 

    This optionally sets some HTML for an object to be rendered into.

    This is nearly an alias for [html], the difference being that the HTML is optional.
*)
val end_object :
  ?html:(selector * ('ctx,Ohm.Html.writer) Ohm.Run.t) ->
  ('ctx,'seed,'result) template ->
  ('ctx,'seed,'result) template

(** Convenience result parsing function. [keep field value] always 
    evaluates to [return (Ok value)]. *)
val keep : field -> 'data -> ('ctx,('data,field * string) BatStd.result) Ohm.Run.t

(** Convenience result parsing function. [keep error field value] either
    evaluates to [return (Ok value)] if value is a non-empty string or to
    [return (Bad (field,error))] otherwise. *)
val required : 
     ('ctx,string) Ohm.Run.t
  -> field
  -> string
  -> ('ctx,(string,field * string) BatStd.result) Ohm.Run.t

(** Convenience result for adding the field to the returned value. This helps
    when post-processing is required (such as when checking two fields together)
    and the field needs to be available to add the error. 
*)
val postpone : 
     (field -> 'data -> ('ctx,('data,field * string) BatStd.result) Ohm.Run.t)
  -> (field -> 'data -> ('ctx,('data * field,field * string) BatStd.result) Ohm.Run.t)

(** Provides a source of data for initializing a form during construction. *)
type 'seed source 

(** Initialize the form with all empty fields. *)
val empty : 'any source 

(** Initialize the form using an application-provided type. This is usually performed when
    displaying a form for the first time, using database-provided data. 
*)
val from_seed : 'seed -> 'seed source

(** Initialize the form using a string sent by the client. This is usually performed when
    parsing the client-sent response. Note that the string contains JSON data serialized
    by the client.
*)
val from_post : string -> 'any source

(** Initialize the form using a JSON value. This function is called by {!from_response} 
    internally, but it may be used when the client response was sent as part of a
    larger JSON value and is already deserialized.
*)
val from_post_json : Ohm.Json.t -> 'any source

(** A form instance, carries some data which may be extracted into the result type. *)
type ('ctx,'result) form

(** Creating a specific form instance. *)
val create : 
     template:('ctx,'seed,'result) template
  -> source:'seed source
  -> ('ctx,'result) form

(** Render the form HTML, outputs both HTML and JavaScript. The URL to which the form must post
    (or any other endpoint) is provided as well.
*)
val render : ('ctx,'result) form -> Ohm.JsCode.Endpoint.t -> ('ctx,Ohm.Html.writer) Ohm.Run.t

(** Compute the server's JSON response. Includes both the form data and any specified errors. *)
val response : ('ctx,'result) form -> ('ctx, (string * Ohm.Json.t) list) Ohm.Run.t

(** Alter the form by adding errors. These are provided as an associative list. Only the last
    error for any field is kept. The new form is returned.
*)
val set_errors : (field * string) list -> ('ctx,'result) form -> ('ctx,'result) form

(** Determine whether a form contains any errors. *)
val has_errors : ('ctx,'result) form -> bool

(** Extract the form data as a result type. This uses the parsing functions provided in the 
    template. Usually, the result type is either a variant that can represent a correct value
    OR a list of errors, or a function that returns such a variant. Use of BatStd.result is
    suggested.
*)
val result : ('ctx,'result) form -> ('ctx,('result, (field * string) list) BatStd.result) Ohm.Run.t

(** Convenience utility functions. *)
module Convenience : sig

  (** Render the form assuming that the endpoint (to which the form is posted) is an URL *)
  val render : ('ctx,'result) form -> string -> ('ctx,Ohm.Html.writer) Ohm.Run.t

  (** Test whether a string (that has been stripped of leading and trailing whitespace) 
      is probably a correct e-mail. *)
  val valid_email : string -> bool 

  (** Email field validation. *)
  val email : 
       required:('ctx,string) Ohm.Run.t
    -> ('ctx,string) Ohm.Run.t
    -> field
    -> string
    -> ('ctx,(string,field * string) BatStd.result) Ohm.Run.t
    
end

(** A default form skin: allows rendering the form with a standard HTML appearance. *)
module Skin : sig

  (** A form wrapper that contains all the rendered fields and an "ok" button. *)
  val with_ok_button : 
       ok:('c,string) Ohm.Run.t
    -> ('c,'s,'r) template
    -> ('c,'s,'r) template

  (** A [type="text"] input field. *)
  val text : 
       label:('c,string) Ohm.Run.t
    -> ('s -> ('c,string) Ohm.Run.t)
    -> (field -> string -> ('c,('r,field * string) BatStd.result) Ohm.Run.t)
    -> ('c,'s,'r) template

  (** A [type="text"] input field with [class="-wide"] *)
  val widetext : 
       label:('c,string) Ohm.Run.t
    -> ('s -> ('c,string) Ohm.Run.t)
    -> (field -> string -> ('c,('r,field * string) BatStd.result) Ohm.Run.t)
    -> ('c,'s,'r) template

  (** A [textarea] field. *)
  val textarea : 
       label:('c,string) Ohm.Run.t
    -> ('s -> ('c,string) Ohm.Run.t)
    -> (field -> string -> ('c,('r,field * string) BatStd.result) Ohm.Run.t)
    -> ('c,'s,'r) template

  (** A [type="password"] input field. *)
  val password : 
       label:('c,string) Ohm.Run.t
    -> (field -> string -> ('c,('r,field * string) BatStd.result) Ohm.Run.t)
    -> ('c,'s,'r) template

  (** A list of radio-buttons. *)
  val radio : 
   ?horizontal:bool -> 
    label:('ctx,string) Ohm.Run.t ->
    format:'data Ohm.Fmt.fmt ->
    source:('data * ('ctx,Ohm.Html.writer) Ohm.Run.t) list ->
    ('seed -> ('ctx,'data option) Ohm.Run.t) ->
    (field -> 'data option  -> ('ctx,('result,field * string) BatStd.result) Ohm.Run.t) ->
    ('ctx,'seed,'result) template    
    
  (** An optional field containing a sub-form *)
  val option : 
       label:('ctx,string) Ohm.Run.t
    -> remove_html:('ctx,Ohm.Html.writer) Ohm.Run.t
    -> add_html:('ctx,Ohm.Html.writer) Ohm.Run.t
    -> ('seed -> ('ctx,'inseed option) Ohm.Run.t) 
    -> ('ctx,'inseed,'result) template
    -> ('ctx,'seed,'result option) template
end
