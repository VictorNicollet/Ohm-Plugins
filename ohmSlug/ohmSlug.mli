(* Ohm is © 2012 Victor Nicollet *)

(** Generate slugs: url-compatible sequences of characters. *)

(** Standard generation algorithm. Folds accents down to their underlying latin letters, lowercases
    everything, and replaces non-letter non-number characters with hyphens. *)
val make : string -> string
