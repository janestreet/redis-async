open Core

(* Redis allows an unsigned 64 bit integer for cursor. Because we cannot easily represent
   this in OCaml and the data is opaque to the client anyhow, just store a string. *)

type t = string [@@deriving equal, sexp_of]

let zero      = "0"
let of_string = Fn.id
let to_string = Fn.id
let ( = )     = equal
