(** A cursor type for use with commands such as SCAN *)

open Core

type t [@@deriving equal, sexp_of]

val zero : t
val ( = ) : t -> t -> bool

include Stringable.S with type t := t
