(** Key events that you may ask Redis to inform you of
    https://redis.io/docs/manual/keyspace-notifications/

    This variant is incomplete and is intended to be added to as needed.
*)

open Core

type t =
  [ `del
  | `expire
  | `new_
  ]
[@@deriving sexp_of]

include Stringable.S with type t := t
