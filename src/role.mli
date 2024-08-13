(** The role of a Redis node as described by
    https://redis.io/commands/role/
*)

open Core

module Replica : sig
  module Connection_state : sig
    type t =
      | Connect
      | Connecting
      | Sync
      | Connected
      | Handshake
    [@@deriving sexp_of, compare]
  end

  type t =
    { leader : Host_and_port.t
    ; connection_state : Connection_state.t
    ; replication_offset : int
    }
  [@@deriving sexp_of, compare]
end

module Leader : sig
  module Replica : sig
    type t =
      { where_to_connect : Host_and_port.t
      ; replication_offset : int
      }
    [@@deriving sexp_of, compare]
  end

  type t =
    { replication_offset : int
    ; replicas : Replica.t list
    }
  [@@deriving sexp_of, compare]
end

module Sentinel : sig
  (** The array of leader names monitored by the Sentinel instance *)
  type t = string list [@@deriving sexp_of, compare]
end

type t =
  | Leader of Leader.t
  | Replica of Replica.t
  | Sentinel of Sentinel.t
[@@deriving sexp_of, compare]

val of_resp3 : Resp3.t -> t Or_error.t

module For_test_with_deterministic_sexp : sig
  type nonrec t = t [@@deriving compare, sexp_of]
end
