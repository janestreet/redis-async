open! Core

(** Represents one element of the return value of the 'sentinel replicas' command. This is
    a partial implementation and more fields can be added as-needed.

    Non-inclusive field names here match the stable underlying protocol. It may be
    possible to update this someday: https://github.com/valkey-io/valkey/issues/751
*)
module Replica : sig
  type t =
    { name : string
    ; host_and_port : Host_and_port.t
    ; flags : string
    ; master_link_status : string
    ; master_host_and_port : Host_and_port.t
    ; slave_priority : int
    }
  [@@deriving sexp_of]

  val of_string_map : Resp3.t String.Map.t -> t Or_error.t
end

(** Represents the return value of the 'sentinel master <name>' command. This is a partial
    implementation and more fields can be added as-needed. *)
module Leader : sig
  type t =
    { name : string
    ; host_and_port : Host_and_port.t
    ; down_after : Time_ns.Span.t
    ; failover_timeout : Time_ns.Span.t
    }

  val of_string_map : Resp3.t String.Map.t -> t Or_error.t
end
