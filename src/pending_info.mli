(** The return value for the XPENDING command
    https://redis.io/commands/xpending/
*)

open! Core

module Extended : sig
  type t =
    { stream_id : Stream_id.t
    ; owner : Consumer.t
    ; since_last_delivery : Time_ns.Span.t
    ; delivery_count : int
    }
  [@@deriving sexp_of]

  val of_resp3 : Resp3.t -> t Or_error.t
end
