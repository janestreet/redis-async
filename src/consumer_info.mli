(** The return value for the XINFO CONSUMERS command
    https://redis.io/commands/xinfo-consumers/
*)

open! Core

type t =
  { name : Consumer.t
  ; pending : int
  ; idle : Time_ns.Span.t
  ; inactive : Time_ns.Span.t option (** [inactive] was added in 7.2.x *)
  }
[@@deriving sexp_of]

val of_resp3 : Resp3.t -> t Or_error.t

(** Before Redis 7.2.0, idle used to denote the time passed since last successful
    interaction. In 7.2.0, inactive was added and idle was changed to denote the time
    passed since last attempted interaction.

    https://redis.io/docs/latest/commands/xinfo-consumers/
*)
val last_successful_interaction : t -> Time_ns.Span.t

val last_attempted_interaction : t -> Time_ns.Span.t option
