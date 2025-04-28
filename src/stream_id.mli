(** A Redis Stream Id whose format is:

    <millisecondsTime>-<sequenceNumber>

    See: https://redis.io/docs/data-types/streams-tutorial/
    https://redis.io/docs/latest/develop/data-types/streams/#entry-ids *)

open! Core
include Identifiable.S_plain

(** The wall clock time implied by the stream id. This time has millisecond precision. *)
val to_time_exn : t -> Time_ns.t

(** The lowest possible stream id *)
val zero : t
