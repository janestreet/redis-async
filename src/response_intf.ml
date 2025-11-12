(** A pending response is created upon issuing a Redis command.

    Each pending response contains a parser appropriate for that response as well as an
    [Ivar] that the creating command will wait for. *)

open Core
open Async

module type S = sig
  type t

  val this : t Or_error.t Ivar.t
  val parse : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t Or_error.t
end
