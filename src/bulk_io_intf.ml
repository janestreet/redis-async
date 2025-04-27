(** The most important element of the RESP3 protocol is a bulk / blob string, which is a
    sequence of bytes having a known length.

    This interface may be implemented to (de)serialize a specific type to and from this
    representation. *)

open Core
open Async

module type S = sig
  type t

  module Redis_bulk_io : sig
    val length : t -> int
    val consume : len:int -> ([> read ], Iobuf.seek) Iobuf.t -> t Or_error.t
    val write : len:int -> Writer.t -> t -> unit

    include Stringable.S with type t := t
  end
end
