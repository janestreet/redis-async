open! Core
open Async

exception Need_more_data

(** Check that this iobuf has the requested length available. Throw [Need_more_data] if it
    does not.*)
val check_length_exn : len:int -> (_, _) Iobuf.t -> unit

val write_crlf : Writer.t -> unit
val itoa : int -> string
