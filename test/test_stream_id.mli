(** A stream ID with formatting made partially opaque for tests. This is necessary because
    the first part of a stream id contains a time component. *)

open! Core

type t = Redis.Stream_id.t [@@deriving sexp_of]
