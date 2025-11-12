(** The RESP3 protocol as specified here:
    https://github.com/antirez/RESP3/blob/master/spec.md *)

open Core

exception Protocol_error of string

type t =
  | String of string
  | Error of string
  | Int of int
  | Null
  | Double of float
  | Boolean of bool
  | Bignum of Bignum.t
  | Array of t array
  | Map of (t * t) array
  | Set of t array
[@@deriving compare, equal, sexp_of]

val simple_string : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> string
val blob_string : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> string
val extract_error : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> Error.t
val parse_exn : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t
val expect_crlf : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> unit
val expect_char : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> char -> unit
val consume_char : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> char
val peek_char : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> char
val ends_in_crlf : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> bool
val number : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> int
