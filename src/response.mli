(** A Response describes how a Redis reply resulting from a particular command should be
    handled.

    Replies from Redis are in the RESP3 protocol format, which is an all-inclusive
    superset of possible data representations. Individual Redis commands result in a
    limited set of expected responses that are much more tightly scoped than RESP3. For
    example: a SET command is expected to reply with a RESP3 simple string with contents
    "OK".

    Working with raw RESP3 makes coding individual commands both inconvenient and
    inefficient. By providing dedicated parsers for expected responses we allow for
    typeful APIs with compact and efficient implementations. *)

open Core

(** Create a response that runs the supplied parsing function *)
val create
  :  ((read, Iobuf.seek) Iobuf.t -> 'a Or_error.t)
  -> (module Response_intf.S with type t = 'a)

(** Create a response that expects 'OK' *)
val create_ok      : unit -> (module Response_intf.S with type t = unit)

(** Create a response that expects an integer *)
val create_int     : unit -> (module Response_intf.S with type t = int)

(** Create a response that allows the full RESP3 representation *)
val create_resp3   : unit -> (module Response_intf.S with type t = Resp3.t)

(** Create a response that expects an integer of 0 or 1 *)
val create_01_bool : unit -> (module Response_intf.S with type t = bool)

(** Create a response that expects a list of integers of 0 or 1 *)
val create_01_bool_list : unit -> (module Response_intf.S with type t = bool list)
