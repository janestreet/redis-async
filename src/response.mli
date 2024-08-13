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

val handle_unexpected_response' : expected:string -> Resp3.t -> Error.t
val handle_unexpected_response : expected:string -> Resp3.t -> _ Or_error.t

(** Create a response that runs the supplied parsing function *)
val create
  :  ((read, Iobuf.seek) Iobuf.t -> 'a Or_error.t)
  -> (module Response_intf.S with type t = 'a)

(** Create a response that expects 'OK' *)
val create_ok : unit -> (module Response_intf.S with type t = unit)

(** Create a response that expects an integer *)
val create_int : unit -> (module Response_intf.S with type t = int)

(** Create a response that expects an optional integer *)
val create_int_option : unit -> (module Response_intf.S with type t = int option)

(** Create a response that expects an optional float. *)
val create_float_option : unit -> (module Response_intf.S with type t = float option)

(** Create a response that allows the full RESP3 representation *)
val create_resp3 : unit -> (module Response_intf.S with type t = Resp3.t)

(** Create a response that expects an integer of 0 or 1 *)
val create_01_bool : unit -> (module Response_intf.S with type t = bool)

(** Create a response that expects a list of integers of 0 or 1 *)
val create_01_bool_list : unit -> (module Response_intf.S with type t = bool list)

(** Create a response that expects a specific string followed by a number, which is part
    of the response to a [P]SUBSCRIBE command *)
val create_subscription
  :  channel:string
  -> on_success:(unit -> unit)
  -> (module Response_intf.S with type t = int)

(** Same as [create_subscription], except in response to a [P]UNSUBSCRIBE command. *)
val create_unsubscription
  :  channel:string
  -> on_success:(unit -> unit)
  -> (module Response_intf.S with type t = int)

(** Create a response that expects a string *)
val create_string : unit -> (module Response_intf.S with type t = string)

(** Create a response that expects a string list *)
val create_string_list : unit -> (module Response_intf.S with type t = string list)

(** Create a response that expects a string encoded ip and port *)
val create_host_and_port : unit -> (module Response_intf.S with type t = Host_and_port.t)

(** Create a response that expects role information from Redis *)
val create_role : unit -> (module Response_intf.S with type t = Role.t)

(** Create a response that expects a map with string keys *)
val create_string_map
  :  unit
  -> (module Response_intf.S with type t = Resp3.t String.Map.t)

(** Create a response that expects an array of string maps *)
val create_string_map_list
  :  unit
  -> (module Response_intf.S with type t = Resp3.t String.Map.t list)
