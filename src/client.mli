(** A connection to a Redis and low-level methods for interaction *)

open Core
open Async

type 'a t

module Make (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) : sig
  module Key_parser   : Parse_bulk_intf.S with type t := Key.t
  module Field_parser : Parse_bulk_intf.S with type t := Field.t
  module Value_parser : Parse_bulk_intf.S with type t := Value.t

  module Field_value_map_parser :
    Parse_bulk_intf.S_map with type key := Field.t and type value := Value.t

  val create
    :  ?on_disconnect:(unit -> unit)
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t
    -> unit
    -> Key.t t Deferred.Or_error.t

  val close : Key.t t -> unit Deferred.t

  (** Send a command built from strings to Redis and expect a Response of the specified
      kind.

      All Redis commands are arrays of strings (keeping in mind that strings are the same
      as byte arrays) so this is the most general form. *)
  val command_string
    :  Key.t t
    -> string list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s to Redis and
      expect a Response of the specified kind. *)
  val command_key
    :  Key.t t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> Key.t list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      serialized command arguments to Redis and expect a Response of the specified kind. *)
  type ('arg, 'r) command_key_args :=
    Key.t t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> Key.t list
    -> 'arg list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      Value.t arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_values : (Value.t, 'r) command_key_args

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      Field.t arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_fields : (Field.t, 'r) command_key_args

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      string arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_string_args : (string, 'r) command_key_args

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      serialized [Field.t], [Value.t] pairs to Redis and expect a Response of the
      specified kind. *)
  val command_keys_fields_and_values
    :  Key.t t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> Key.t list
    -> (Field.t * Value.t) list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by an associative list of interleaved
      [Key.t]s and [Value.t]s to Redis and expect a Response of the specified kind. *)
  val command_kv
    :  Key.t t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> (Key.t, Value.t) List.Assoc.t
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by an associative
      list of interleaved [int]s and [Value.t]s to Redis and expect a Response of the
      specified kind.*)
  val command_key_scores_values
    :  Key.t t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> Key.t
    -> ([ `Score of float ] * Value.t) list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by [int]s range
      items to Redis and expect a Response of the specified kind. *)
  val command_key_range
    :  Key.t t
    -> string list
    -> Key.t
    -> min_index:int
    -> max_index:int
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by
      [Value.t Maybe_bound.t]s range items to Redis and expect a Response
      of the specified kind. *)
  val command_key_lex_range
    :  Key.t t
    -> string list
    -> Key.t
    -> min:Value.t Maybe_bound.t
    -> max:Value.t Maybe_bound.t
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Turn on Redis client tracking and provide a pipe of invalidation messages received
      from the server. Closing the pipe turns tracking off.

      Read here for more on usage:
      https://redis.io/commands/client-tracking
      https://redis.io/topics/client-side-caching

      @param bcast Whether to use broadcast mode. Off by default.
  *)
  val client_tracking
    :  Key.t t
    -> ?bcast:bool
    -> unit
    -> [ `All | `Key of Key.t ] Pipe.Reader.t Deferred.Or_error.t
end
