(** A connection to a Redis and low-level methods for interaction *)

open Core
open Async

type ('a, 'key, 'field, 'value) t

module Make (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) : sig
  module Key_parser : Parse_bulk_intf.S with type t := Key.t
  module Field_parser : Parse_bulk_intf.S with type t := Field.t
  module Value_parser : Parse_bulk_intf.S with type t := Value.t

  module Field_value_map_parser :
    Parse_bulk_intf.S_map with type key := Field.t and type value := Value.t

  type nonrec 'a t = ('a, Key.t, Field.t, Value.t) t

  (** Create a client that connects directly to a Redis node. [on_disconnect] will be
      called after disconnecting from the Redis node. *)
  val create
    :  ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t
    -> ([< `Leader | `Replica | `Sentinel ] as 'a)
    -> 'a t Deferred.Or_error.t

  val sentinel_get_leader_address
    :  [< `Sentinel ] t
    -> leader_name:string
    -> Host_and_port.t Deferred.Or_error.t

  val sentinel_replicas
    :  [< `Sentinel ] t
    -> leader_name:string
    -> Sentinel.Replica.t list Deferred.Or_error.t

  val sentinel_connect_to_one_replica
    :  [< `Sentinel ] t
    -> ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> ?replica_priority_sorter:(Sentinel.Replica.t list -> Sentinel.Replica.t list)
    -> leader_name:string
    -> unit
    -> [< `Replica ] t Deferred.Or_error.t

  (** Create a leader connection given a sentinel connection. *)
  val sentinel_connect_to_leader
    :  [< `Sentinel ] t
    -> ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> leader_name:string
    -> unit
    -> [< `Leader ] t Deferred.Or_error.t

  val close : _ t -> unit Deferred.t
  val close_finished : _ t -> unit Deferred.t
  val has_close_started : _ t -> bool
  val connection_state : _ t -> [ `Connected | `Disconnected | `Disconnecting ]

  (** Send a command built from strings to Redis and expect a Response of the specified
      kind.

      All Redis commands are arrays of strings (keeping in mind that strings are the same
      as byte arrays) so this is the most general form. *)
  val command_string
    :  _ t
    -> string list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s to Redis and
      expect a Response of the specified kind. *)
  val command_key
    :  _ t
    -> ?result_of_empty_input:'r Or_error.t
         (** If [Key.t list] is empty, and [result_of_empty_input] is not provided, Redis
             will return an error. *)
    -> string list
    -> Key.t list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      serialized command arguments to Redis and expect a Response of the specified kind. *)
  type ('arg, 'r, 'a) command_key_args :=
    'a t
    -> ?result_of_empty_input:'r Or_error.t
         (** If [Key.t list] or ['arg list] is empty, and [result_of_empty_input] is not
             provided, Redis will return an error. *)
    -> string list
    -> Key.t list
    -> 'arg list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      Value.t arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_values : (Value.t, 'r, _) command_key_args

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      Field.t arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_fields : (Field.t, 'r, _) command_key_args

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      string arguments to Redis and expect a Response of the specified kind. *)
  val command_keys_string_args : (string, 'r, _) command_key_args

  (** Send a command built from strings followed by a serialized [Key.t], followed by
      string arguments, followed by serialized [Field.t]s to Redis and expect a Response
      of the specified kind. *)
  val command_key_string_args_fields
    :  _ t
    -> ?result_of_empty_input:'r Or_error.t
         (** If [Field.t list] or the args [string list] is empty, and
             [result_of_empty_input] is not provided, Redis will return an error. *)
    -> string list
    -> Key.t
    -> string list
    -> Field.t list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by serialized [Key.t]s, followed by
      serialized [Field.t], [Value.t] pairs to Redis and expect a Response of the
      specified kind. *)
  val command_keys_fields_and_values
    :  _ t
    -> ?result_of_empty_input:'r Or_error.t
         (** If [Key.t list] or [(Field.t * Value.t) list] is empty, and
             [result_of_empty_input] is not provided, Redis will return an error. *)
    -> string list
    -> Key.t list
    -> string list
    -> (Field.t * Value.t) list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by an associative list of interleaved
      [Key.t]s and [Value.t]s to Redis and expect a Response of the specified kind. *)
  val command_kv
    :  _ t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> (Key.t, Value.t) List.Assoc.t
    -> string list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by an associative
      list of interleaved [int]s and [Value.t]s to Redis and expect a Response of the
      specified kind. *)
  val command_key_scores_values
    :  _ t
    -> ?result_of_empty_input:'r Or_error.t
    -> string list
    -> Key.t
    -> ([ `Score of float ] * Value.t) list
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by [int]s range
      items to Redis and expect a Response of the specified kind. *)
  val command_key_range
    :  _ t
    -> string list
    -> Key.t
    -> min_index:int
    -> max_index:int
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by
      [float Maybe_bound.t]s range items to Redis and expect a Response of the specified
      kind. *)
  val command_key_score_range
    :  _ t
    -> string list
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> with_scores:bool
         (** If true, use [Value_parser.with_scores] to build the response type. *)
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Send a command built from strings followed by [Key.t], followed by
      [Value.t Maybe_bound.t]s range items to Redis and expect a Response of the specified
      kind. *)
  val command_key_lex_range
    :  _ t
    -> string list
    -> Key.t
    -> min:Value.t Maybe_bound.t
    -> max:Value.t Maybe_bound.t
    -> (module Response_intf.S with type t = 'r)
    -> 'r Deferred.Or_error.t

  (** Turn on Redis client tracking and provide a pipe of invalidation messages received
      from the server. Closing the pipe turns tracking off.

      Read here for more on usage: https://redis.io/commands/client-tracking
      https://redis.io/topics/client-side-caching

      @param bcast Whether to use broadcast mode. Off by default. *)
  val client_tracking
    :  [< `Leader | `Replica ] t
    -> ?bcast:bool
    -> unit
    -> [ `All | `Key of Key.t ] Pipe.Reader.t Deferred.Or_error.t

  (** Returns a pipe reader for a subscription. Multiple subscriptions to the same channel
      share the same redis subscription. If all readers subscribed to a channel close
      their pipe, an unsubscribe would eventually be issued. *)
  val subscribe_raw
    :  [< `Leader | `Replica ] t
    -> [ `Literal of string list | `Pattern of string list ]
    -> consume:((read, Iobuf.seek) Iobuf.t -> subscription:string -> 'a)
    -> 'a Pipe.Reader.t Deferred.Or_error.t
end
