(** A Redis client that provides a typeful API around commands.

    Note that if you prefer a vanilla Redis client you can specify Bulk_io.String for
    [Key] and [Value].
*)

open! Core
open  Async

module type S = sig
  module Key : sig
    type t
  end

  module Field : sig
    type t
  end

  module Value : sig
    type t
  end

  type t

  val create
    :  ?on_disconnect:(unit -> unit)
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t
    -> unit
    -> t Deferred.Or_error.t

  val close    : t -> unit Deferred.t

  (** Redis commands are documented at: https://redis.io/commands *)

  val select   : t -> int -> unit Deferred.Or_error.t
  val flushall : t -> unit Deferred.Or_error.t
  val flushdb  : t -> unit Deferred.Or_error.t
  val shutdown : t -> unit Deferred.Or_error.t
  val echo     : t -> Key.t -> Key.t Deferred.Or_error.t
  val ping     : t -> string -> string Deferred.Or_error.t
  val incr     : t -> Key.t -> int Deferred.Or_error.t
  val del      : t -> Key.t list -> int Deferred.Or_error.t
  val unlink   : t -> Key.t list -> int Deferred.Or_error.t
  val dbsize   : t -> int Deferred.Or_error.t
  val exists   : t -> Key.t list -> int Deferred.Or_error.t
  val keys     : ?pattern:string (** defaults to '*' *) -> t -> Key.t list Deferred.Or_error.t

  val scan
    :  t
    -> cursor:Cursor.t
    -> ?count:int
    -> ?pattern:string
    -> unit
    -> (Cursor.t * Key.t list) Deferred.Or_error.t

  (** Turn on Redis client tracking and provide a pipe of invalidation messages received
      from the server. Closing the pipe turns tracking off.

      The NOLOOP option is used, which means that subscribers will not see invalidation
      messages caused by themselves, unless it is from the flushdb / flushall command.

      Read here for more on usage:
      https://redis.io/commands/client-tracking
      https://redis.io/topics/client-side-caching

      @param bcast Whether to use BCAST. Off by default.
  *)
  val client_tracking
    :  t
    -> ?bcast:bool
    -> unit
    -> [ `All | `Key of Key.t ] Pipe.Reader.t Deferred.Or_error.t

  val set        : t -> Key.t -> ?expire:Time_ns.Span.t -> Value.t -> unit Deferred.Or_error.t
  val setnx      : t -> Key.t -> Value.t -> bool Deferred.Or_error.t
  val mset       : t -> (Key.t * Value.t) list -> unit Deferred.Or_error.t
  val msetnx     : t -> (Key.t * Value.t) list -> bool Deferred.Or_error.t
  val get        : t -> Key.t -> Value.t option Deferred.Or_error.t
  val mget       : t -> Key.t list -> Value.t option list Deferred.Or_error.t
  val smembers   : t -> Key.t -> Value.t list Deferred.Or_error.t
  val sismember  : t -> Key.t -> Value.t -> bool Deferred.Or_error.t
  val smismember : t -> Key.t -> Value.t list -> bool list Deferred.Or_error.t
  val sadd       : t -> Key.t -> Value.t list -> int Deferred.Or_error.t
  val srem       : t -> Key.t -> Value.t list -> int Deferred.Or_error.t
  val zadd       : t -> Key.t -> ([ `Score of float ] * Value.t) list -> int Deferred.Or_error.t
  val zrem       : t -> Key.t -> Value.t list -> int Deferred.Or_error.t

  val pttl
    :  t
    -> Key.t
    -> [ `Timeout of Time_ns.Span.t | `No_timeout | `No_key ] Deferred.Or_error.t

  val pexpire : t -> Key.t -> Time_ns.Span.t -> [ `Set | `Not_set ] Deferred.Or_error.t

  val zrange
    :  t
    -> Key.t
    -> min_index:int
    -> max_index:int
    -> Value.t list Deferred.Or_error.t

  val zrangebylex
    :  t
    -> Key.t
    -> min:Value.t Maybe_bound.t
    -> max:Value.t Maybe_bound.t
    -> Value.t list Deferred.Or_error.t

  val hset    : t -> Key.t -> (Field.t * Value.t) list -> int     Deferred.Or_error.t
  val hget    : t -> Key.t -> Field.t -> Value.t option           Deferred.Or_error.t
  val hmget   : t -> Key.t -> Field.t list -> Value.t option list Deferred.Or_error.t
  val hgetall : t -> Key.t -> (Field.t * Value.t) list            Deferred.Or_error.t
  val hvals   : t -> Key.t -> Value.t list                        Deferred.Or_error.t
  val hkeys   : t -> Key.t -> Field.t list                        Deferred.Or_error.t
  val hdel    : t -> Key.t -> Field.t list -> int                 Deferred.Or_error.t

  val hscan
    :  t
    -> cursor:Cursor.t
    -> ?count:int
    -> Key.t
    -> (Cursor.t * (Field.t * Value.t) list) Deferred.Or_error.t

  val keyevent_notifications
    :  t
    -> ([< Key_event.t ] as 'a) list
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val keyspace_notifications
    :  t
    -> ([< Key_event.t ] as 'a) list
    -> patterns:string list
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val publish     : t -> string -> Key.t -> int Deferred.Or_error.t
  val subscribe   : t -> string list -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t
  val psubscribe  : t -> string list -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t
  val script_load : t -> string -> Sha1.t Deferred.Or_error.t
  val evalsha     : t -> Sha1.t -> Key.t list -> Value.t list -> Resp3.t Deferred.Or_error.t

  val raw_command : t -> string list -> Resp3.t Deferred.Or_error.t
  val version     : t -> string                 Deferred.Or_error.t
end
