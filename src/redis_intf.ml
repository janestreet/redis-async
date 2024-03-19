(** A Redis client that provides a typeful API around commands.

    Note that if you prefer a vanilla Redis client you can specify Bulk_io.String for
    [Key] and [Value].
*)

open! Core
open Async

type ('a, 'key, 'field, 'value) t = ('a, 'key, 'field, 'value) Client.t

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

  type 'a t = ('a, Key.t, Field.t, Value.t) Client.t

  val create'
    :  ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t
    -> 'a
    -> 'a t Deferred.Or_error.t

  val create
    :  ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t
    -> unit
    -> [ `Primary ] t Deferred.Or_error.t

  val create_using_sentinel
    :  ?on_disconnect:(unit -> unit)
    -> ?sentinel_auth:Auth.t
    -> ?auth:Auth.t
    -> leader_name:string
    -> where_to_connect:[< Socket.Address.t ] Tcp.Where_to_connect.t list
    -> unit
    -> [ `Primary ] t Deferred.Or_error.t

  val close : _ t -> unit Deferred.t
  val close_finished : _ t -> unit Deferred.t
  val has_close_started : _ t -> bool

  (** Redis commands are documented at: https://redis.io/commands *)

  val select : [< `Primary | `Replica ] t -> int -> unit Deferred.Or_error.t
  val flushall : [< `Primary ] t -> unit Deferred.Or_error.t
  val flushdb : [< `Primary ] t -> unit Deferred.Or_error.t
  val shutdown : [< `Primary | `Replica | `Sentinel ] t -> unit Deferred.Or_error.t
  val echo : [< `Primary | `Replica ] t -> Key.t -> Key.t Deferred.Or_error.t

  val ping
    :  [< `Primary | `Replica | `Sentinel ] t
    -> string
    -> string Deferred.Or_error.t

  val incr : [< `Primary ] t -> Key.t -> int Deferred.Or_error.t
  val del : [< `Primary ] t -> Key.t list -> int Deferred.Or_error.t
  val unlink : [< `Primary ] t -> Key.t list -> int Deferred.Or_error.t
  val dbsize : [< `Primary | `Replica ] t -> int Deferred.Or_error.t
  val exists : [< `Primary | `Replica ] t -> Key.t list -> int Deferred.Or_error.t

  val keys
    :  ?pattern:string (** defaults to '*' *)
    -> [< `Primary | `Replica ] t
    -> Key.t list Deferred.Or_error.t

  val rename : [< `Primary ] t -> Key.t -> new_key:Key.t -> unit Deferred.Or_error.t

  val scan
    :  [< `Primary | `Replica ] t
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
    :  [< `Primary | `Replica ] t
    -> ?bcast:bool
    -> unit
    -> [ `All | `Key of Key.t ] Pipe.Reader.t Deferred.Or_error.t

  val set
    :  [< `Primary ] t
    -> Key.t
    -> ?expire:Time_ns.Span.t
    -> Value.t
    -> unit Deferred.Or_error.t

  val setnx : [< `Primary ] t -> Key.t -> Value.t -> bool Deferred.Or_error.t
  val mset : [< `Primary ] t -> (Key.t * Value.t) list -> unit Deferred.Or_error.t
  val msetnx : [< `Primary ] t -> (Key.t * Value.t) list -> bool Deferred.Or_error.t
  val get : [< `Primary | `Replica ] t -> Key.t -> Value.t option Deferred.Or_error.t
  val mget : [< `Primary ] t -> Key.t list -> Value.t option list Deferred.Or_error.t
  val smembers : [< `Primary | `Replica ] t -> Key.t -> Value.t list Deferred.Or_error.t

  val sismember
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> Value.t
    -> bool Deferred.Or_error.t

  val smismember
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> Value.t list
    -> bool list Deferred.Or_error.t

  val sadd : [< `Primary ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t
  val srem : [< `Primary ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t

  val zadd
    :  [< `Primary ] t
    -> Key.t
    -> ([ `Score of float ] * Value.t) list
    -> int Deferred.Or_error.t

  val zrem : [< `Primary ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t

  val pttl
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> [ `Timeout of Time_ns.Span.t | `No_timeout | `No_key ] Deferred.Or_error.t

  val pexpire
    :  [< `Primary ] t
    -> Key.t
    -> ?nx:bool
    -> Time_ns.Span.t
    -> [ `Set | `Not_set ] Deferred.Or_error.t

  val pexpireat
    :  [< `Primary ] t
    -> Key.t
    -> ?nx:bool
    -> Time_ns.t
    -> [ `Set | `Not_set ] Deferred.Or_error.t

  val zrange
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> min_index:int
    -> max_index:int
    -> Value.t list Deferred.Or_error.t

  val zscore
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> Value.t
    -> [ `Score of float ] option Deferred.Or_error.t

  val zrangebylex
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> min:Value.t Maybe_bound.t
    -> max:Value.t Maybe_bound.t
    -> Value.t list Deferred.Or_error.t

  val zrangebyscore
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> Value.t list Deferred.Or_error.t

  val zrangebyscore_withscores
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> (Value.t * [ `Score of float ]) list Deferred.Or_error.t

  val zremrangebyscore
    :  [< `Primary ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> int Deferred.Or_error.t

  val hexists : [< `Primary | `Replica ] t -> Key.t -> Field.t -> bool Deferred.Or_error.t

  val hset
    :  [< `Primary ] t
    -> Key.t
    -> (Field.t * Value.t) list
    -> int Deferred.Or_error.t

  val hget
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> Field.t
    -> Value.t option Deferred.Or_error.t

  val hmget
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> Field.t list
    -> Value.t option list Deferred.Or_error.t

  val hgetall
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> (Field.t * Value.t) list Deferred.Or_error.t

  val hvals : [< `Primary | `Replica ] t -> Key.t -> Value.t list Deferred.Or_error.t
  val hkeys : [< `Primary | `Replica ] t -> Key.t -> Field.t list Deferred.Or_error.t
  val hlen : [< `Primary | `Replica ] t -> Key.t -> int Deferred.Or_error.t
  val hdel : [< `Primary ] t -> Key.t -> Field.t list -> int Deferred.Or_error.t

  val hscan
    :  [< `Primary | `Replica ] t
    -> cursor:Cursor.t
    -> ?count:int
    -> Key.t
    -> (Cursor.t * (Field.t * Value.t) list) Deferred.Or_error.t

  val keyevent_notifications
    :  [< `Primary | `Replica ] t
    -> ([< Key_event.t ] as 'a) list
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val keyspace_notifications
    :  [< `Primary | `Replica ] t
    -> ([< Key_event.t ] as 'a) list
    -> [ `Patterns of string list | `Keys of Key.t list ]
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val publish : [< `Primary | `Replica ] t -> string -> Key.t -> int Deferred.Or_error.t

  val subscribe
    :  [< `Primary | `Replica ] t
    -> string list
    -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val subscribe_raw
    :  [< `Primary | `Replica ] t
    -> [ `Literal of string list | `Pattern of string list ]
    -> consume:((read, Iobuf.seek) Iobuf.t -> subscription:string -> 'a)
    -> 'a Pipe.Reader.t Deferred.Or_error.t

  val psubscribe
    :  [< `Primary | `Replica ] t
    -> string list
    -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val script_load : [< `Primary | `Replica ] t -> string -> Sha1.t Deferred.Or_error.t

  val evalsha
    :  [< `Primary | `Replica ] t
    -> Sha1.t
    -> Key.t list
    -> Value.t list
    -> Resp3.t Deferred.Or_error.t

  val raw_command
    :  [< `Primary | `Replica | `Sentinel ] t
    -> string list
    -> Resp3.t Deferred.Or_error.t

  val version : [< `Primary | `Replica | `Sentinel ] t -> string Deferred.Or_error.t
  val role : [< `Primary | `Replica | `Sentinel ] t -> Role.t Deferred.Or_error.t

  (** ACL and authentication commands.

      Read here for more:

      https://redis.io/docs/manual/security/acl/#command-categories
      https://redis.io/docs/manual/security/acl/#selectors
  *)
  val acl_setuser
    :  [< `Primary | `Replica | `Sentinel ] t
    -> username:string
    -> rules:string list
    -> unit Deferred.Or_error.t

  val acl_deluser
    :  [< `Primary | `Replica | `Sentinel ] t
    -> string list
    -> int Deferred.Or_error.t

  val acl_users
    :  [< `Primary | `Replica | `Sentinel ] t
    -> string list Deferred.Or_error.t

  val acl_list : [< `Primary | `Replica | `Sentinel ] t -> string list Deferred.Or_error.t

  val acl_getuser
    :  [< `Primary | `Replica | `Sentinel ] t
    -> username:string
    -> Resp3.t Deferred.Or_error.t

  val auth
    :  [< `Primary | `Replica | `Sentinel ] t
    -> auth:Auth.t
    -> unit
    -> unit Deferred.Or_error.t

  (** Sentinel specific commands.

      Read here for more: https://redis.io/docs/manual/sentinel/#sentinel-api *)
  val sentinel_leader : [< `Sentinel ] t -> string -> Host_and_port.t Deferred.Or_error.t

  (** Streams *)

  val xadd
    :  [< `Primary ] t
    -> Key.t
    -> ?stream_id:Stream_id.t
    -> (Field.t * Value.t) list
    -> Stream_id.t Deferred.Or_error.t

  val xgroup_create
    :  [< `Primary ] t
    -> Key.t
    -> Group.t
    -> ?stream_id:Stream_id.t
    -> ?mkstream:unit
    -> unit
    -> [ `Ok | `Already_exists ] Deferred.Or_error.t

  val xrange
    :  [< `Primary | `Replica ] t
    -> Key.t
    -> ?start:Stream_id.t
    -> ?end_:Stream_id.t
    -> ?count:int
    -> unit
    -> (Stream_id.t * (Field.t * Value.t) list) list Deferred.Or_error.t

  val xreadgroup
    :  [< `Primary | `Replica ] t
    -> Group.t
    -> Consumer.t
    -> ?count:int
    -> ?block:[ `Don't_block | `Forever | `For_up_to of Time_ns.Span.t ]
    -> (Key.t * Stream_id.t option) list
    -> (Key.t * (Stream_id.t * (Field.t * Value.t) list) list) list Deferred.Or_error.t

  val xclaim
    :  [< `Primary ] t
    -> ?idle:Time_ns.Span.t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> min_idle_time:Time_ns.Span.t
    -> Stream_id.t list
    -> (Stream_id.t * (Field.t * Value.t) list) list Deferred.Or_error.t

  val xclaim_justid
    :  [< `Primary ] t
    -> ?idle:Time_ns.Span.t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> min_idle_time:Time_ns.Span.t
    -> Stream_id.t list
    -> Stream_id.t list Deferred.Or_error.t

  val xautoclaim
    :  [< `Primary ] t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> min_idle_time:Time_ns.Span.t
    -> start:Stream_id.t
    -> ?count:int
    -> unit
    -> ([ `Next_stream_id of Stream_id.t ]
       * (Stream_id.t * (Field.t * Value.t) list) list
       * [ `No_longer_exist of Stream_id.t list ])
       Deferred.Or_error.t

  val xack
    :  [< `Primary ] t
    -> Key.t
    -> Group.t
    -> Stream_id.t list
    -> int Deferred.Or_error.t
end
