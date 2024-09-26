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
    -> [ `Leader ] t Deferred.Or_error.t

  val sentinel_replicas
    :  [< `Sentinel ] t
    -> leader_name:string
    -> Sentinel.Replica.t list Deferred.Or_error.t

  val sentinel_get_leader_address
    :  [< `Sentinel ] t
    -> leader_name:string
    -> Host_and_port.t Or_error.t Deferred.t

  (** Create a replica connection given a sentinel connection.

      @param replica_priority_sorter Provides the order in which to attempt replica
      connections. Defaults to randomization.
  *)
  val sentinel_connect_to_one_replica
    :  [< `Sentinel ] t
    -> ?on_disconnect:(unit -> unit)
    -> ?auth:Auth.t
    -> ?replica_priority_sorter:(Sentinel.Replica.t list -> Sentinel.Replica.t list)
    -> leader_name:string
    -> unit
    -> [< `Replica ] t Deferred.Or_error.t

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

  (** Redis commands are documented at: https://redis.io/commands *)

  val select : [< `Leader | `Replica ] t -> int -> unit Deferred.Or_error.t
  val flushall : [< `Leader ] t -> unit Deferred.Or_error.t
  val flushdb : [< `Leader ] t -> unit Deferred.Or_error.t
  val shutdown : [< `Leader | `Replica | `Sentinel ] t -> unit Deferred.Or_error.t
  val echo : [< `Leader | `Replica ] t -> Key.t -> Key.t Deferred.Or_error.t
  val ping : [< `Leader | `Replica | `Sentinel ] t -> string -> string Deferred.Or_error.t

  val wait
    :  [< `Leader ] t
    -> num_replicas:int
    -> timeout:[ `Never | `After of Time_ns.Span.t ]
    -> int Deferred.Or_error.t

  val incr : [< `Leader ] t -> Key.t -> int Deferred.Or_error.t
  val del : [< `Leader ] t -> Key.t list -> int Deferred.Or_error.t
  val unlink : [< `Leader ] t -> Key.t list -> int Deferred.Or_error.t
  val dbsize : [< `Leader | `Replica ] t -> int Deferred.Or_error.t
  val exists : [< `Leader | `Replica ] t -> Key.t list -> int Deferred.Or_error.t

  val keys
    :  ?pattern:string (** defaults to '*' *)
    -> [< `Leader | `Replica ] t
    -> Key.t list Deferred.Or_error.t

  val rename : [< `Leader ] t -> Key.t -> new_key:Key.t -> unit Deferred.Or_error.t

  val scan
    :  [< `Leader | `Replica ] t
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
    :  [< `Leader | `Replica ] t
    -> ?bcast:bool
    -> unit
    -> [ `All | `Key of Key.t ] Pipe.Reader.t Deferred.Or_error.t

  val set
    :  [< `Leader ] t
    -> Key.t
    -> ?expire:Time_ns.Span.t
    -> Value.t
    -> unit Deferred.Or_error.t

  val setnx : [< `Leader ] t -> Key.t -> Value.t -> bool Deferred.Or_error.t
  val mset : [< `Leader ] t -> (Key.t * Value.t) list -> unit Deferred.Or_error.t
  val msetnx : [< `Leader ] t -> (Key.t * Value.t) list -> bool Deferred.Or_error.t
  val get : [< `Leader | `Replica ] t -> Key.t -> Value.t option Deferred.Or_error.t

  val mget
    :  [< `Leader | `Replica ] t
    -> Key.t list
    -> Value.t option list Deferred.Or_error.t

  val smembers : [< `Leader | `Replica ] t -> Key.t -> Value.t list Deferred.Or_error.t

  val sismember
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Value.t
    -> bool Deferred.Or_error.t

  val smismember
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Value.t list
    -> bool list Deferred.Or_error.t

  val sadd : [< `Leader ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t
  val srem : [< `Leader ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t

  val sscan
    :  [< `Leader | `Replica ] t
    -> cursor:Cursor.t
    -> ?count:int
    -> ?pattern:string
    -> Key.t
    -> (Cursor.t * Value.t list) Deferred.Or_error.t

  val zadd
    :  [< `Leader ] t
    -> Key.t
    -> ([ `Score of float ] * Value.t) list
    -> int Deferred.Or_error.t

  val zrem : [< `Leader ] t -> Key.t -> Value.t list -> int Deferred.Or_error.t

  val pttl
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> [ `Timeout of Time_ns.Span.t | `No_timeout | `No_key ] Deferred.Or_error.t

  val pexpire
    :  [< `Leader ] t
    -> Key.t
    -> ?nx:bool
    -> Time_ns.Span.t
    -> [ `Set | `Not_set ] Deferred.Or_error.t

  val pexpireat
    :  [< `Leader ] t
    -> Key.t
    -> ?nx:bool
    -> Time_ns.t
    -> [ `Set | `Not_set ] Deferred.Or_error.t

  val zcard : [< `Leader | `Replica ] t -> Key.t -> int Deferred.Or_error.t

  val zrange
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> min_index:int
    -> max_index:int
    -> Value.t list Deferred.Or_error.t

  val zscore
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Value.t
    -> [ `Score of float ] option Deferred.Or_error.t

  val zrank
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Value.t
    -> [ `Rank of int ] option Deferred.Or_error.t

  val zrangebylex
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> min:Value.t Maybe_bound.t
    -> max:Value.t Maybe_bound.t
    -> Value.t list Deferred.Or_error.t

  val zrangebyscore
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> Value.t list Deferred.Or_error.t

  val zrangebyscore_withscores
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> (Value.t * [ `Score of float ]) list Deferred.Or_error.t

  val zremrangebyscore
    :  [< `Leader ] t
    -> Key.t
    -> min_score:float Maybe_bound.t
    -> max_score:float Maybe_bound.t
    -> int Deferred.Or_error.t

  val hexists : [< `Leader | `Replica ] t -> Key.t -> Field.t -> bool Deferred.Or_error.t

  val hset
    :  [< `Leader ] t
    -> Key.t
    -> (Field.t * Value.t) list
    -> int Deferred.Or_error.t

  val hsetnx
    :  [< `Leader ] t
    -> Key.t
    -> (Field.t * Value.t) list
    -> int Deferred.Or_error.t

  val hget
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Field.t
    -> Value.t option Deferred.Or_error.t

  val hmget
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> Field.t list
    -> Value.t option list Deferred.Or_error.t

  val hgetall
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> (Field.t * Value.t) list Deferred.Or_error.t

  val hvals : [< `Leader | `Replica ] t -> Key.t -> Value.t list Deferred.Or_error.t
  val hkeys : [< `Leader | `Replica ] t -> Key.t -> Field.t list Deferred.Or_error.t
  val hlen : [< `Leader | `Replica ] t -> Key.t -> int Deferred.Or_error.t
  val hdel : [< `Leader ] t -> Key.t -> Field.t list -> int Deferred.Or_error.t

  val hscan
    :  [< `Leader | `Replica ] t
    -> cursor:Cursor.t
    -> ?count:int
    -> ?pattern:string
    -> Key.t
    -> (Cursor.t * (Field.t * Value.t) list) Deferred.Or_error.t

  val keyevent_notifications
    :  [< `Leader | `Replica ] t
    -> ([< Key_event.t ] as 'a) list
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val keyspace_notifications
    :  [< `Leader | `Replica ] t
    -> ([< Key_event.t ] as 'a) list
    -> [ `Patterns of string list | `Keys of Key.t list ]
    -> ('a * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val publish : [< `Leader | `Replica ] t -> string -> Key.t -> int Deferred.Or_error.t

  val subscribe
    :  [< `Leader | `Replica ] t
    -> string list
    -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val subscribe_raw
    :  [< `Leader | `Replica ] t
    -> [ `Literal of string list | `Pattern of string list ]
    -> consume:((read, Iobuf.seek) Iobuf.t -> subscription:string -> 'a)
    -> 'a Pipe.Reader.t Deferred.Or_error.t

  val psubscribe
    :  [< `Leader | `Replica ] t
    -> string list
    -> (string * Key.t) Pipe.Reader.t Deferred.Or_error.t

  val script_load : [< `Leader | `Replica ] t -> string -> Sha1.t Deferred.Or_error.t

  val evalsha
    :  [< `Leader | `Replica ] t
    -> Sha1.t
    -> Key.t list
    -> Value.t list
    -> Resp3.t Deferred.Or_error.t

  val raw_command
    :  [< `Leader | `Replica | `Sentinel ] t
    -> string list
    -> Resp3.t Deferred.Or_error.t

  val version : [< `Leader | `Replica | `Sentinel ] t -> string Deferred.Or_error.t
  val role : [< `Leader | `Replica | `Sentinel ] t -> Role.t Deferred.Or_error.t

  (** ACL and authentication commands.

      Read here for more:

      https://redis.io/docs/manual/security/acl/#command-categories
      https://redis.io/docs/manual/security/acl/#selectors
  *)
  val acl_setuser
    :  [< `Leader | `Replica | `Sentinel ] t
    -> username:string
    -> rules:string list
    -> unit Deferred.Or_error.t

  val acl_deluser
    :  [< `Leader | `Replica | `Sentinel ] t
    -> string list
    -> int Deferred.Or_error.t

  val acl_users : [< `Leader | `Replica | `Sentinel ] t -> string list Deferred.Or_error.t
  val acl_list : [< `Leader | `Replica | `Sentinel ] t -> string list Deferred.Or_error.t

  val acl_getuser
    :  [< `Leader | `Replica | `Sentinel ] t
    -> username:string
    -> Resp3.t Deferred.Or_error.t

  val auth
    :  [< `Leader | `Replica | `Sentinel ] t
    -> auth:Auth.t
    -> unit
    -> unit Deferred.Or_error.t

  (** Streams *)

  module Stream_range : sig
    type t =
      | Inclusive of Stream_id.t
      | Exclusive of Stream_id.t
  end

  val xlen : [< `Leader | `Replica ] t -> Key.t -> int Deferred.Or_error.t

  val xadd
    :  [< `Leader ] t
    -> Key.t
    -> ?stream_id:Stream_id.t
    -> (Field.t * Value.t) list
    -> Stream_id.t Deferred.Or_error.t

  val xgroup_create
    :  [< `Leader ] t
    -> Key.t
    -> Group.t
    -> ?stream_id:Stream_id.t
    -> ?mkstream:unit
    -> unit
    -> [ `Ok | `Already_exists ] Deferred.Or_error.t

  val xrange
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> ?start:Stream_range.t
    -> ?end_:Stream_range.t
    -> ?count:int
    -> unit
    -> (Stream_id.t * (Field.t * Value.t) list) list Deferred.Or_error.t

  val xrevrange
    :  [< `Leader | `Replica ] t
    -> Key.t
    -> ?end_:Stream_range.t
    -> ?start:Stream_range.t
    -> ?count:int
    -> unit
    -> (Stream_id.t * (Field.t * Value.t) list) list Deferred.Or_error.t

  val xreadgroup
    :  [< `Leader | `Replica ] t
    -> Group.t
    -> Consumer.t
    -> ?count:int
    -> ?block:[ `Don't_block | `Forever | `For_up_to of Time_ns.Span.t ]
    -> (Key.t * Stream_id.t option) list
    -> (Key.t * (Stream_id.t * (Field.t * Value.t) list) list) list Deferred.Or_error.t

  val xclaim
    :  [< `Leader ] t
    -> ?idle:Time_ns.Span.t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> min_idle_time:Time_ns.Span.t
    -> Stream_id.t list
    -> (Stream_id.t * (Field.t * Value.t) list) list Deferred.Or_error.t

  val xclaim_justid
    :  [< `Leader ] t
    -> ?idle:Time_ns.Span.t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> min_idle_time:Time_ns.Span.t
    -> Stream_id.t list
    -> Stream_id.t list Deferred.Or_error.t

  val xautoclaim
    :  [< `Leader ] t
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
    :  [< `Leader ] t
    -> Key.t
    -> Group.t
    -> Stream_id.t list
    -> int Deferred.Or_error.t

  val xinfo_consumers
    :  [< `Leader ] t
    -> Key.t
    -> Group.t
    -> [ `Ok of Consumer_info.t list | `No_such_key | `No_such_group ] Deferred.Or_error.t

  val xgroup_delconsumer
    :  [< `Leader ] t
    -> Key.t
    -> Group.t
    -> Consumer.t
    -> int Deferred.Or_error.t

  val xpending_extended
    :  [< `Leader ] t
    -> Key.t
    -> Group.t
    -> ?start:Stream_range.t
    -> ?end_:Stream_range.t
    -> count:int
    -> unit
    -> [ `Ok of Pending_info.Extended.t list | `No_such_stream_or_group ]
         Deferred.Or_error.t
end
