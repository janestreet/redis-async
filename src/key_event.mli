(** Key events that you may ask Redis to inform you of
    https://redis.io/docs/latest/develop/pubsub/keyspace-notifications/

    This variant is incomplete and is intended to be added to as needed. *)

open! Core

type t =
  [ `del (** DEL generates a del event for every deleted key. *)
  | `expire
    (** EXPIRE and all its variants (PEXPIRE, EXPIREAT, PEXPIREAT) generate an expire
        event when called with a positive timeout (or a future timestamp). Note that when
        these commands are called with a negative timeout value or timestamp in the past,
        the key is deleted and only a del event is generated instead. *)
  | `new_ (** Every time a new key is added to the data set, a new event is generated. *)
  | `expired
    (** Every time a key with a time to live associated is removed from the data set
        because it expired, an expired event is generated. *)
  | `set
    (** Every time set, setex, setnx, or getset is called, a set event is generated. *)
  | `hset (** Every time hset, hsetnx, or hmset is called, an hset event is generated. *)
  | `incrby (** incr, decr, incrby, decrby *)
  ]
[@@deriving sexp_of, enumerate, string]
