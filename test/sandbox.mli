(** An ephemeral Redis instance that may be used for tests *)

open! Core
open  Async

(** Provide connection information to a sandboxed Redis leader. The server will be reset
    as part of this function call. *)
val where_to_connect
  :  unit
  -> (Tcp.Where_to_connect.unix * Tcp.Where_to_connect.inet) Deferred.Or_error.t

(** Provide connection information to a sandboxed, readonly Redis replica. The server will
    be reset as a part of this function call. *)
val where_to_connect_replica
  :  unit
  -> (Tcp.Where_to_connect.unix * Tcp.Where_to_connect.inet) Deferred.Or_error.t

(** Provide connection information to a sandboxed Redis Sentinel connected to a leader
    with one replica. The server will be reset as a part of this function call. *)
val where_to_connect_sentinel
  :  unit
  -> (string * Tcp.Where_to_connect.inet) Deferred.Or_error.t

(** Runs a command on a leader node *)
val run
  :  (module Redis.S with type t = 'a)
  -> ('a -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

(** Runs a command on a read-only replica *)
val run_replica
  :  (module Redis.S with type t = 'a)
  -> ('a -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

(** Runs on a cluster of three Redis nodes:
    1. A Sentinel
    2. A leader Replica
    3. A follower Replica *)
val run_sentinel
  :  (module Redis.S with type t = 'a)
  -> ('a -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

(** Teardown the sandbox *)
val teardown : ?on_disconnect:(unit -> unit) -> unit -> unit Deferred.Or_error.t

val version  : string
