(** An ephemeral Redis instance that may be used for tests *)

open! Core
open  Async

(** Provide connection information to a sandboxed Redis server. The server will be reset
    as part of this function call. *)
val where_to_connect : unit -> Tcp.Where_to_connect.unix Deferred.Or_error.t

val run
  :  (module Redis.S with type t = 'a)
  -> ('a -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t

val version : string
