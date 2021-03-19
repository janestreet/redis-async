open Core
open Async
open Common
open Deferred.Or_error.Let_syntax
module Bulk_io = Bulk_io
module Resp3   = Resp3

module type S = Redis_intf.S

module Make (Key : Bulk_io_intf.S) (Value : Bulk_io_intf.S) = struct
  module Key   = Key
  module Value = Value
  include Client.Make (Key) (Value)

  type t = Key.t Client.t

  let select t index = command_string t [ "SELECT"; itoa index ] (Response.create_ok ())
  let flushall t     = command_string t [ "FLUSHALL" ] (Response.create_ok ())
  let flushdb t      = command_string t [ "FLUSHDB" ] (Response.create_ok ())
  let shutdown t     = command_string t [ "SHUTDOWN" ] (Response.create_ok ())
  let set t k v      = command_kv t [ "SET" ] [ k, v ] (Response.create_ok ())
  let setnx t k v    = command_kv t [ "SETNX" ] [ k, v ] (Response.create_01_bool ())
  let get t k        = command_key t [ "GET" ] [ k ] (Response.create Value_parser.single_opt)
  let echo t k       = command_key t [ "ECHO" ] [ k ] (Response.create Key_parser.single)
  let unlink t keys  = command_key t [ "UNLINK" ] keys (Response.create_int ())
  let dbsize t       = command_string t [ "DBSIZE" ] (Response.create_int ())
  let exists t keys  = command_key t [ "EXISTS" ] keys (Response.create_int ())

  let mset t kvs =
    command_kv t ~result_of_empty_input:(Ok ()) [ "MSET" ] kvs (Response.create_ok ())
  ;;

  let mget t keys =
    command_key
      t
      ~result_of_empty_input:(Ok [])
      [ "MGET" ]
      keys
      (Response.create Value_parser.list_opt)
  ;;

  let del t keys =
    command_key t ~result_of_empty_input:(Ok 0) [ "DEL" ] keys (Response.create_int ())
  ;;

  let smembers t key =
    command_key
      t
      ~result_of_empty_input:(Ok [])
      [ "SMEMBERS" ]
      [ key ]
      (Response.create Value_parser.set)
  ;;

  let sadd t key values =
    command_keys_values t [ "SADD" ] [ key ] values (Response.create_int ())
  ;;

  let srem t key values =
    command_keys_values t [ "SREM" ] [ key ] values (Response.create_int ())
  ;;

  let keys ?(pattern = "*") t =
    command_string t [ "KEYS"; pattern ] (Response.create Key_parser.list)
  ;;

  let scan t ~cursor ?count () =
    let count =
      match count with
      | None       -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let%map i, l =
      command_string
        t
        ("SCAN" :: itoa cursor :: count)
        (Response.create Key_parser.int_and_list)
    in
    `Cursor i, l
  ;;
end
