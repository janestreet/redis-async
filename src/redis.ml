open Core
open Async
open Common
open Deferred.Or_error.Let_syntax
module Bulk_io = Bulk_io
module Resp3   = Resp3

module type S_generic = Redis_intf.S_generic
module type S         = Redis_intf.S
module type S_hash    = Redis_intf.S_hash

module Make_generic
    (Key : Bulk_io_intf.S)
    (Field : Bulk_io_intf.S)
    (Value : Bulk_io_intf.S) =
struct
  module Key   = Key
  module Field = Field
  module Value = Value
  include Client.Make (Key) (Field) (Value)

  type t = Key.t Client.t

  let select   t index = command_string t [ "SELECT"; itoa index ] (Response.create_ok ())
  let flushall t       = command_string t [ "FLUSHALL"           ] (Response.create_ok ())
  let flushdb  t       = command_string t [ "FLUSHDB"            ] (Response.create_ok ())
  let shutdown t       = command_string t [ "SHUTDOWN"           ] (Response.create_ok ())

  let unlink t keys =
    command_key t ~result_of_empty_input:(Ok 0) [ "UNLINK" ] keys (Response.create_int ())
  ;;

  let dbsize t      = command_string t [ "DBSIZE" ] (Response.create_int ())
  let exists t keys = command_key t [ "EXISTS" ] keys (Response.create_int ())
  let echo   t k    = command_key t [ "ECHO" ] [ k ] (Response.create Key_parser.single)

  let del t keys =
    command_key t ~result_of_empty_input:(Ok 0) [ "DEL" ] keys (Response.create_int ())
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

module Make (Key : Bulk_io_intf.S) (Value : Bulk_io_intf.S) = struct
  include Make_generic (Key) (Bulk_io.String) (Value)

  let set   t k v = command_kv t [ "SET"   ] [ k, v ] (Response.create_ok      ())
  let setnx t k v = command_kv t [ "SETNX" ] [ k, v ] (Response.create_01_bool ())

  let msetnx t kvs =
    (* This atomically sets all the keys/values as long as none of the keys already exist.
       For an empty set of key-values, we always succeed (vacuous truth), so the result
       of empty input should be true. *)
    command_kv
      t
      ~result_of_empty_input:(Ok true)
      [ "MSETNX" ]
      kvs
      (Response.create_01_bool ())
  ;;

  let get t k = command_key t [ "GET" ] [ k ] (Response.create Value_parser.single_opt)

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

  let smembers t key =
    command_key
      t
      ~result_of_empty_input:(Ok [])
      [ "SMEMBERS" ]
      [ key ]
      (Response.create Value_parser.set)
  ;;

  let sismember t key value =
    command_keys_values t [ "SISMEMBER" ] [ key ] [ value ] (Response.create_01_bool ())
  ;;

  let smismember t key values =
    command_keys_values
      t
      ~result_of_empty_input:(Ok [])
      [ "SMISMEMBER" ]
      [ key ]
      values
      (Response.create_01_bool_list ())
  ;;

  let sadd t key values =
    command_keys_values
      t
      ~result_of_empty_input:(Ok 0)
      [ "SADD" ]
      [ key    ]
      values
      (Response.create_int ())
  ;;

  let srem t key values =
    command_keys_values t [ "SREM" ] [ key ] values (Response.create_int ())
  ;;

  let zadd t key values =
    command_key_scores_values
      t
      ~result_of_empty_input:(Ok 0)
      [ "ZADD" ]
      key
      values
      (Response.create_int ())
  ;;

  let zrange t key ~min_index ~max_index =
    command_key_range
      t
      [ "ZRANGE" ]
      key
      ~min_index
      ~max_index
      (Response.create Value_parser.list)
  ;;

  let zrem t key values =
    command_keys_values t [ "ZREM" ] [ key ] values (Response.create_int ())
  ;;

  let zrangebylex t key ~min ~max =
    command_key_lex_range
      t
      [ "ZRANGEBYLEX" ]
      key
      ~min
      ~max
      (Response.create Value_parser.list)
  ;;
end

module Make_hash (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
struct
  include Make_generic (Key) (Field) (Value)

  let hset t k fvs =
    command_keys_fields_and_values
      t
      ~result_of_empty_input:(Ok 0)
      [ "HSET" ]
      [ k ]
      fvs
      (Response.create_int ())
  ;;

  let hget t k f =
    command_keys_fields t [ "HGET" ] [ k ] [ f ] (Response.create Value_parser.single_opt)
  ;;

  let hmget t k fs =
    command_keys_fields
      t
      ~result_of_empty_input:(Ok [])
      [ "HMGET" ]
      [ k ]
      fs
      (Response.create Value_parser.list_opt)
  ;;

  let hgetall t k =
    command_key t [ "HGETALL" ] [ k ] (Response.create Field_value_map_parser.map)
  ;;

  let hkeys t k = command_key t [ "HKEYS" ] [ k ] (Response.create Field_parser.list)
  let hvals t k = command_key t [ "HVALS" ] [ k ] (Response.create Value_parser.list)

  let hdel t k fs =
    command_keys_fields
      t
      ~result_of_empty_input:(Ok 0)
      [ "HDEL" ]
      [ k ]
      fs
      (Response.create_int ())
  ;;

  let hscan t ~cursor ?count k =
    let count =
      match count with
      | None       -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let%map i, l =
      command_keys_string_args
        t
        [ "HSCAN" ]
        [ k ]
        (itoa cursor :: count)
        (Response.create Field_value_map_parser.int_and_alternating_key_value)
    in
    `Cursor i, l
  ;;
end
