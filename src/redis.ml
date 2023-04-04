open Core
open Async
open Common
open Deferred.Or_error.Let_syntax
module Bulk_io   = Bulk_io
module Resp3     = Resp3
module Key_event = Key_event
module Cursor    = Cursor
module Role      = Role
module Auth      = Auth

module type S = Redis_intf.S

module Make_field (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
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
  let ping   t arg  = command_string t [ "PING"; arg ] (Response.create_string ())
  let incr   t k    = command_key t [ "INCR" ] [ k ] (Response.create_int ())

  let del t keys =
    command_key t ~result_of_empty_input:(Ok 0) [ "DEL" ] keys (Response.create_int ())
  ;;

  let keys ?(pattern = "*") t =
    command_string t [ "KEYS"; pattern ] (Response.create Key_parser.list)
  ;;

  let rename t k ~new_key =
    command_key t [ "RENAME" ] [ k; new_key ] (Response.create_ok ())
  ;;

  let scan t ~cursor ?count ?pattern () =
    let count =
      match count with
      | None       -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let pattern =
      match pattern with
      | None         -> []
      | Some pattern -> [ "MATCH"; pattern ]
    in
    command_string
      t
      (("SCAN" :: Cursor.to_string cursor :: count) @ pattern)
      (Response.create Key_parser.cursor_and_list)
  ;;

  let set t k ?expire v =
    let args =
      match expire with
      | None        -> []
      | Some expire -> [ "PX"; Int.to_string (Time_ns.Span.to_int_ms expire) ]
    in
    command_kv t [ "SET" ] [ k, v ] args (Response.create_ok ())
  ;;

  let setnx t k v = command_kv t [ "SETNX" ] [ k, v ] [] (Response.create_01_bool ())

  let pexpire t key ?(nx = false) span =
    let nx = if nx then [ "NX" ] else [] in
    match%map
      command_keys_string_args
        t
        [ "PEXPIRE" ]
        [ key ]
        (Int.to_string (Time_ns.Span.to_int_ms span) :: nx)
        (Response.create_int ())
    with
    | 1 -> `Set
    | 0 -> `Not_set
    | n -> raise_s [%message [%here] "Unexpected response" (n : int)]
  ;;

  let pexpireat t key ?(nx = false) time =
    let nx = if nx then [ "NX" ] else [] in
    match%map
      command_keys_string_args
        t
        [ "PEXPIREAT" ]
        [ key ]
        (Int.to_string (Time_ns.to_span_since_epoch time |> Time_ns.Span.to_int_ms) :: nx)
        (Response.create_int ())
    with
    | 1 -> `Set
    | 0 -> `Not_set
    | n -> raise_s [%message [%here] "Unexpected response" (n : int)]
  ;;

  let pttl t key =
    match%map command_key t [ "PTTL" ] [ key ] (Response.create_int ()) with
    | -2 -> `No_key
    | -1 -> `No_timeout
    | n  -> `Timeout (Time_ns.Span.of_int_ms n)
  ;;

  let msetnx t kvs =
    (* This atomically sets all the keys/values as long as none of the keys already exist.
       For an empty set of key-values, we always succeed (vacuous truth), so the result
       of empty input should be true. *)
    command_kv
      t
      ~result_of_empty_input:(Ok true)
      [ "MSETNX" ]
      kvs
      []
      (Response.create_01_bool ())
  ;;

  let get t k = command_key t [ "GET" ] [ k ] (Response.create Value_parser.single_opt)

  let mset t kvs =
    command_kv t ~result_of_empty_input:(Ok ()) [ "MSET" ] kvs [] (Response.create_ok ())
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

  let zscore t k v =
    let%map score =
      command_keys_values t [ "ZSCORE" ] [ k ] [ v ] (Response.create_float_option ())
    in
    Option.map score ~f:(fun score -> `Score score)
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

  let zrangebyscore t key ~min_score ~max_score =
    command_key_score_range
      t
      [ "ZRANGEBYSCORE" ]
      key
      ~min_score
      ~max_score
      (Response.create Value_parser.list)
  ;;

  let zremrangebyscore t key ~min_score ~max_score =
    command_key_score_range
      t
      [ "ZREMRANGEBYSCORE" ]
      key
      ~min_score
      ~max_score
      (Response.create_int ())
  ;;

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

  let hexists t k f =
    command_keys_fields t [ "HEXISTS" ] [ k ] [ f ] (Response.create_01_bool ())
  ;;

  let hgetall t k =
    command_key t [ "HGETALL" ] [ k ] (Response.create Field_value_map_parser.map)
  ;;

  let hkeys t k = command_key t [ "HKEYS" ] [ k ] (Response.create     Field_parser.list)
  let hlen  t k = command_key t [ "HLEN"  ] [ k ] (Response.create_int ()               )
  let hvals t k = command_key t [ "HVALS" ] [ k ] (Response.create     Value_parser.list)

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
    command_keys_string_args
      t
      [ "HSCAN" ]
      [ k ]
      (Cursor.to_string cursor :: count)
      (Response.create Field_value_map_parser.cursor_and_alternating_key_value)
  ;;

  let publish t channel key =
    command_key t [ "PUBLISH"; channel ] [ key ] (Response.create_int ())
  ;;

  let keyevent_configuration : Key_event.t -> char = function
    | `del     -> 'g'
    | `expire  -> 'g'
    | `new_    -> 'n'
    | `expired -> 'x'
    | `set     -> '$'
    | `hset    -> 'h'
  ;;

  let keyspace_setup t category events =
    let events = List.dedup_and_sort events ~compare:Poly.compare in
    (* Merge any existing keyspace event configuration with what is being requested in
       this invocation *)
    let%bind existing_configuration =
      match%map
        command_string
          t
          [ "CONFIG"; "GET"; "notify-keyspace-events" ]
          (Response.create_resp3 ())
      with
      | Resp3.(Map [| (String "notify-keyspace-events", String v) |]) -> v
      | resp3 ->
        raise_s [%message [%here] "Unexpected response to config get" (resp3 : Resp3.t)]
    in
    let configuration =
      (* Redis sets flags based on this string and does not care about duplicate characters *)
      String.of_char_list
        (category :: List.map (events :> Key_event.t list) ~f:keyevent_configuration)
      ^ existing_configuration
    in
    let%map () =
      command_string
        t
        [ "CONFIG"; "SET"; "notify-keyspace-events"; configuration ]
        (Response.create_ok ())
    in
    (* Transform the string events we may receive back into their variant
       representation. *)
    String.Map.of_alist_exn
      (List.map events ~f:(fun event -> Key_event.to_string (event :> Key_event.t), event))
  ;;

  let map_events_in_reader reader lookup =
    Pipe.map' reader ~f:(fun raw_events ->
      Deferred.return
        (Queue.filter_map raw_events ~f:(fun (raw_event, key) ->
           let%map.Option event = Map.find lookup raw_event in
           event, key)))
  ;;

  let keyevent_notifications t events =
    let%bind lookup = keyspace_setup t 'E' events in
    let%map reader =
      subscribe_raw
        t
        (`Literal
           (List.map events ~f:(fun event ->
              "__keyevent@0__:" ^ Key_event.to_string (event :> Key_event.t))))
        ~consume:(fun buf ~subscription ->
          String.drop_prefix subscription 15, Or_error.ok_exn (Key_parser.single buf))
    in
    map_events_in_reader reader lookup
  ;;

  let keyspace_notifications t events targets =
    let keyspace_prefix        = "__keyspace@0__:" in
    let keyspace_prefix_length = 15                in
    let%bind lookup = keyspace_setup t 'K' events in
    let targets =
      match targets with
      | `Patterns patterns ->
        `Pattern (List.map patterns ~f:(fun pattern -> keyspace_prefix ^ pattern))
      | `Keys keys ->
        `Literal
          (List.map keys ~f:(fun key -> keyspace_prefix ^ Key.Redis_bulk_io.to_string key))
    in
    let%map reader =
      subscribe_raw t targets ~consume:(fun buf ~subscription ->
        let key =
          match targets with
          | `Pattern _ ->
            Parse_bulk.apply_single buf ~f:(fun ~len buf ->
              Common.check_length_exn buf ~len:keyspace_prefix_length;
              Iobuf.unsafe_advance buf keyspace_prefix_length;
              Key.Redis_bulk_io.consume buf ~len:(len - keyspace_prefix_length))
            |> Or_error.ok_exn
          | `Literal _ ->
            Key.Redis_bulk_io.of_string
              (String.drop_prefix subscription keyspace_prefix_length)
        in
        Resp3.expect_char buf '$';
        let raw_event = Resp3.blob_string buf in
        raw_event, key)
    in
    map_events_in_reader reader lookup
  ;;

  let script_load t lua =
    let%map sha1 =
      command_string t [ "SCRIPT"; "LOAD"; lua ] (Response.create_string ())
    in
    Sha1.of_string sha1
  ;;

  let evalsha t sha1 keys values =
    command_keys_values
      t
      [ "EVALSHA"; Sha1.to_string sha1; Int.to_string (List.length keys) ]
      keys
      values
      (Response.create_resp3 ())
  ;;

  let subscribe t subscriptions =
    subscribe_raw t (`Literal subscriptions) ~consume:(fun buf ~subscription ->
      subscription, Or_error.ok_exn (Key_parser.single buf))
  ;;

  let psubscribe t subscriptions =
    subscribe_raw t (`Pattern subscriptions) ~consume:(fun buf ~subscription:_ ->
      Resp3.expect_char buf '$';
      let channel = Resp3.blob_string buf in
      channel, Or_error.ok_exn (Key_parser.single buf))
  ;;

  let role t = command_string t [ "ROLE" ] (Response.create_role ())

  let sentinel_leader t name =
    command_string
      t
      [ "SENTINEL"; "GET-MASTER-ADDR-BY-NAME"; name ]
      (Response.create_host_and_port ())
  ;;

  let auth t ~auth:{ Auth.username; password } () =
    let cmds = [ "AUTH"; username; password ] in
    command_string t cmds (Response.create_ok ())
  ;;

  let acl_setuser t ~username ~rules =
    command_string t ([ "ACL"; "SETUSER"; username ] @ rules) (Response.create_ok ())
  ;;

  let acl_deluser t = function
    | []        -> return 0
    | usernames ->
      command_string t ("ACL" :: "DELUSER" :: usernames) (Response.create_int ())
  ;;

  let acl_users t = command_string t [ "ACL"; "USERS" ] (Response.create_string_list ())
  let acl_list  t = command_string t [ "ACL"; "LIST"  ] (Response.create_string_list ())

  let acl_getuser t ~username =
    command_string t [ "ACL"; "GETUSER"; username ] (Response.create_resp3 ())
  ;;

  let raw_command t commands = command_string t commands (Response.create_resp3 ())

  (** Interpret the result of the INFO command which does not use Resp3 *)
  let parse_info str =
    String.split_lines str
    |> List.filter_map ~f:(String.lsplit2 ~on:':')
    |> String.Map.of_alist_or_error
  ;;

  let version t =
    match%bind raw_command t [ "INFO"; "SERVER" ] with
    | Resp3.String server_info ->
      Deferred.return
        (let%bind.Or_error parsed_info = parse_info server_info in
         Map.find_or_error parsed_info "redis_version")
    | unexpected ->
      Deferred.Or_error.error_s
        [%message
          [%here]
            "Unexpected response while checking the Redis version"
            (unexpected : Resp3.t)]
  ;;
end

module Make (Key : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
  Make_field (Key) (Bulk_io.String) (Value)
