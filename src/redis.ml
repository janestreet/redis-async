open Core
open Async
open Common
open Deferred.Or_error.Let_syntax
module Auth = Auth
module Bulk_io = Bulk_io
module Consumer = Consumer
module Consumer_info = Consumer_info
module Cursor = Cursor
module Group = Group
module Key_event = Key_event
module Pending_info = Pending_info
module Resp3 = Resp3
module Role = Role
module Sentinel = Sentinel
module Sha1 = Sha1
module Stream_id = Stream_id

module type S = Redis_intf.S

type ('a, 'key, 'field, 'value) t = ('a, 'key, 'field, 'value) Client.t

module Make_field (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
struct
  module Key = Key
  module Field = Field
  module Value = Value
  include Client.Make (Key) (Field) (Value)

  let create' ?on_disconnect ?auth ~where_to_connect type_ =
    create ?on_disconnect ?auth ~where_to_connect type_
  ;;

  let create ?on_disconnect ?auth ~where_to_connect () =
    create' ?on_disconnect ?auth ~where_to_connect `Primary
  ;;

  let select t index = command_string t [ "SELECT"; itoa index ] (Response.create_ok ())
  let flushall t = command_string t [ "FLUSHALL" ] (Response.create_ok ())
  let flushdb t = command_string t [ "FLUSHDB" ] (Response.create_ok ())
  let shutdown t = command_string t [ "SHUTDOWN" ] (Response.create_ok ())

  let unlink t keys =
    command_key t ~result_of_empty_input:(Ok 0) [ "UNLINK" ] keys (Response.create_int ())
  ;;

  let dbsize t = command_string t [ "DBSIZE" ] (Response.create_int ())
  let exists t keys = command_key t [ "EXISTS" ] keys (Response.create_int ())
  let echo t k = command_key t [ "ECHO" ] [ k ] (Response.create Key_parser.single)
  let ping t arg = command_string t [ "PING"; arg ] (Response.create_string ())

  let wait t ~num_replicas ~timeout =
    let timeout =
      match timeout with
      | `Never -> "0"
      | `After span -> Time_ns.Span.to_int_ms span |> Int.to_string
    in
    command_string
      t
      [ "WAIT"; Int.to_string num_replicas; timeout ]
      (Response.create_int ())
  ;;

  let incr t k = command_key t [ "INCR" ] [ k ] (Response.create_int ())

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
      | None -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let pattern =
      match pattern with
      | None -> []
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
      | None -> []
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
    | n -> `Timeout (Time_ns.Span.of_int_ms n)
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
      [ key ]
      values
      (Response.create_int ())
  ;;

  let srem t key values =
    command_keys_values t [ "SREM" ] [ key ] values (Response.create_int ())
  ;;

  let sscan t ~cursor ?count ?pattern k =
    let count =
      match count with
      | None -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let pattern =
      match pattern with
      | None -> []
      | Some pattern -> [ "MATCH"; pattern ]
    in
    command_keys_string_args
      t
      [ "SSCAN" ]
      [ k ]
      ((Cursor.to_string cursor :: pattern) @ count)
      (Response.create Value_parser.cursor_and_list)
  ;;

  let zcard t key = command_key t [ "ZCARD" ] [ key ] (Response.create_int ())

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

  let zrank t k v =
    let%map rank =
      command_keys_values t [ "ZRANK" ] [ k ] [ v ] (Response.create_int_option ())
    in
    Option.map rank ~f:(fun rank -> `Rank rank)
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
      ~with_scores:false
      (Response.create Value_parser.list)
  ;;

  let zrangebyscore_withscores t key ~min_score ~max_score =
    command_key_score_range
      t
      [ "ZRANGEBYSCORE" ]
      key
      ~min_score
      ~max_score
      ~with_scores:true
      (Response.create Value_parser.with_scores)
  ;;

  let zremrangebyscore t key ~min_score ~max_score =
    command_key_score_range
      t
      [ "ZREMRANGEBYSCORE" ]
      key
      ~min_score
      ~max_score
      ~with_scores:false
      (Response.create_int ())
  ;;

  let hset t k fvs =
    command_keys_fields_and_values
      t
      ~result_of_empty_input:(Ok 0)
      [ "HSET" ]
      [ k ]
      []
      fvs
      (Response.create_int ())
  ;;

  let hsetnx t k fvs =
    command_keys_fields_and_values
      t
      ~result_of_empty_input:(Ok 0)
      [ "HSETNX" ]
      [ k ]
      []
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

  let hkeys t k = command_key t [ "HKEYS" ] [ k ] (Response.create Field_parser.list)
  let hlen t k = command_key t [ "HLEN" ] [ k ] (Response.create_int ())
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

  let hscan t ~cursor ?count ?pattern k =
    let count =
      match count with
      | None -> []
      | Some count -> [ "COUNT"; itoa count ]
    in
    let pattern =
      match pattern with
      | None -> []
      | Some pattern -> [ "MATCH"; pattern ]
    in
    command_keys_string_args
      t
      [ "HSCAN" ]
      [ k ]
      ((Cursor.to_string cursor :: pattern) @ count)
      (Response.create Field_value_map_parser.cursor_and_alternating_key_value)
  ;;

  let publish t channel key =
    command_key t [ "PUBLISH"; channel ] [ key ] (Response.create_int ())
  ;;

  let keyevent_configuration : Key_event.t -> char = function
    | `del -> 'g'
    | `expire -> 'g'
    | `new_ -> 'n'
    | `expired -> 'x'
    | `set -> '$'
    | `hset -> 'h'
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
    let keyspace_prefix = "__keyspace@0__:" in
    let keyspace_prefix_length = 15 in
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

  let auth t ~auth:{ Auth.username; password } () =
    let cmds = [ "AUTH"; username; password ] in
    command_string t cmds (Response.create_ok ())
  ;;

  let acl_setuser t ~username ~rules =
    command_string t ([ "ACL"; "SETUSER"; username ] @ rules) (Response.create_ok ())
  ;;

  let acl_deluser t = function
    | [] -> return 0
    | usernames ->
      command_string t ("ACL" :: "DELUSER" :: usernames) (Response.create_int ())
  ;;

  let acl_users t = command_string t [ "ACL"; "USERS" ] (Response.create_string_list ())
  let acl_list t = command_string t [ "ACL"; "LIST" ] (Response.create_string_list ())

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

  module Stream_range = struct
    type t =
      | Inclusive of Stream_id.t
      | Exclusive of Stream_id.t

    let start = function
      | None -> "-"
      | Some (Inclusive id) -> Stream_id.to_string id
      | Some (Exclusive id) -> "(" ^ Stream_id.to_string id
    ;;

    let end_ = function
      | None -> "+"
      | Some (Inclusive id) -> Stream_id.to_string id
      | Some (Exclusive id) -> "(" ^ Stream_id.to_string id
    ;;
  end

  let parse_stream_response buf =
    Resp3.expect_char buf '*';
    let len = Resp3.number buf in
    let result =
      List.init len ~f:(fun _ ->
        Resp3.expect_char buf '*';
        Resp3.expect_char buf '2';
        Resp3.expect_crlf buf;
        Resp3.expect_char buf '$';
        let id = Stream_id.of_string (Resp3.blob_string buf) in
        let%map.Or_error fvs = Field_value_map_parser.alternating_kv buf in
        id, fvs)
    in
    (* Or_error.all reverses the list! *)
    Or_error.all (List.rev result)
  ;;

  let parse_keyed_stream_response buf =
    match Resp3.consume_char buf with
    | '_' ->
      Resp3.expect_crlf buf;
      Ok []
    | '%' ->
      let len = Resp3.number buf in
      let result =
        List.init len ~f:(fun _ ->
          let%bind.Or_error key = Key_parser.single buf in
          let%map.Or_error stream_response = parse_stream_response buf in
          key, stream_response)
      in
      (* Or_error.all reverses the list! *)
      Or_error.all (List.rev result)
    | unexpected ->
      raise (Resp3.Protocol_error (sprintf "Expected %% or _ but observed %c" unexpected))
  ;;

  let xlen t key = command_key t [ "XLEN" ] [ key ] (Response.create_int ())

  let xadd t key ?stream_id fvlist =
    let stream_id = Option.value_map stream_id ~default:"*" ~f:Stream_id.to_string in
    let%map response =
      command_keys_fields_and_values
        t
        [ "XADD" ]
        [ key ]
        [ stream_id ]
        fvlist
        (Response.create_string ())
    in
    Stream_id.of_string response
  ;;

  let xgroup_create t key group ?stream_id ?mkstream () =
    let args =
      Group.to_string group
      :: Option.value_map stream_id ~default:"$" ~f:Stream_id.to_string
      :: List.filter_opt [ Option.map mkstream ~f:(fun () -> "MKSTREAM") ]
    in
    match%map.Deferred
      command_keys_string_args
        t
        [ "XGROUP"; "CREATE" ]
        [ key ]
        args
        (Response.create_resp3 ())
    with
    | Ok (String "OK") -> Ok `Ok
    | Ok (Error "BUSYGROUP Consumer Group name already exists") -> Ok `Already_exists
    | Ok (Error error) -> Or_error.error_string error
    | Error _ as error -> error
    | Ok unexpected -> raise_s [%message [%here] (unexpected : Resp3.t)]
  ;;

  let xrange t key ?start ?end_ ?count () =
    let start = Stream_range.start start in
    let end_ = Stream_range.end_ end_ in
    let args =
      match count with
      | None -> [ start; end_ ]
      | Some count -> [ start; end_; "COUNT"; Int.to_string count ]
    in
    command_keys_string_args
      t
      [ "XRANGE" ]
      [ key ]
      args
      (Response.create parse_stream_response)
  ;;

  let xrevrange t key ?end_ ?start ?count () =
    let end_ = Stream_range.end_ end_ in
    let start = Stream_range.start start in
    let args =
      match count with
      | None -> [ end_; start ]
      | Some count -> [ end_; start; "COUNT"; Int.to_string count ]
    in
    command_keys_string_args
      t
      [ "XREVRANGE" ]
      [ key ]
      args
      (Response.create parse_stream_response)
  ;;

  let xreadgroup t group consumer ?count ?(block = `Don't_block) streams =
    let count =
      Option.value_map count ~default:[] ~f:(fun count ->
        [ "COUNT"; Int.to_string count ])
    in
    let block =
      match block with
      | `Don't_block -> []
      | `Forever -> [ "BLOCK"; "0" ]
      | `For_up_to block -> [ "BLOCK"; Int.to_string (Time_ns.Span.to_int_ms block) ]
    in
    command_keys_string_args
      t
      ([ "XREADGROUP"; "GROUP"; Group.to_string group; Consumer.to_string consumer ]
       @ count
       @ block
       @ [ "STREAMS" ])
      (List.map streams ~f:fst)
      (List.map streams ~f:(fun (_, id) ->
         Option.value_map id ~default:">" ~f:Stream_id.to_string))
      (Response.create parse_keyed_stream_response)
  ;;

  let xclaim_command t ?idle key group consumer ~min_idle_time streams ~trailing ~response
    =
    let idle =
      match idle with
      | None -> []
      | Some idle -> [ "IDLE"; Int.to_string (Time_ns.Span.to_int_ms idle) ]
    in
    command_keys_string_args
      t
      [ "XCLAIM" ]
      [ key ]
      ([ Group.to_string group
       ; Consumer.to_string consumer
       ; Int.to_string (Time_ns.Span.to_int_ms min_idle_time)
       ]
       @ List.map streams ~f:Stream_id.to_string
       @ idle
       @ trailing)
      response
  ;;

  let xclaim t ?idle key group consumer ~min_idle_time streams =
    xclaim_command
      t
      ?idle
      key
      group
      consumer
      ~min_idle_time
      ~trailing:[]
      ~response:(Response.create parse_stream_response)
      streams
  ;;

  let xclaim_justid t ?idle key group consumer ~min_idle_time streams =
    let%map sl =
      xclaim_command
        t
        ?idle
        key
        group
        consumer
        ~min_idle_time
        streams
        ~trailing:[ "JUSTID" ]
        ~response:(Response.create_string_list ())
    in
    List.map ~f:Stream_id.of_string sl
  ;;

  let xautoclaim t key group consumer ~min_idle_time ~start ?count () =
    let count =
      Option.value_map count ~default:[] ~f:(fun count ->
        [ "COUNT"; Int.to_string count ])
    in
    command_keys_string_args
      t
      [ "XAUTOCLAIM" ]
      [ key ]
      ([ Group.to_string group
       ; Consumer.to_string consumer
       ; Int.to_string (Time_ns.Span.to_int_ms min_idle_time)
       ; Stream_id.to_string start
       ]
       @ count)
      (Response.create (fun buf ->
         Resp3.expect_char buf '*';
         Resp3.expect_char buf '3';
         Resp3.expect_crlf buf;
         Resp3.expect_char buf '$';
         let next_stream_id = Stream_id.of_string (Resp3.blob_string buf) in
         let%map.Or_error stream_response = parse_stream_response buf in
         Resp3.expect_char buf '*';
         let len = Resp3.number buf in
         let no_longer_exist =
           List.init len ~f:(fun _ -> Stream_id.of_string (Resp3.simple_string buf))
         in
         `Next_stream_id next_stream_id, stream_response, `No_longer_exist no_longer_exist))
  ;;

  let xack t key group ids =
    command_keys_string_args
      t
      [ "XACK" ]
      [ key ]
      (Group.to_string group :: List.map ids ~f:Stream_id.to_string)
      (Response.create_int ())
  ;;

  let xinfo_consumers t key group =
    match%bind
      command_keys_string_args
        t
        [ "XINFO"; "CONSUMERS" ]
        [ key ]
        [ Group.to_string group ]
        (Response.create_resp3 ())
    with
    | Resp3.(Array consumer_infos) ->
      let%map consumer_infos =
        Array.to_list consumer_infos
        |> List.map ~f:Consumer_info.of_resp3
        |> Or_error.all
        |> Deferred.return
      in
      `Ok consumer_infos
    | Error err when String.is_prefix err ~prefix:"NOGROUP" -> return `No_such_group
    | Error "ERR no such key" -> return `No_such_key
    | resp3 ->
      Deferred.Or_error.error_s
        [%message [%here] "Unexpected response to xinfo consumers" (resp3 : Resp3.t)]
  ;;

  let xgroup_delconsumer t key group consumer =
    command_keys_string_args
      t
      [ "XGROUP"; "DELCONSUMER" ]
      [ key ]
      [ Group.to_string group; Consumer.to_string consumer ]
      (Response.create_int ())
  ;;

  let xpending_extended t key group ?start ?end_ ~count () =
    let start = Stream_range.start start in
    let end_ = Stream_range.end_ end_ in
    match%map.Deferred
      command_keys_string_args
        t
        [ "XPENDING" ]
        [ key ]
        [ Group.to_string group; start; end_; Int.to_string count ]
        (Response.create_resp3 ())
    with
    | Ok (Array arr) ->
      (match
         Array.partition_map arr ~f:(fun resp3 ->
           match Pending_info.Extended.of_resp3 resp3 with
           | Ok pi -> First pi
           | Error err -> Second err)
       with
       | ok, [||] -> Ok (`Ok (Array.to_list ok))
       | _, errs -> Error (Error.of_list (Array.to_list errs)))
    | Ok (Error err) when String.is_prefix err ~prefix:"NOGROUP" ->
      Ok `No_such_stream_or_group
    | Error _ as error -> error
    | Ok resp3 ->
      raise_s
        [%message [%here] "Unexpected response to xpending_extended" (resp3 : Resp3.t)]
  ;;
end

module Make (Key : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
  Make_field (Key) (Bulk_io.String) (Value)

module String = Make (Bulk_io.String) (Bulk_io.String)
