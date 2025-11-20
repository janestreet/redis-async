open Core
open Async
open Common

let disconnect_message = "Disconnected from Redis: see server logs for detail"

(** Represents a subscriber to a pub/sub message where deserialization and result type are
    specific to the individual subscriber. *)
type subscriber =
  | Subscriber :
      { writer : 'a Pipe.Writer.t
      ; consume : (read, Iobuf.seek, Iobuf.global) Iobuf.t -> subscription:string -> 'a
      }
      -> subscriber

(** This is a table from pub/sub channel to a list of subscribers. Each channel can have
    multiple subscribers, but that is represented by a single redis subscription that is

    fanned out.

    If the list of subscribers for a channel is empty, that means we issued an unsubscribe
    command to redis. *)
type subscription_table = subscriber list String.Table.t

type ('a, 'key, 'field, 'value) t =
  { pending_response : (module Response_intf.S) Queue.t
  ; reader : Reader.t
  ; writer : Writer.t
  ; mutable invalidations :
      (([ `All | `Key of 'key ] -> unit) Bus.Read_write.t * bool) option
  ; subscriptions : subscription_table
  ; pattern_subscriptions : subscription_table
  }

module Make (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
struct
  module Key_parser = Parse_bulk.Make (Key)
  module Field_parser = Parse_bulk.Make (Field)
  module Value_parser = Parse_bulk.Make (Value)
  module Field_value_map_parser = Parse_bulk.Make_map (Field_parser) (Value_parser)

  type nonrec 'a t = ('a, Key.t, Field.t, Value.t) t

  let write_array_header writer len =
    Writer.write_char writer '*';
    Writer.write writer (itoa len);
    write_crlf writer
  ;;

  let write_array_el
    (type w)
    writer
    (module IO : Bulk_io_intf.S with type t = w)
    ?(prefix = "")
    el
    =
    let len = IO.Redis_bulk_io.length el in
    Writer.write_char writer '$';
    Writer.write writer (len + String.length prefix |> itoa);
    write_crlf writer;
    Writer.write writer prefix;
    IO.Redis_bulk_io.write ~len writer el;
    write_crlf writer
  ;;

  let with_writer t f =
    if Writer.is_closed t.writer
    then Deferred.Or_error.error_string disconnect_message
    else f t.writer
  ;;

  let command_key
    (type r)
    t
    ?result_of_empty_input
    cmds
    args
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty args -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header writer (List.length cmds + List.length args);
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter args ~f:(fun arg -> write_array_el writer (module Key) arg);
        Ivar.read R.this)
  ;;

  let command_keys_args
    (type r a)
    t
    ?result_of_empty_input
    cmds
    key_args
    args
    (module Arg : Bulk_io_intf.S with type t = a)
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty key_args || List.is_empty args -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header
          writer
          (List.length cmds + List.length key_args + List.length args);
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter key_args ~f:(fun arg -> write_array_el writer (module Key) arg);
        List.iter args ~f:(fun arg -> write_array_el writer (module Arg) arg);
        Ivar.read R.this)
  ;;

  let command_key_string_args_fields
    (type r)
    t
    ?result_of_empty_input
    cmds
    key_arg
    args
    field_args
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty args || List.is_empty field_args -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header
          writer
          (List.length cmds + 1 + List.length args + List.length field_args);
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        write_array_el writer (module Key) key_arg;
        List.iter args ~f:(fun arg -> write_array_el writer (module Bulk_io.String) arg);
        List.iter field_args ~f:(fun field -> write_array_el writer (module Field) field);
        Ivar.read R.this)
  ;;

  let command_keys_values t ?result_of_empty_input cmds key_args value_args response =
    command_keys_args
      t
      ?result_of_empty_input
      cmds
      key_args
      value_args
      (module Value)
      response
  ;;

  let command_keys_fields t ?result_of_empty_input cmds key_args field_args response =
    command_keys_args
      t
      ?result_of_empty_input
      cmds
      key_args
      field_args
      (module Field)
      response
  ;;

  let command_keys_string_args t ?result_of_empty_input cmds key_args args response =
    command_keys_args
      t
      ?result_of_empty_input
      cmds
      key_args
      args
      (module Bulk_io.String)
      response
  ;;

  let command_keys_fields_and_values
    (type r)
    t
    ?result_of_empty_input
    cmds
    key_args
    args
    fields_and_value_args
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty key_args || List.is_empty fields_and_value_args ->
      return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header
          writer
          (List.length cmds
           + List.length key_args
           + List.length args
           + (List.length fields_and_value_args * 2));
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter key_args ~f:(fun arg -> write_array_el writer (module Key) arg);
        List.iter args ~f:(fun arg -> write_array_el writer (module Bulk_io.String) arg);
        List.iter fields_and_value_args ~f:(fun (field, value) ->
          write_array_el writer (module Field) field;
          write_array_el writer (module Value) value);
        Ivar.read R.this)
  ;;

  let command_string (type r) t cmds (module R : Response_intf.S with type t = r) =
    command_key t cmds [] (module R)
  ;;

  let command_kv
    (type r)
    t
    ?result_of_empty_input
    cmds
    alist
    args
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty alist -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header
          writer
          (List.length cmds + (List.length alist * 2) + List.length args);
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter alist ~f:(fun (key, value) ->
          write_array_el writer (module Key) key;
          write_array_el writer (module Value) value);
        List.iter args ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        Ivar.read R.this)
  ;;

  let command_key_scores_values
    (type r)
    t
    ?result_of_empty_input
    cmds
    key
    alist
    (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty alist -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header writer (List.length cmds + 1 + (List.length alist * 2));
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        write_array_el writer (module Key) key;
        List.iter alist ~f:(fun (`Score score, value) ->
          write_array_el writer (module Bulk_io.Float) score;
          write_array_el writer (module Value) value);
        Ivar.read R.this)
  ;;

  let command_key_range
    (type r)
    t
    cmds
    key
    ~min_index
    ~max_index
    (module R : Response_intf.S with type t = r)
    =
    with_writer t (fun writer ->
      Queue.enqueue t.pending_response (module R);
      write_array_header writer (List.length cmds + 3);
      List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
      write_array_el writer (module Key) key;
      write_array_el writer (module Bulk_io.Int) min_index;
      write_array_el writer (module Bulk_io.Int) max_index;
      Ivar.read R.this)
  ;;

  let command_key_bounded_range
    (type r s)
    t
    cmds
    key
    ~min
    ~max
    ~infinity_min
    ~infinity_max
    ~with_scores
    ~incl_prefix
    (module R : Response_intf.S with type t = r)
    (module Value : Bulk_io.S with type t = s)
    =
    with_writer t (fun writer ->
      let write_bound bound infinite_symbol =
        match bound with
        | Unbounded -> write_array_el writer (module Bulk_io.String) infinite_symbol
        | Incl value -> write_array_el writer (module Value) value ~prefix:incl_prefix
        | Excl value -> write_array_el writer (module Value) value ~prefix:"("
      in
      Queue.enqueue t.pending_response (module R);
      let extra_cmds_len = if with_scores then 4 else 3 in
      write_array_header writer (List.length cmds + extra_cmds_len);
      List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
      write_array_el writer (module Key) key;
      write_bound min infinity_min;
      write_bound max infinity_max;
      if with_scores then write_array_el writer (module Bulk_io.String) "WITHSCORES";
      Ivar.read R.this)
  ;;

  let command_key_score_range
    (type r)
    t
    cmds
    key
    ~min_score:min
    ~max_score:max
    ~with_scores
    (module R : Response_intf.S with type t = r)
    =
    (* https://redis.io/commands/zrangebyscore/#exclusive-intervals-and-infinity *)
    command_key_bounded_range
      t
      cmds
      key
      ~min
      ~max
      ~infinity_min:"-inf"
      ~infinity_max:"+inf"
      ~incl_prefix:""
      ~with_scores
      (module R)
      (module Bulk_io.Float)
  ;;

  let command_key_lex_range
    (type r)
    t
    cmds
    key
    ~min
    ~max
    (module R : Response_intf.S with type t = r)
    =
    (* https://redis.io/commands/zrangebylex/#how-to-specify-intervals *)
    command_key_bounded_range
      t
      cmds
      key
      ~min
      ~max
      ~infinity_min:"-"
      ~infinity_max:"+"
      ~incl_prefix:"["
      ~with_scores:false
      (module R)
      (module Value)
  ;;

  (** Handle invalidation PUSH messages *)
  let invalidation t data =
    match t.invalidations with
    | None -> ()
    | Some (bus, _) -> Bus.write bus data
  ;;

  let handle_message subscriptions buf =
    Resp3.expect_char buf '$';
    let subscription = Resp3.blob_string buf in
    match Hashtbl.find subscriptions subscription with
    | None ->
      raise_s
        [%message
          [%here]
            "BUG: Received a message that was not subscribed to"
            (subscription : string)]
    | Some writers ->
      let lo = Iobuf.Expert.lo buf in
      List.iteri writers ~f:(fun i (Subscriber { writer; consume }) ->
        if i <> 0 then Iobuf.Expert.set_lo buf lo;
        let payload =
          consume (buf :> (read, Iobuf.seek, Iobuf.global) Iobuf.t) ~subscription
        in
        Pipe.write_without_pushback_if_open writer payload)
  ;;

  (** Read RESP3 out-of-band push messages *)
  let read_push t buf =
    let len = Int.of_string (Resp3.simple_string buf) in
    Resp3.expect_char buf '$';
    let pushed = Resp3.blob_string buf in
    match len, pushed with
    | 2, "invalidate" ->
      (* As of Redis 6.0.8
         - When using BCAST the invalidation array can be larger than size 1, which is not
           documented in the protocol.
         - The invalidation messages are decoupled from the atomicity guarantees inside
           Redis, {{: https://github.com/redis/redis/issues/7563 } which arguably should not
           be the case}. For example: If I invalidate 3 keys using MSET the client should
           ideally receive 1 invalidation message with 3 keys, but instead receives 3
           invalidation message each with one key. *)
      (match Resp3.peek_char buf with
       | '*' ->
         let keys = Key_parser.list buf |> Or_error.ok_exn in
         List.iter keys ~f:(fun key -> invalidation t (`Key key))
       | '_' ->
         Iobuf.advance buf 1;
         Resp3.expect_crlf buf;
         invalidation t `All
       | unexpected ->
         raise
           (Resp3.Protocol_error
              (sprintf "Expected an invalidation message but observed '%c'" unexpected)))
    | 3, ("subscribe" | "psubscribe" | "unsubscribe" | "punsubscribe") ->
      (* Intentionally ignored, see comments for the [subscribe] command *)
      ()
    | 3, "message" -> handle_message t.subscriptions buf
    | 4, "pmessage" -> handle_message t.pattern_subscriptions buf
    | len, _ ->
      raise_s
        [%message
          "Received a PUSH message type which is not implemented"
            (len : int)
            (pushed : string)]
  ;;

  (** Read messages coming from the Redis to the client *)
  let read t =
    Reader.read_one_iobuf_at_a_time t.reader ~handle_chunk:(fun buf ->
      (* We will receive a callback when new data from the server is available.

         When reading RESP3 there's no way to know the full length of the message until we
         have parsed the whole thing. This is a flaw in the protocol. There's also no
         guarantee that our buffer contains a full message, so we need to handle the case
         where the buffer contains an incomplete message. We do this by raising and
         handling the exception [Need_more_data] which causes message parsing to wait for
         more data before continuing.

         Because parsing must start from the beginning of a message and there's no way to
         consistently know how much more data is needed to obtain a complete message, one
         can imagine a pathological case where the cost of parsing a large message that
         arrives in many pieces becomes quadratic.

         We can make an improvement to this behavior: The presence of the characters
         "\r\n" at the end of the buffer is necessary (but not sufficient) for the buffer
         to end in a complete RESP3 message. If these characters are not found then it
         must be true that more data will arrive from the server. We can use this as an
         inexpensive check to defer parsing that would fail without more data. *)
      if Resp3.ends_in_crlf buf
      then (
        try
          while not (Iobuf.is_empty buf) do
            match Iobuf.Unsafe.Peek.char buf ~pos:0 with
            | '>' ->
              Iobuf.advance buf 1;
              read_push t buf
            | _ ->
              if Queue.is_empty t.pending_response
              then
                raise_s
                  [%message
                    [%here]
                      "Received a response when none was expected"
                      (buf
                       : (read_write, Iobuf.seek, Iobuf.global) Iobuf.Window.Hexdump.t)]
              else (
                let response = Queue.peek_exn t.pending_response in
                let module R = (val response : Response_intf.S) in
                Ivar.fill_exn R.this (R.parse buf);
                (* Parsing this message succeeded. It is now safe to dispose of the
                   pending response and mark that this portion of the buffer has been
                   consumed. *)
                Iobuf.narrow_lo buf;
                ignore (Queue.dequeue_exn t.pending_response : (module Response_intf.S)))
          done;
          return `Continue
        with
        | Need_more_data ->
          Iobuf.rewind buf;
          return `Continue
        | exn -> return (`Stop exn))
      else (* There is an incomplete message at the end of the buffer *)
        return `Continue)
  ;;

  let close t =
    let%bind () = Writer.close t.writer in
    let%map () = Reader.close t.reader in
    Option.iter t.invalidations ~f:(fun (bus, _) -> Bus.close bus);
    List.iter
      (Hashtbl.data t.subscriptions @ Hashtbl.data t.pattern_subscriptions)
      ~f:(fun subscribers ->
        List.iter subscribers ~f:(fun (Subscriber { writer; consume = _ }) ->
          Pipe.close writer))
  ;;

  let close_finished t =
    let%bind () = Writer.close_finished t.writer in
    Reader.close_finished t.reader
  ;;

  let has_close_started t = Writer.is_closed t.writer || Reader.is_closed t.reader

  let connection_state t =
    match Writer.is_closed t.writer, Reader.is_closed t.reader with
    | true, true -> `Disconnected
    | true, _ | _, true -> `Disconnecting
    | false, false -> `Connected
  ;;

  let create ?on_disconnect ?auth ~where_to_connect (_ : 'a) =
    let%bind.Deferred.Or_error socket, reader, writer =
      (* Tcp.connect will raise if the connection attempt times out, but we'd prefer to
         return an Error. *)
      Monitor.try_with_or_error (fun () -> Tcp.connect where_to_connect)
    in
    Socket.setopt socket Socket.Opt.keepalive true;
    let pending_response = Queue.create () in
    let t =
      { pending_response
      ; reader
      ; writer
      ; invalidations = None
      ; subscriptions = String.Table.create ()
      ; pattern_subscriptions = String.Table.create ()
      }
    in
    Writer.set_raise_when_consumer_leaves writer false;
    don't_wait_for
      (let%bind reason = read t in
       let reason =
         match reason with
         | `Eof | `Eof_with_unconsumed_data _ -> Error.of_string disconnect_message
         | `Stopped exn -> Error.of_exn exn
       in
       let%map () = close t in
       Queue.iter t.pending_response ~f:(fun response ->
         let module R = (val response : Response_intf.S) in
         Ivar.fill_exn R.this (Error reason));
       Queue.clear t.pending_response;
       Option.iter on_disconnect ~f:(fun f -> f ()));
    (* Tell the session that we will be speaking RESP3 and authenticate if need be *)
    let cmds =
      [ "HELLO"; "3" ]
      (* When protover (i.e. 2/3) is used, we can also pass [AUTH] and [SETNAME] to
         [HELLO]. *)
      @ Option.value_map auth ~default:[] ~f:(fun { Auth.username; password } ->
        [ "AUTH"; username; password ])
    in
    let%map.Deferred.Or_error (_ : Resp3.t String.Map.t) =
      command_string t cmds (Response.create_string_map ())
    in
    t
  ;;

  let role conn = command_string conn [ "ROLE" ] (Response.create_role ())

  let sentinel_get_leader_address (t : [< `Sentinel ] t) ~leader_name =
    command_string
      t
      [ "SENTINEL"; "GET-MASTER-ADDR-BY-NAME"; leader_name ]
      (Response.create_host_and_port ())
  ;;

  let sentinel_replicas (t : [< `Sentinel ] t) ~leader_name =
    let%bind.Deferred.Or_error result =
      command_string
        t
        [ "SENTINEL"; "REPLICAS"; leader_name ]
        (Response.create_string_map_list ())
    in
    List.map result ~f:Sentinel.Replica.of_string_map
    |> Or_error.combine_errors
    |> Deferred.return
  ;;

  (* Sentinel requires two connection steps:

     1. Connect to the sentinel and ask for the leader address
     2. Connect to the proposed leader and confirm that it is a leader

     Read more here:
     https://redis.io/docs/reference/sentinel-clients/#redis-service-discovery-via-sentinel

     If all sentinels fail to connect or return a leader, then the client should return an
     error. The leader node will disconnect from the client on failover, so the client
     does not need to poll or listen to a subscription event to determine when to
     disconnect from a stale leader.

     The same is true for replicas: once we get a replica, we need to connect to it to
     confirm that it is indeed a replica.
  *)

  let sentinel_connect_to_one_replica
    (t : [< `Sentinel ] t)
    ?on_disconnect
    ?auth
    ?(replica_priority_sorter = fun l -> List.permute l)
    ~leader_name
    ()
    =
    let open Deferred.Or_error.Let_syntax in
    let%bind replicas = sentinel_replicas t ~leader_name >>|? replica_priority_sorter in
    Deferred.Or_error.find_map_ok replicas ~f:(fun replica ->
      let replica_host_and_port = replica.Sentinel.Replica.host_and_port in
      let where_to_connect =
        Tcp.Where_to_connect.of_host_and_port replica_host_and_port
      in
      let%bind conn = create ?on_disconnect ?auth ~where_to_connect () in
      match%bind role conn with
      | Replica _ -> return conn
      | (Sentinel _ | Leader _) as role ->
        let%bind.Deferred () = close conn in
        Deferred.Or_error.error_s
          [%message
            "Not a replica" (role : Role.t) ~leader_name (replica : Sentinel.Replica.t)])
  ;;

  let sentinel_connect_to_leader
    (t : [< `Sentinel ] t)
    ?on_disconnect
    ?auth
    ~leader_name
    ()
    =
    let open Deferred.Or_error.Let_syntax in
    let%bind leader_addr =
      match%map.Deferred sentinel_get_leader_address t ~leader_name with
      | Error error ->
        Or_error.error_s
          [%message
            "Error getting leader, maybe the server is not a sentinel?" (error : Error.t)]
      | Ok leader -> Ok (Tcp.Where_to_connect.of_host_and_port leader)
    in
    let%bind leader_conn =
      create ?on_disconnect ?auth ~where_to_connect:leader_addr ()
      |> Deferred.Or_error.tag_s
           ~tag:
             [%message
               "Failed to connect to redis"
                 ~leader_name
                 (leader_addr : Tcp.Where_to_connect.inet)]
    in
    match%bind role leader_conn with
    | Leader _ -> return leader_conn
    | (Sentinel _ | Replica _) as role ->
      let%bind.Deferred () = close leader_conn in
      Deferred.Or_error.error_s
        [%message
          "Not the leader"
            (role : Role.t)
            ~leader_name
            (leader_addr : Tcp.Where_to_connect.inet)]
  ;;

  let start_client_tracking t ?(bcast = false) () =
    match t.invalidations with
    | Some (bus, current_bcast) ->
      if Bool.equal current_bcast bcast
      then Bus.read_only bus |> Deferred.Or_error.return
      else
        Deferred.Or_error.error_s
          [%message
            "start_client_tracking called with a different bcast. Call \
             stop_client_tracking first to change bcast."
              (bcast : bool)]
    | None ->
      let bus =
        Bus.create_exn
          Arity1
          ~on_subscription_after_first_write:Allow
          ~on_callback_raise:Error.raise
      in
      t.invalidations <- Some (bus, bcast);
      let commands =
        match bcast with
        | false -> [ "CLIENT"; "TRACKING"; "ON"; "NOLOOP" ]
        | true -> [ "CLIENT"; "TRACKING"; "ON"; "NOLOOP"; "BCAST" ]
      in
      let%map.Deferred.Or_error () = command_string t commands (Response.create_ok ()) in
      Bus.read_only bus
  ;;

  let stop_client_tracking t () =
    match t.invalidations with
    | Some (bus, _) ->
      Bus.close bus;
      t.invalidations <- None;
      command_string t [ "CLIENT"; "TRACKING"; "OFF" ] (Response.create_ok ())
    | None -> Deferred.Or_error.ok_unit
  ;;

  let add_subscriber
    t
    (lookup : subscription_table)
    ~unsubscribe_command
    (Subscriber { writer; _ } as subscriber)
    ~channel
    =
    Hashtbl.add_multi lookup ~key:channel ~data:subscriber;
    don't_wait_for
    @@
    let%bind () = Pipe.closed writer in
    (* We know for a fact that [Hashtbl.find lookup channel] exists and has this entry
       because the only way that entry would be removed is if the list is empty, and that
       list is only empty if all subscribers have been removed. The only code that removes
       this subscriber is the one below.
    *)
    let remaining_subscribers =
      Hashtbl.update_and_return lookup channel ~f:(function
        | None ->
          raise_s
            [%message
              "BUG: [Redis.Client.add_subscriber] could not find entry for channel in \
               the subscription table when cleaning up a closed subscriber"
                (channel : string)]
        | Some subscribers ->
          List.filter subscribers ~f:(fun subscriber_in_list ->
            not (phys_equal subscriber_in_list subscriber)))
    in
    if List.is_empty remaining_subscribers
    then
      Deferred.ignore_m
        (* Ignore the return value because:
           - A failure probably means the connection was closed
           - There's nothing we can do about an unsubscription failure in the first place
        *)
        (with_writer t (fun writer ->
           write_array_header writer 2;
           write_array_el writer (module Bulk_io.String) unsubscribe_command;
           let (module R) =
             Response.create_unsubscription ~channel ~on_success:(fun () ->
               Hashtbl.remove lookup channel)
           in
           Queue.enqueue t.pending_response (module R);
           write_array_el writer (module Bulk_io.String) channel;
           Ivar.read R.this))
    else return ()
  ;;

  let subscribe_impl t channels ~command ~unsubscribe_command ~lookup ~consume =
    (* Subscription command replies are unusual: Redis will respond using a separate push
       message for each channel subscribed to, but unlike normal push messages, each is
       expected to be in pipelined sequence like a normal command.

       To deal with this we listen for push messages of "subscribe" and ignore them. The
       receive loop continues and will treat the following message fragment as a normal
       in-band protocol message. This command expects the same number of responses of this
       shape as channels specified to the command or an error, in which case it will
       dequeue whatever other responses it may be expecting. *)
    let subscription_reader, subscription_writer = Pipe.create () in
    let subscriber = Subscriber { consume; writer = subscription_writer } in
    match
      List.filter channels ~f:(fun channel ->
        match Hashtbl.find_multi lookup channel with
        | _ :: _ ->
          (* If the list is ever empty, then we know for a fact that an unsubscribe has
             been issued or we have never subscribed, so we have to resubscribe. *)
          add_subscriber t lookup subscriber ~unsubscribe_command ~channel;
          false
        | [] -> true)
    with
    | [] -> Deferred.Or_error.return subscription_reader
    | new_channels ->
      let%map.Deferred.Or_error () =
        with_writer t (fun writer ->
          write_array_header writer (List.length channels + 1);
          write_array_el writer (module Bulk_io.String) command;
          List.map new_channels ~f:(fun channel ->
            let r =
              Response.create_subscription ~channel ~on_success:(fun () ->
                (* Subscriber registration must be added synchronously and immediately
                   after a subscription succeeds, as opposed to waiting for the Response
                   to be determined. This is necessary because the receive loop processes
                   a buffer without yielding that may contain multiple messages, and a
                   message destined for a new subscriber could be within that buffer
                   following the subscription success. *)
                add_subscriber t lookup subscriber ~unsubscribe_command ~channel)
            in
            let (module R) = r in
            Queue.enqueue t.pending_response (module R);
            write_array_el writer (module Bulk_io.String) channel;
            channel, r)
          |> List.fold ~init:Deferred.Or_error.ok_unit ~f:(fun acc (_channel, r) ->
            match%bind.Deferred acc with
            | Error error ->
              (* If there was an error, dequeue the next subscription request, as there
                 will never be a response. *)
              ignore (Queue.dequeue_exn t.pending_response : (module Response_intf.S));
              Deferred.Or_error.fail error
            | Ok () ->
              let (module R) = r in
              let%map.Deferred.Or_error _ = Ivar.read R.this in
              ()))
      in
      subscription_reader
  ;;

  let subscribe_raw t = function
    | `Literal subscriptions ->
      subscribe_impl
        t
        subscriptions
        ~command:"SUBSCRIBE"
        ~unsubscribe_command:"UNSUBSCRIBE"
        ~lookup:t.subscriptions
    | `Pattern subscriptions ->
      subscribe_impl
        t
        subscriptions
        ~command:"PSUBSCRIBE"
        ~unsubscribe_command:"PUNSUBSCRIBE"
        ~lookup:t.pattern_subscriptions
  ;;
end
