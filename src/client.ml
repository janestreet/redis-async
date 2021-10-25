open Core
open Async
open Common

type 'a t =
  { pending_response      : (module Response_intf.S) Queue.t
  ; reader                : Reader.t
  ; writer                : Writer.t
  ; mutable invalidations : [ `All | `Key of 'a ] Pipe.Writer.t list
  }

module Make (Key : Bulk_io_intf.S) (Field : Bulk_io_intf.S) (Value : Bulk_io_intf.S) =
struct
  module Key_parser   = Parse_bulk.Make (Key)
  module Field_parser = Parse_bulk.Make (Field)
  module Value_parser = Parse_bulk.Make (Value)
  module Field_value_map_parser = Parse_bulk.Make_map (Field_parser) (Value_parser)

  let write_array_header writer len =
    Writer.write_char writer '*';
    Writer.write      writer (itoa len);
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
    Writer.write      writer (len + String.length prefix |> itoa);
    write_crlf writer;
    Writer.write writer prefix;
    IO.Redis_bulk_io.write ~len writer el;
    write_crlf writer
  ;;

  let with_writer t f =
    if Writer.is_closed t.writer
    then Deferred.Or_error.error_string "Disconnected"
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
        List.iter args ~f:(fun arg -> write_array_el writer (module Key           ) arg);
        Ivar.read R.this)
  ;;

  let command_keys_args
        (type r a)
        t
        ?result_of_empty_input
        cmds
        key_args
        args
        (module Arg : Bulk_io_intf.S  with type t = a)
        (module R   : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty key_args || List.is_empty args -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header
          writer
          (List.length cmds + List.length key_args + List.length args);
        List.iter cmds     ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter key_args ~f:(fun arg -> write_array_el writer (module Key           ) arg);
        List.iter args     ~f:(fun arg -> write_array_el writer (module Arg           ) arg);
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
           + (List.length fields_and_value_args * 2));
        List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter key_args ~f:(fun arg -> write_array_el writer (module Key) arg);
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
        (module R : Response_intf.S with type t = r)
    =
    match result_of_empty_input with
    | Some result when List.is_empty alist -> return result
    | _ ->
      with_writer t (fun writer ->
        Queue.enqueue t.pending_response (module R);
        write_array_header writer (List.length cmds + (List.length alist * 2));
        List.iter cmds  ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
        List.iter alist ~f:(fun (key, value) ->
          write_array_el writer (module Key  ) key;
          write_array_el writer (module Value) value);
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

  let command_key_lex_range
        (type r)
        t
        cmds
        key
        ~min
        ~max
        (module R : Response_intf.S with type t = r)
    =
    with_writer t (fun writer ->
      let write_bound bound infinite_symbol =
        match bound with
        | Unbounded  -> write_array_el writer (module Bulk_io.String) infinite_symbol
        | Incl value -> write_array_el writer (module Value) value ~prefix:"["
        | Excl value -> write_array_el writer (module Value) value ~prefix:"("
      in
      Queue.enqueue t.pending_response (module R);
      write_array_header writer (List.length cmds + 3);
      List.iter cmds ~f:(fun cmd -> write_array_el writer (module Bulk_io.String) cmd);
      write_array_el writer (module Key) key;
      write_bound min "-";
      write_bound max "+";
      Ivar.read R.this)
  ;;

  (** Handle invalidation PUSH messages *)
  let invalidation t data =
    let was_changed, invalidations =
      List.fold
        t.invalidations
        ~init:(false, [])
        ~f:(fun (was_changed, invalidations) invalidation ->
          if Pipe.is_closed invalidation
          then true, invalidations
          else (
            Pipe.write_without_pushback invalidation data;
            was_changed, invalidation :: invalidations))
    in
    if was_changed
    then (
      t.invalidations <- invalidations;
      if List.is_empty invalidations
      then
        don't_wait_for
          (Deferred.ignore_m
             (command_string t [ "CLIENT"; "TRACKING"; "OFF" ] (Response.create_ok ()))))
  ;;

  (** Read RESP3 out-of-band push messages *)
  let read_push t buf =
    let len = Int.of_string (Resp3.simple_string buf) in
    Resp3.expect_char buf '$';
    let pushed = Resp3.blob_string buf in
    match len, pushed, Resp3.peek_char buf with
    | 2, "invalidate", '*' ->
      (* As of Redis 6.0.8
         - When using BCAST the invalidation array can be larger than size 1, which is not
           documented in the protocol.
         - The invalidation messages are decoupled from the atomicity guarantees inside
           Redis, {{: https://github.com/redis/redis/issues/7563 } which arguably should not
           be the case}. For example: If I invalidate 3 keys using MSET the client should
           ideally receive 1 invalidation message with 3 keys, but instead receives 3
           invalidation message each with one key. *)
      let keys = Key_parser.list buf |> Or_error.ok_exn in
      List.iter keys ~f:(fun key -> invalidation t (`Key key))
    | 2, "invalidate", '_' ->
      Iobuf.advance buf 1;
      Resp3.expect_crlf buf;
      invalidation t `All
    | _ ->
      raise_s
        [%message
          "Received a PUSH message type which is not implemented" (pushed : string)]
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
            | _   ->
              if Queue.is_empty t.pending_response
              then
                raise_s
                  [%message
                    [%here]
                      "Received a response when none was expected"
                      (buf : (read_write, Iobuf.seek) Iobuf.Window.Hexdump.t)]
              else (
                let response = Queue.peek_exn t.pending_response in
                let module R = (val response : Response_intf.S) in
                Ivar.fill R.this (R.parse buf);
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
    let%map  () = Reader.close t.reader in
    List.iter t.invalidations ~f:Pipe.close;
    t.invalidations <- []
  ;;

  let create ?on_disconnect ~where_to_connect () =
    let%bind.Deferred.Or_error _socket, reader, writer =
      (* Tcp.connect will raise if the connection attempt times out, but we'd prefer to
         return an Error. *)
      Monitor.try_with_or_error (fun () -> Tcp.connect where_to_connect)
    in
    let pending_response = Queue.create ()                                          in
    let t                = { pending_response; reader; writer; invalidations = [] } in
    Writer.set_raise_when_consumer_leaves writer false;
    don't_wait_for
      (let%bind reason = read t in
       let reason =
         match reason with
         | `Eof | `Eof_with_unconsumed_data _ -> Error.of_string "Disconnected"
         | `Stopped exn                       -> Error.of_exn exn
       in
       let%map () = close t in
       Queue.iter t.pending_response ~f:(fun response ->
         let module R = (val response : Response_intf.S) in
         Ivar.fill R.this (Error reason));
       Queue.clear t.pending_response;
       Option.iter on_disconnect ~f:(fun f -> f ()));
    (* Tell the session that we will be speaking RESP3 *)
    let%map.Deferred.Or_error _ =
      command_string t [ "HELLO"; "3" ] (Response.create_resp3 ())
    in
    t
  ;;

  let client_tracking t ?(bcast = false) () =
    let commands =
      match bcast with
      | false -> [ "CLIENT"; "TRACKING"; "ON"; "NOLOOP"          ]
      | true  -> [ "CLIENT"; "TRACKING"; "ON"; "NOLOOP"; "BCAST" ]
    in
    let reader, writer = Pipe.create   ()              in
    let was_empty      = List.is_empty t.invalidations in
    t.invalidations <- writer :: t.invalidations;
    let%map.Deferred.Or_error () =
      if was_empty
      then command_string t commands (Response.create_ok ())
      else Deferred.Or_error.return ()
    in
    reader
  ;;
end
