open! Core
open  Async
open  Redis
open  Deferred.Or_error.Let_syntax
module Expect_test_config = Expect_test_config_or_error
module R = Redis.Make (Bulk_io.String) (Bulk_io.String)

let with_sandbox = Sandbox.run (module R)

let%expect_test "Strings" =
  with_sandbox (fun r ->
    let%bind response = R.exists r [ "hello" ] in
    [%test_eq: int] response 0;
    let%bind ()       = R.set r "hello" "world" in
    let%bind response = R.exists r [ "hello" ]  in
    [%test_eq: int] response 1;
    let%bind response = R.dbsize r in
    [%test_eq: int] response 1;
    let%bind response = R.get r "hello" in
    print_s ([%sexp_of: string option] response);
    [%expect {| (world) |}];
    let%bind response = R.get r "bork" in
    print_s ([%sexp_of: string option] response);
    [%expect {| () |}];
    let%bind () = R.select r 1                              in
    let%bind () = R.mset r [ "a", "b"; "c", "d"; "e", "f" ] in
    let%bind response = R.del r [ "c"; "g" ]                in
    print_s ([%sexp_of: int] response);
    [%expect {|
      1 |}];
    let%bind response = R.mget r [ "hello"; "a"; "c"; "e" ] in
    print_s ([%sexp_of: string option list] response);
    [%expect {| (() (b) () (f)) |}];
    let%bind response = R.setnx r "color" "blue" in
    print_s ([%sexp_of: bool] response);
    [%expect {| true |}];
    let%bind response = R.setnx r "color" "green" in
    [%test_eq: bool] response false;
    let%bind response = R.get r "color" in
    print_s ([%sexp_of: string option] response);
    [%expect {| (blue) |}];
    (* color already exists, so it should not be overriden, and none of the other values
       should be set. *)
    let%bind response = R.msetnx r [ "color", "gray"; "foo", "bar"; "baz", "foobaz" ] in
    [%test_eq: bool] response false;
    let%bind response = R.mget r [ "color"; "foo"; "baz" ] in
    print_s [%sexp (response : string option list)];
    [%expect {| ((blue) () ()) |}];
    (* all of the keys are new keys, so they should be set *)
    let%bind response = R.msetnx r [ "foo", "bar"; "baz", "foobaz" ] in
    [%test_eq: bool] response true;
    let%bind response = R.mget r [ "foo"; "baz" ] in
    print_s [%sexp (response : string option list)];
    [%expect {| ((bar) (foobaz)) |}];
    (* Special case for empty variadic inputs *)
    let%bind ()       = R.mset   r [] in
    let%bind response = R.msetnx r [] in
    [%test_eq: bool] response true;
    let%bind response = R.mget r [] in
    [%test_eq: string option list] response [];
    let%bind response = R.del r [] in
    [%test_eq: int] response 0;
    let%bind cursor, scan0 = R.scan r ~cursor:Cursor.zero ~count:1 () in
    let%bind cursor, scan1 = R.scan r ~cursor ()                      in
    (* The SCAN test is written to show summary results because this command is
       intentionally non-deterministic (see documentation) *)
    print_s
      ([%sexp_of: Cursor.t * string list]
         (cursor, List.dedup_and_sort ~compare:String.compare (scan0 @ scan1)));
    [%expect {| (0 (a baz color e foo)) |}];
    let%bind cursor, scan3 = R.scan r ~cursor:Cursor.zero ~pattern:"[ab]*" () in
    print_s
      ([%sexp_of: Cursor.t * string list]
         (cursor, List.dedup_and_sort ~compare:String.compare scan3));
    [%expect {| (0 (a baz)) |}];
    let%bind () = R.set r "has expire" "x" ~expire:Time_ns.Span.hour in
    let print_timeout_opaque response =
      print_endline
        (match response with
         (* Don't print the timeout value to make tests deterministic *)
         | `Timeout _  -> "Timeout"
         | `No_timeout -> "No_timeout"
         | `No_key     -> "No_key")
    in
    let%bind response = R.pttl r "has expire" in
    print_timeout_opaque response;
    [%expect {| Timeout |}];
    let%bind response = R.pttl r "foo" in
    print_timeout_opaque response;
    [%expect {| No_timeout |}];
    let%bind response = R.pttl r "key that was never set" in
    print_timeout_opaque response;
    [%expect {| No_key |}];
    let%bind response = R.pexpire r "foo" Time_ns.Span.hour in
    print_s ([%sexp_of: [ `Set | `Not_set ]] response);
    [%expect {| Set |}];
    let%bind response = R.pexpire r "key that was never set" Time_ns.Span.hour in
    print_s ([%sexp_of: [ `Set | `Not_set ]] response);
    [%expect {| Not_set |}];
    return ())
;;

let%expect_test "Command: keys" =
  with_sandbox (fun r ->
    let%bind () =
      R.mset r [ "a", "b"; "c", "d"; "e", "f"; "foo", "g"; "foobar", "h" ]
    in
    let%bind response = R.keys r >>| List.sort ~compare:String.compare in
    print_s ([%sexp_of: string list] response);
    [%expect {| (a c e foo foobar) |}];
    let%bind response =
      R.keys r ~pattern:"foo*" >>| List.sort ~compare:String.compare
    in
    print_s ([%sexp_of: string list] response);
    [%expect {| (foo foobar) |}];
    return ())
;;

let%expect_test "Command: incr" =
  with_sandbox (fun r ->
    (* Redis docs: "If the key does not exist, it is set to 0 before performing the
       operation." *)
    let%bind response = R.incr r "hi" in
    [%test_eq: int] response 1;
    let%bind response = R.incr r "hi" in
    [%test_eq: int] response 2;
    return ())
;;

let%expect_test "Command: ping" =
  with_sandbox (fun r ->
    let%bind response = R.ping r "hi" in
    [%test_eq: string] response "hi";
    return ())
;;

let%expect_test "set commands" =
  with_sandbox (fun r ->
    let key_1 = "1" in
    let query_and_print_members key =
      let%map members_unsorted = R.smembers r key in
      (* Sorting here is important because redis represents the values as an unordered
         set, and so the return order will not be deterministic. *)
      let members = List.sort members_unsorted ~compare:String.compare in
      print_s ([%sexp_of: string list] members)
    in
    let%bind response = R.sadd r key_1 [ "a"; "b"; "c"; "d" ] in
    [%test_eq: int] response 4;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (a b c d) |}];
    let%bind response = R.sadd r key_1 [ "c"; "d"; "e"; "f" ] in
    [%test_eq: int] response 2;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (a b c d e f) |}];
    let%bind response = R.srem r key_1 [ "a"; "b"; "d"; "g" ] in
    [%test_eq: int] response 3;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (c e f) |}];
    let%bind response = R.sismember r key_1 "a" in
    [%test_eq: bool] response false;
    let%bind response = R.sismember r key_1 "c" in
    [%test_eq: bool] response true;
    let%bind response = R.smismember r key_1 [] in
    [%test_eq: bool list] response [];
    let%bind response = R.smismember r key_1 [ "a"; "f" ] in
    [%test_eq: bool list] response [ false; true ];
    return ())
;;

let%expect_test "sorted set commands" =
  with_sandbox (fun r ->
    let key_1 = "1" in
    let query_and_print_members key =
      let%map members = R.zrange r key ~min_index:0 ~max_index:(-1) in
      print_s ([%sexp_of: string list] members)
    in
    let%bind response =
      R.zadd r key_1 [ `Score 0., "a"; `Score 0., "b"; `Score 0., "c"; `Score 0., "d" ]
    in
    [%test_eq: int] response 4;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (a b c d) |}];
    let%bind response =
      R.zadd r key_1 [ `Score 0., "c"; `Score 0., "d"; `Score 0., "e"; `Score 0., "f" ]
    in
    [%test_eq: int] response 2;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (a b c d e f) |}];
    let%bind response = R.zrangebylex r key_1 ~min:(Incl "b") ~max:(Excl "e") in
    print_s ([%sexp_of: string list] response);
    [%expect {| (b c d) |}];
    let%bind response = R.zrangebylex r key_1 ~min:(Excl "b") ~max:Unbounded in
    print_s ([%sexp_of: string list] response);
    [%expect {| (c d e f) |}];
    let%bind response = R.zrangebylex r key_1 ~min:Unbounded ~max:(Incl "d") in
    print_s ([%sexp_of: string list] response);
    [%expect {| (a b c d) |}];
    let%bind response = R.zrem r key_1 [ "a"; "b"; "d"; "g" ] in
    [%test_eq: int] response 3;
    let%bind () = query_and_print_members key_1 in
    [%expect {| (c e f) |}];
    let%bind response = R.raw_command r [ "echo"; "hello" ] in
    print_s ([%sexp_of: Resp3.t] response);
    [%expect {| (String hello) |}];
    let%bind response = R.version r in
    [%test_eq: string] response Sandbox.version;
    return ())
;;

let%expect_test "hash commands" =
  with_sandbox (fun r ->
    let key_1 = "1" in
    (* HSET *)
    let%bind response = R.hset r key_1 [] in
    [%test_eq: int] response 0;
    let%bind response = R.hset r key_1 [ "a", "A"; "b", "B"; "c", "C" ] in
    [%test_eq: int] response 3;
    (* HGET *)
    let%bind response = R.hget r key_1 "a" in
    [%test_eq: string option] response (Some "A");
    let%bind response = R.hget r key_1 "d" in
    [%test_eq: string option] response None;
    (* HMGET *)
    let%bind response = R.hmget r key_1 [] in
    [%test_eq: string option list] response [];
    let%bind response = R.hmget r key_1 [ "a"; "d" ] in
    [%test_eq: string option list] response [ Some "A"; None ];
    (* HGETALL *)
    let%bind response = R.hgetall r key_1 in
    [%test_eq: (string * string) list] response [ "a", "A"; "b", "B"; "c", "C" ];
    (* HDEL *)
    let%bind response = R.hdel r key_1 [] in
    [%test_eq: int] response 0;
    let%bind response = R.hdel r key_1 [ "d"; "c" ] in
    [%test_eq: int] response 1;
    (* HKEYS *)
    let%bind response = R.hkeys r key_1 >>| List.sort ~compare:String.compare in
    [%test_eq: string list] response [ "a"; "b" ];
    (* HVALS *)
    let%bind response = R.hvals r key_1 >>| List.sort ~compare:String.compare in
    [%test_eq: string list] response [ "A"; "B" ];
    (* HSCAN *)
    let values = [ "a", "A"; "b", "B"; "c", "C"; "d", "D"; "e", "E" ] in
    let%bind (_ : int) = R.hset r key_1 values in
    let%bind response =
      Deferred.Or_error.repeat_until_finished
        (Cursor.zero (* cursor *), [] (* values *))
        (fun (cursor, values) ->
           let%map cursor, new_values = R.hscan r ~cursor ~count:2 key_1 in
           let values = values @ new_values in
           if Cursor.(cursor = zero) then `Finished values else `Repeat (cursor, values))
      >>| List.dedup_and_sort ~compare:[%compare: string * string]
    in
    [%test_eq: (string * string) list] response values;
    return ())
;;

let%expect_test "Parallel" =
  with_sandbox (fun r ->
    let values = List.init 10000 ~f:(fun i -> Int.to_string i, Int.to_string i) in
    let%bind _error =
      let%map.Deferred errors =
        Deferred.List.map values ~how:`Parallel ~f:(fun (k, v) -> R.set r k v)
      in
      Or_error.combine_errors errors
    in
    let%bind _error =
      let%map.Deferred errors =
        Deferred.List.map values ~how:`Parallel ~f:(fun (k, v) ->
          let%map response = R.get r k in
          if not (Option.equal String.equal (Some v) response)
          then print_s ([%sexp_of: string option] response))
      in
      Or_error.combine_errors errors
    in
    return ())
;;

let%expect_test "Bin_prot" =
  let module Key = struct
    module T = struct
      type t = { msg : string } [@@deriving bin_io, sexp_of]
    end

    include T
    include Bulk_io.Make_binable (T)
  end
  in
  let module Value = struct
    module T = struct
      type t =
        { color  : string
        ; number : int
        }
      [@@deriving bin_io, sexp_of]
    end

    include T
    include Bulk_io.Make_binable (T)
  end
  in
  let module Int_value = struct
    include Int
    include Bulk_io.Make_binable (Int)
  end
  in
  let module R = Redis.Make (Key) (Value) in
  let%bind where_to_connect = Sandbox.where_to_connect ()                           in
  let%bind r = R.create ~where_to_connect ()                                        in
  let%bind () = R.set r { Key.msg = "hello" } { Value.color = "blue"; number = 42 } in
  let%bind response = R.get r { Key.msg = "hello" }                                 in
  print_s ([%sexp_of: Value.t option] response);
  [%expect {| (((color blue) (number 42))) |}];
  let module Redis_with_too_small_value = Redis.Make (Key) (Int_value) in
  let%bind          r        = Redis_with_too_small_value.create ~where_to_connect () in
  let%bind.Deferred response = Redis_with_too_small_value.get r { Key.msg = "hello" } in
  print_s ([%sexp_of: int option Or_error.t] response);
  [%expect {| (Error "Bin_prot should have read 6 bytes but read 1") |}];
  let module Wrong_value = struct
    module T = struct
      type t = [ `Will_not_parse of int ] [@@deriving bin_io, sexp_of]
    end

    include T
    include Bulk_io.Make_binable (T)
  end
  in
  let module Redis_with_wrong_value = Redis.Make (Key) (Wrong_value) in
  let%bind          r        = Redis_with_wrong_value.create ~where_to_connect () in
  let%bind.Deferred response = Redis_with_wrong_value.get r { Key.msg = "hello" } in
  print_s ([%sexp_of: Wrong_value.t option Or_error.t] response);
  [%expect {| (Error (common.ml.Read_error Variant_tag 4)) |}];
  return ()
;;

let%expect_test "Invalidation" =
  let%bind where_to_connect = Sandbox.where_to_connect   () in
  let%bind reader           = R.create ~where_to_connect () in
  let%bind writer           = R.create ~where_to_connect () in
  let%bind invalidation     = R.client_tracking reader   () in
  let expect_invalidation () =
    let%map.Deferred result = Pipe.read invalidation in
    Ok (print_s ([%sexp_of: [ `Ok of [ `All | `Key of string ] | `Eof ]] result))
  in
  let%bind response = R.get reader "hello" in
  print_s ([%sexp_of: string option] response);
  [%expect {| () |}];
  let%bind () = R.set writer "hello" "world" in
  let%bind () = expect_invalidation ()       in
  [%expect {| (Ok (Key hello)) |}];
  let%bind () = R.flushall writer      in
  let%bind () = expect_invalidation () in
  [%expect {| (Ok All) |}];
  let%bind () = R.flushdb reader       in
  let%bind () = expect_invalidation () in
  [%expect {| (Ok All) |}];
  Pipe.close_read invalidation;
  return ()
;;

let%expect_test "Broadcast invalidation" =
  let%bind where_to_connect = Sandbox.where_to_connect             () in
  let%bind reader           = R.create ~where_to_connect           () in
  let%bind writer           = R.create ~where_to_connect           () in
  let%bind invalidation     = R.client_tracking ~bcast:true reader () in
  let expect_invalidation () =
    let%map.Deferred result = Pipe.read invalidation in
    Ok (print_s ([%sexp_of: [ `Ok of [ `All | `Key of string ] | `Eof ]] result))
  in
  let%bind () = R.set writer "hello" "world" in
  (* Reader receives bcast invalidation without first requesting any keys *)
  let%bind () = expect_invalidation ()       in
  [%expect {| (Ok (Key hello)) |}];
  let%bind () = R.flushall writer      in
  let%bind () = expect_invalidation () in
  [%expect {| (Ok All) |}];
  Pipe.close_read invalidation;
  return ()
;;

let%expect_test "Large message" =
  (* This intends to exercise the Need_more_data logic in Client.read. This is a good test
     for the negative case but not the positive one. That is: we don't have a way to prove
     that this test is doing its job other than commenting out Need_more_data logic and
     making sure the test fails, as behavior depends on timing as well as how the OS
     buffers the large message. *)
  with_sandbox (fun r ->
    let%bind _ = R.echo r (String.init 1000000 ~f:(fun _ -> 'x')) in
    [%expect {| |}];
    return ())
;;

let%expect_test "Bulk messages up to maximum size works" =
  with_sandbox (fun r ->
    let count = 511_482 in
    let%bind () =
      R.mset r (List.init count ~f:(fun i -> Int.to_string i, Int.to_string i))
    in
    let%bind.Deferred response =
      R.mget r (List.init count ~f:(fun i -> Int.to_string i))
    in
    print_s ([%sexp_of: unit Or_error.t] (Or_error.ignore_m response));
    [%expect {| (Ok ()) |}];
    return ())
;;

module Subscription_data = struct
  type 'a t =
    [ `Eof
    | `Nothing_available
    | `Ok of ('a * string) Base.Queue.t
    ]
  [@@deriving sexp_of]
end

let%expect_test "pub/sub" =
  with_sandbox (fun r ->
    let%bind reader   = R.subscribe  r [ "foo"; "bar" ] in
    let%bind p_reader = R.psubscribe r [ "f*"         ] in
    let p () =
      print_s
        [%message
          ""
            ~subscribe:(Pipe.read_now' reader : string Subscription_data.t)
            ~psubscribe:(Pipe.read_now' p_reader : string Subscription_data.t)]
    in
    p ();
    [%expect {| ((subscribe Nothing_available) (psubscribe Nothing_available)) |}];
    let%bind how_many_subscribers = R.publish r "foo" "hello" in
    [%test_eq: int] how_many_subscribers 2;
    p ();
    [%expect {| ((subscribe (Ok ((foo hello)))) (psubscribe (Ok ((foo hello))))) |}];
    let%bind how_many_subscribers = R.publish r "baz" "nobody is listening" in
    [%test_eq: int] how_many_subscribers 0;
    p ();
    [%expect {| ((subscribe Nothing_available) (psubscribe Nothing_available)) |}];
    let%bind how_many_subscribers = R.publish r "bar" "different channel, same pipe" in
    [%test_eq: int] how_many_subscribers 1;
    p ();
    [%expect
      {|
      ((subscribe (Ok ((bar "different channel, same pipe"))))
       (psubscribe Nothing_available)) |}];
    return ())
;;

let%expect_test "keyspace notifications" =
  with_sandbox (fun r ->
    (* The two readers intentionally have different event configurations. This exercies
       that event configuration is at the Redis instance level and notifications are
       broadcast (not per client), so it is possible to construct readers that will
       receive events that they did not request. Readers should drop such events. *)
    let%bind keyevent_reader = R.keyevent_notifications r [ `del; `expire ]           in
    let%bind keyspace_reader = R.keyspace_notifications r [ `del ] ~patterns:[ "f*" ] in
    let p () =
      print_s
        [%message
          ""
            ~keyevent:(Pipe.read_now' keyevent_reader : Key_event.t Subscription_data.t)
            ~keyspace:(Pipe.read_now' keyspace_reader : Key_event.t Subscription_data.t)]
    in
    p ();
    [%expect {| ((keyevent Nothing_available) (keyspace Nothing_available)) |}];
    let%bind () = R.mset r [ "foo", "hello"; "bar", "baz" ] in
    let%bind _  = R.del r [ "foo"; "was never set"; "bar" ] in
    let%bind _  = R.echo r "wait for a round trip"          in
    p ();
    [%expect {| ((keyevent (Ok ((del foo) (del bar)))) (keyspace (Ok ((del foo))))) |}];
    let%bind          _  = R.set r "foo" "back again" ~expire:Time_ns.Span.millisecond in
    let%bind.Deferred () = Clock_ns.after Time_ns.Span.millisecond                     in
    (* get has a side effect of forcing expiry evaluation *)
    let%bind          _  = R.get r "foo"                                               in
    p ();
    [%expect {| ((keyevent (Ok ((expire foo)))) (keyspace Nothing_available)) |}];
    return ())
;;

let%expect_test "scripting" =
  with_sandbox (fun r ->
    let%bind sha1 = R.script_load r {|return {"Hello, world", KEYS[1], ARGV[1]}|} in
    let%bind response = R.evalsha r sha1 [ "foo" ] [ "bar" ] in
    print_s ([%sexp_of: Redis.Resp3.t] response);
    [%expect {| (Array ((String "Hello, world") (String foo) (String bar))) |}];
    return ())
;;

let%expect_test "Disconnect **THIS TEST MUST BE LAST**" =
  let%bind where_to_connect = Sandbox.where_to_connect () in
  let on_disconnect () = print_endline "on_disconnect" in
  let%bind          r        = R.create ~on_disconnect ~where_to_connect () in
  let%bind.Deferred response = R.shutdown r                                 in
  print_s ([%sexp_of: unit Or_error.t] response);
  [%expect {|
    on_disconnect
    (Error Disconnected) |}];
  let%bind.Deferred response = R.echo r "hello" in
  print_s ([%sexp_of: string Or_error.t] response);
  [%expect {| (Error Disconnected) |}];
  return ()
;;
