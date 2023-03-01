open Core
open Async
open Deferred.Or_error.Let_syntax
open Redis
module R = Redis.Make (Bulk_io.String) (Bulk_io.String)
module Expect_test_config = Expect_test_config_or_error

let weak_to_connected_redis () =
  let redis_ref = Weak_pointer.create () in
  let%bind where_to_connect, _ = Sandbox.where_to_connect () in
  Shutdown.at_shutdown (fun () -> Deferred.ignore_m (Sandbox.teardown ()));
  let%bind r = R.create ~where_to_connect () in
  Weak_pointer.set redis_ref (Heap_block.create_exn r);
  return redis_ref
;;

let%expect_test "leak" =
  let%bind redis_ref = weak_to_connected_redis () in
  Gc.full_major ();
  [%test_eq: bool] (Weak_pointer.is_some redis_ref) true;
  return ()
;;
