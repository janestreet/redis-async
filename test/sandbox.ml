open Async
open Core
open Async_unix
open Deferred.Or_error.Let_syntax

let path () = "/usr/bin/"


module Redis_string = Redis.Make (Redis.Bulk_io.String) (Redis.Bulk_io.String)

let t = Set_once.create ()

let create () =
  let%bind.Deferred directory = Unix.mkdtemp "/dev/shm/redis" in
  let unixsocket = directory ^ "/redis.sock" in
  let%bind _ =
    Process.create
      ~prog:"bwrap"
      ~args:
        [ "--dev-bind"
        ; "/"
        ; "/"
        ; "--unshare-pid"
        ; "--proc"
        ; "/proc"
        ; "--die-with-parent"
        ; "--"
        ; path () ^ "/redis-server"
        ; "--port"
        ; "0"
        ; "--unixsocket"
        ; unixsocket
        ; "--save"
        ; {|""|}
        ]
      ()
  in
  let%bind.Deferred () = Sys.when_file_exists unixsocket in
  let where_to_connect = Tcp.Where_to_connect.of_file unixsocket in
  Deferred.Or_error.return where_to_connect
;;

let where_to_connect () =
  let%bind where_to_connect =
    let open Set_once.Optional_syntax in
    match%optional t with
    | None ->
      let where_to_connect = create () in
      Shutdown.at_shutdown (fun () ->
        Deferred.ignore_m
          (let%bind where_to_connect = where_to_connect in
           let%bind redis = Redis_string.create ~where_to_connect () in
           Redis_string.shutdown redis));
      Set_once.set_exn t [%here] where_to_connect;
      where_to_connect
    | Some where_to_connect -> where_to_connect
  in
  let%bind redis = Redis_string.create ~where_to_connect () in
  let%map  ()    = Redis_string.flushall redis              in
  where_to_connect
;;
