open Async
open Core
open Async_unix
open Deferred.Or_error.Let_syntax

let path () = "/usr/bin/"
let version = "7.0.11"
let redis_server_binary () = path () ^/ "redis-server"


module Redis_string = Redis.Make (Redis.Bulk_io.String) (Redis.Bulk_io.String)

let mkdtmp () = Unix.mkdtemp "/dev/shm/redis"

let create ~args ~connected directory =
  let redis_server = redis_server_binary () in
  let%bind () =
    match%map.Deferred Sys.file_exists redis_server with
    | `Yes           -> Or_error.return ()
    | `No | `Unknown ->
      Or_error.error_s [%message [%here] "The redis binary was not found" ~redis_server]
  in
  let port =
    Available_ephemeral_port.take Socket.Type.tcp |> Available_ephemeral_port.port
  in
  let args =
    [ "--dev-bind"
    ; "/"
    ; "/"
    ; "--unshare-pid"
    ; "--proc"
    ; "/proc"
    ; "--die-with-parent"
    ; "--"
    ; redis_server
    ]
    @ args
    @ [ "--dir"; directory; "--port"; Int.to_string port ]
  in
  let%bind _ = Process.create ~prog:"bwrap" ~args () in
  let where_to_connect =
    Host_and_port.create ~host:"127.0.0.1" ~port |> Tcp.Where_to_connect.of_host_and_port
  in
  let%map () =
    match%bind.Deferred
      Clock_ns.with_timeout (Time_ns.Span.of_int_sec 30) (connected ())
    with
    | `Result (Ok ())              -> return ()
    | `Result (Error _) | `Timeout ->
      Deferred.Or_error.error_s [%message [%here] "Redis did not start"]
  in
  Shutdown.at_shutdown (fun () ->
    Deferred.ignore_m
      (let%bind redis = Redis_string.create ~where_to_connect () in
       let%bind () = Redis_string.shutdown redis in
       Process.run_expect_no_output ~prog:"rm" ~args:[ "-rf"; directory ] ()));
  where_to_connect
;;

let create_with_sock ?(args = []) ?connected () =
  let%bind.Deferred directory = mkdtmp () in
  let unixsocket       = directory ^ "/redis.sock"               in
  let where_to_connect = Tcp.Where_to_connect.of_file unixsocket in
  let args             = args @ [ "--unixsocket"; unixsocket ]   in
  let connected () =
    let%bind.Deferred () = Sys.when_file_exists unixsocket in
    match connected with
    | Some connected ->
      let%bind          r    = Redis_string.create ~where_to_connect () in
      let%bind.Deferred resp = connected r                              in
      let%bind.Deferred ()   = Redis_string.close r                     in
      Deferred.return resp
    | None -> return ()
  in
  let%map where_to_connect_inet = create ~args ~connected directory in
  where_to_connect, where_to_connect_inet
;;

let reset_acl client =
  let%bind () =
    Redis_string.acl_setuser
      client
      ~username:"default"
      ~rules:[ "reset"; "on"; "nopass"; "~*"; "&*"; "+@all" ]
  in
  let%bind users = Redis_string.acl_users client in
  let users =
    List.filter users ~f:(function
      | "default" -> false
      | _         -> true)
  in
  Redis_string.acl_deluser client users |> Deferred.Or_error.ignore_m
;;

let flush where_to_connect =
  let%bind r  = Redis_string.create ~where_to_connect () in
  let%bind () = reset_acl r                              in
  let%bind () =
    match%bind Redis_string.role r with
    | Sentinel _ | Replica _ -> return ()
    | Leader _               -> Redis_string.flushall r
  in
  let%map.Deferred () = Redis_string.close r in
  Ok ()
;;

let l = Set_once.create ()

let where_to_connect_leader () =
  let%bind where_to_connect_unix, where_to_connect_inet =
    let open Set_once.Optional_syntax in
    match%optional l with
    | None                  ->
      let where_to_connect = create_with_sock () in
      Set_once.set_exn l [%here] where_to_connect;
      where_to_connect
    | Some where_to_connect -> where_to_connect
  in
  let%map () = flush where_to_connect_unix in
  where_to_connect_unix, where_to_connect_inet
;;

let where_to_connect = where_to_connect_leader
let r                = Set_once.create ()

let where_to_connect_replica () =
  let%bind _, leader = where_to_connect_leader () in
  let open Set_once.Optional_syntax in
  let%bind where_to_connect_unix, where_to_connect_inet =
    match%optional r with
    | None ->
      let rec connected redis =
        match%bind.Deferred Redis_string.role redis with
        | Ok (Leader _) | Ok (Sentinel _) ->
          Deferred.Or_error.error_s
            [%message [%here] "Replica is not actually a replica node"]
        | Error _ ->
          let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_sec 1) in
          connected redis
        | Ok (Replica r) ->
          (match r.connection_state with
           | Connected -> return ()
           | Connecting | Connect | Sync | Handshake ->
             let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_sec 1) in
             connected redis)
      in
      let%bind.Deferred leader = Tcp.Where_to_connect.remote_address leader in
      let leader = Socket.Address.Inet.to_host_and_port leader in
      let args =
        [ "--replicaof"
        ; Host_and_port.host leader
        ; Host_and_port.port leader |> Int.to_string
        ]
      in
      let where_to_connect = create_with_sock ~args ~connected () in
      Set_once.set_exn r [%here] where_to_connect;
      where_to_connect
    | Some where_to_connect -> where_to_connect
  in
  let%map () = flush where_to_connect_unix in
  where_to_connect_unix, where_to_connect_inet
;;

let s = Set_once.create ()

let where_to_connect_sentinel () =
  let%bind _, leader = where_to_connect_leader  () in
  let%bind _         = where_to_connect_replica () in
  let open Set_once.Optional_syntax in
  match%optional s with
  | None ->
    let%bind.Deferred directory = mkdtmp ()                                  in
    let%bind.Deferred leader    = Tcp.Where_to_connect.remote_address leader in
    let leader      = Socket.Address.Inet.to_host_and_port leader in
    let leader_name = "test"                                      in
    let%bind.Deferred config_file, _ = Unix.mkstemp (directory ^ "/redis_conf") in
    let args =
      [ config_file
      ; "--sentinel"
      ; "monitor"
      ; leader_name
      ; Host_and_port.host leader
      ; Host_and_port.port leader |> Int.to_string
      ; "1"
      ; "--sentinel"
      ; "down-after-milliseconds"
      ; leader_name
      ; "1000"
      ]
    in
    let rec connected () =
      match%bind.Deferred Unix.stat config_file with
      | { kind = `File; size; _ } when Int64.(( > )) size Int64.zero -> return ()
      | _ ->
        let%bind.Deferred () = Clock.after (Time_float.Span.of_int_sec 1) in
        connected ()
    in
    let%bind where_to_connect = create ~connected ~args directory in
    let where_to_connect = leader_name, where_to_connect in
    Set_once.set_exn s [%here] where_to_connect;
    return where_to_connect
  | Some where_to_connect -> return where_to_connect
;;

let run (type r) (module R : Redis.S with type t = r) f =
  let%bind         where_to_connect, _ = where_to_connect ()           in
  let%bind         r                   = R.create ~where_to_connect () in
  let%bind         ()                  = f r                           in
  let%map.Deferred ()                  = R.close r                     in
  Ok ()
;;

let run_replica (type r) (module R : Redis.S with type t = r) f =
  let%bind         where_to_connect, _ = where_to_connect_replica ()   in
  let%bind         r                   = R.create ~where_to_connect () in
  let%bind         ()                  = f r                           in
  let%map.Deferred ()                  = R.close r                     in
  Ok ()
;;

let run_sentinel (type r) (module R : Redis.S with type t = r) f =
  let%bind where_to_connect = where_to_connect_sentinel () in
  let leader_name, where_to_connect = where_to_connect in
  let%bind r =
    R.create_using_sentinel ~leader_name ~where_to_connect:[ where_to_connect ] ()
  in
  let%bind         () = f r       in
  let%map.Deferred () = R.close r in
  Ok ()
;;

module R = Redis.Make (Redis.Bulk_io.String) (Redis.Bulk_io.String)

let teardown ?on_disconnect () =
  let disconnect where_to_connect =
    let%bind r = R.create ?on_disconnect ~where_to_connect () in
    R.shutdown r
  in
  let%bind _, sentinel = where_to_connect_sentinel () in
  let%bind _, replica  = where_to_connect_replica  () in
  let%bind _, leader   = where_to_connect          () in
  (* The order we shutdown matters due to inter-dependencies. *)
  Deferred.Or_error.combine_errors_unit
    (List.map [ sentinel; replica; leader ] ~f:disconnect)
;;
