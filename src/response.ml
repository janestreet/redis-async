open Core
open Async

let create (type a) (parse : (read, Iobuf.seek) Iobuf.t -> a Or_error.t) =
  (module struct
    type t = a

    let this  : a Or_error.t Ivar.t = Ivar.create ()
    let parse buf                   = parse (Iobuf.read_only buf)
  end : Response_intf.S
    with type t = a)
;;

let handle_unexpected_response ~expected : Resp3.t -> 'a Or_error.t = function
  | Resp3.Error err -> Or_error.error_string err
  | got ->
    Or_error.error_s [%message "Unexpected response" (expected : string) (got : Resp3.t)]
;;

let create_ok () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | String "OK" -> Ok ()
    | other       -> handle_unexpected_response ~expected:"Ok" other)
;;

let create_int () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | Int i -> Ok i
    | other -> handle_unexpected_response ~expected:"int" other)
;;

let create_resp3 () = create (fun buf -> Ok (Resp3.parse_exn buf))

let parse_01_bool = function
  | Resp3.Int 0 -> Ok false
  | Int 1       -> Ok true
  | other       -> handle_unexpected_response ~expected:"bool" other
;;

let create_01_bool () = create (fun buf -> Resp3.parse_exn buf |> parse_01_bool)

let create_01_bool_list () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | Array array ->
      let%map.Or_error result =
        Array.fold_result array ~init:[] ~f:(fun acc value ->
          let%map.Or_error bool = parse_01_bool value in
          bool :: acc)
      in
      List.rev result
    | other -> handle_unexpected_response ~expected:"bool list" other)
;;

let create_subscription ~channel =
  create (fun buf ->
    let expected = "subscription" in
    match Resp3.parse_exn buf with
    | String s when String.(s = channel) ->
      (match Resp3.parse_exn buf with
       | Int i -> Ok i
       | other -> handle_unexpected_response ~expected other)
    | other -> handle_unexpected_response ~expected other)
;;

let create_string () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | String s -> Ok s
    | other    -> handle_unexpected_response ~expected:"string" other)
;;

let create_host_and_port () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | Array [| Resp3.String host; Resp3.String port |] ->
      let%map.Or_error port = Or_error.try_with (fun () -> Int.of_string port) in
      Host_and_port.create ~host ~port
    | other -> handle_unexpected_response ~expected:"where to connect" other)
;;

let create_role () = create (fun buf -> Role.of_resp3 (Resp3.parse_exn buf))

let create_string_map () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | Map pairs ->
      Array.fold_result pairs ~init:String.Map.empty ~f:(fun acc pair ->
        match pair with
        | String key, data -> Map.set acc ~key ~data |> Or_error.return
        | other, _         -> handle_unexpected_response ~expected:"string key" other)
    | other -> handle_unexpected_response ~expected:"string map" other)
;;
