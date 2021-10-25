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

let handle_unexpected : Resp3.t -> 'a Or_error.t = function
  | Resp3.Error err -> Or_error.error_string err
  | unexpected      -> Or_error.errorf !"Unexpected: %{sexp#mach:Resp3.t}" unexpected
;;

let create_ok () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | String "OK" -> Ok ()
    | other       -> handle_unexpected other)
;;

let create_int () =
  create (fun buf ->
    match Resp3.parse_exn buf with
    | Int i -> Ok i
    | other -> handle_unexpected other)
;;

let create_resp3 () = create (fun buf -> Ok (Resp3.parse_exn buf))

let parse_01_bool = function
  | Resp3.Int 0 -> Ok false
  | Int 1       -> Ok true
  | other       -> handle_unexpected other
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
    | other -> handle_unexpected other)
;;
