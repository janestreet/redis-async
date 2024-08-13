open! Core

type t =
  { name : Consumer.t
  ; pending : int
  ; idle : Time_ns.Span.t
  ; inactive : Time_ns.Span.t option
  }
[@@deriving sexp_of]

let of_resp3 = function
  | Resp3.(
      Map
        [| (String "name", String name)
         ; (String "pending", Int pending)
         ; (String "idle", Int idle)
         ; (String "inactive", Int inactive)
        |]) ->
    Ok
      { name = Consumer.of_string name
      ; pending
      ; idle = Time_ns.Span.of_int_ms idle
      ; inactive = Time_ns.Span.of_int_ms inactive |> Some
      }
  | Resp3.(
      Map
        [| (String "name", String name)
         ; (String "pending", Int pending)
         ; (String "idle", Int idle)
        |]) ->
    Ok
      { name = Consumer.of_string name
      ; pending
      ; idle = Time_ns.Span.of_int_ms idle
      ; inactive = None
      }
  | resp3 ->
    Or_error.error_s
      [%message [%here] "Unexpected consumer info format" (resp3 : Resp3.t)]
;;

let last_successful_interaction t =
  match t.inactive with
  | Some inactive -> inactive
  | None -> t.idle
;;

let last_attempted_interaction t =
  match t.inactive with
  | Some _ -> Some t.idle
  | None -> None
;;
