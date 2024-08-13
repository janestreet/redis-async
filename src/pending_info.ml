open! Core

module Extended = struct
  type t =
    { stream_id : Stream_id.t
    ; owner : Consumer.t
    ; since_last_delivery : Time_ns.Span.t
    ; delivery_count : int
    }
  [@@deriving sexp_of]

  let of_resp3 = function
    | Resp3.(
        Array
          [| String stream_id
           ; String owner
           ; Int ms_since_last_delivery
           ; Int delivery_count
          |]) ->
      Ok
        { stream_id = Stream_id.of_string stream_id
        ; owner = Consumer.of_string owner
        ; since_last_delivery = Time_ns.Span.of_int_ms ms_since_last_delivery
        ; delivery_count
        }
    | resp3 ->
      Or_error.error_s
        [%message [%here] "Unexpected pending info format" (resp3 : Resp3.t)]
  ;;
end
