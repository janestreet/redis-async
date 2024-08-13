open Core

module T = struct
  include String

  let module_name = "Redis.Stream_id"
end

include T
include Identifiable.Make_plain (T)

let to_time_exn t =
  let epoch_ms = String.split t ~on:'-' |> List.hd_exn |> Int.of_string in
  Time_ns.of_span_since_epoch (Time_ns.Span.of_int_ms epoch_ms)
;;

let zero = "0-0"
