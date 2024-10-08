open! Core
open Or_error.Let_syntax

let extract_data string_map key =
  match Map.find string_map key with
  | None ->
    Or_error.error_s [%message "Missing key in sentinel query response" (key : string)]
  | Some (Resp3.String data) -> Ok data
  | Some unexpected ->
    Or_error.error_s
      [%message
        "Expected a string map value in sentinel query response"
          (key : string)
          (unexpected : Resp3.t)]
;;

module Replica = struct
  type t =
    { name : string
    ; host_and_port : Host_and_port.t
    ; flags : string
    ; master_link_status : string
    ; master_host_and_port : Host_and_port.t
    ; slave_priority : int
    }
  [@@deriving sexp_of]

  let of_string_map string_map =
    let extract_data = extract_data string_map in
    let%bind name = extract_data "name" in
    let%bind replica_host = extract_data "ip" in
    let%bind replica_port = extract_data "port" in
    let%bind flags = extract_data "flags" in
    let%bind master_host = extract_data "master-host" in
    let%bind master_port = extract_data "master-port" in
    let%bind slave_priority = extract_data "slave-priority" in
    let%bind master_link_status = extract_data "master-link-status" in
    let replica_host_and_port =
      Host_and_port.create ~host:replica_host ~port:(Int.of_string replica_port)
    in
    let master_host_and_port =
      Host_and_port.create ~host:master_host ~port:(Int.of_string master_port)
    in
    { name
    ; host_and_port = replica_host_and_port
    ; flags
    ; master_link_status
    ; master_host_and_port
    ; slave_priority = Int.of_string slave_priority
    }
    |> return
  ;;
end

module Leader = struct
  type t =
    { name : string
    ; host_and_port : Host_and_port.t
    ; down_after : Time_ns.Span.t
    ; failover_timeout : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let of_string_map string_map =
    let extract_data = extract_data string_map in
    let%bind name = extract_data "name" in
    let%bind host = extract_data "ip" in
    let%bind port = extract_data "port" in
    let host_and_port = Host_and_port.create ~host ~port:(Int.of_string port) in
    let%bind down_after_milliseconds = extract_data "down-after-milliseconds" in
    let%bind failover_timeout = extract_data "failover-timeout" in
    { name
    ; host_and_port
    ; down_after = Time_ns.Span.of_int_ms (Int.of_string down_after_milliseconds)
    ; failover_timeout = Time_ns.Span.of_int_ms (Int.of_string failover_timeout)
    }
    |> return
  ;;
end
