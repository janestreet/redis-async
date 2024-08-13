open! Core

module Replica = struct
  (* there are more fields in the response but we just extract those we need for now*)
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
    let open Or_error.Let_syntax in
    let extract_data key =
      match Map.find string_map key with
      | None ->
        Or_error.error_s
          [%message "Missing key in sentinel query response" (key : string)]
      | Some (Resp3.String data) -> Ok data
      | Some unexpected ->
        Or_error.error_s
          [%message
            "Expected a string map value in sentinel query response"
              (key : string)
              (unexpected : Resp3.t)]
    in
    let%bind name = extract_data "name" in
    let%bind replica_host = extract_data "ip" in
    let%bind replica_port = extract_data "port" in
    let%bind flags = extract_data "flags" in
    let%bind master_host = extract_data "master-host" in
    let%bind master_port = extract_data "master-port" in
    let%bind slave_priority = extract_data "slave-priority" in
    let%map master_link_status = extract_data "master-link-status" in
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
  ;;
end
