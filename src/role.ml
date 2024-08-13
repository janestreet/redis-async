open Core

module Replica = struct
  module Connection_state = struct
    type t =
      | Connect
      | Connecting
      | Sync
      | Connected
      | Handshake
      (** [Handshake] Undocumented. Can happen if the replica is unable to connect to the leader. *)
    [@@deriving sexp_of, compare]

    let of_string = function
      | "connect" -> Ok Connect
      | "connecting" -> Ok Connecting
      | "sync" -> Ok Sync
      | "connected" -> Ok Connected
      | "handshake" -> Ok Handshake
      | connection_state ->
        Or_error.error_s
          [%message "Unrecognized connection state" (connection_state : string)]
    ;;
  end

  type t =
    { leader : Host_and_port.t
    ; connection_state : Connection_state.t
    ; replication_offset : int
    }
  [@@deriving sexp_of, compare]

  let keyword = "slave"

  let of_resp3 = function
    | [ Resp3.String k
      ; Resp3.String host
      ; Resp3.Int port
      ; Resp3.String connection_state
      ; Resp3.Int replication_offset
      ]
      when String.equal k keyword ->
      let%bind.Or_error connection_state = Connection_state.of_string connection_state in
      let leader = Host_and_port.create ~host ~port in
      Ok { leader; connection_state; replication_offset }
    | resp3 -> Or_error.error_s [%message "Invalid Replica resp3:" (resp3 : Resp3.t list)]
  ;;

  module For_test_with_deterministic_sexp = struct
    type nonrec t = t =
      { leader : Host_and_port.Hide_port_in_test.t
      ; connection_state : Connection_state.t
      ; replication_offset : (int[@sexp.opaque])
      }
    [@@deriving compare, sexp_of]
  end
end

module Leader = struct
  module Replica = struct
    type t =
      { where_to_connect : Host_and_port.t
      ; replication_offset : int
      }
    [@@deriving sexp_of, compare]

    let of_resp3 resp3 =
      match resp3 with
      | Resp3.Array
          [| Resp3.String host; Resp3.String port; Resp3.String replication_offset |] ->
        let%bind.Or_error port = Or_error.try_with (fun () -> Int.of_string port) in
        let%bind.Or_error replication_offset =
          Or_error.try_with (fun () -> Int.of_string replication_offset)
        in
        let where_to_connect = Host_and_port.create ~host ~port in
        Ok { where_to_connect; replication_offset }
      | resp3 ->
        Or_error.error_s [%message "Invalid Leader Replica resp3:" (resp3 : Resp3.t)]
    ;;

    module For_test_with_deterministic_sexp = struct
      type nonrec t = t =
        { where_to_connect : Host_and_port.Hide_port_in_test.t
        ; replication_offset : (int[@sexp.opaque])
        }
      [@@deriving compare, sexp_of]
    end
  end

  type t =
    { replication_offset : int
    ; replicas : Replica.t list
    }
  [@@deriving sexp_of, compare]

  let keyword = "master"

  let of_resp3 = function
    | [ Resp3.String k; Resp3.Int replication_offset; Resp3.Array replicas ]
      when String.equal k keyword ->
      let%bind.Or_error replicas =
        Array.to_list replicas |> List.map ~f:Replica.of_resp3 |> Or_error.all
      in
      Ok { replication_offset; replicas }
    | resp3 -> Or_error.error_s [%message "Invalid Leader resp3:" (resp3 : Resp3.t list)]
  ;;

  module For_test_with_deterministic_sexp = struct
    type nonrec t = t =
      { replication_offset : (int[@sexp.opaque])
      ; replicas : Replica.For_test_with_deterministic_sexp.t list
      }
    [@@deriving compare, sexp_of]
  end
end

module Sentinel = struct
  type t = string list [@@deriving sexp_of, compare]

  let keyword = "sentinel"

  let of_resp3 = function
    | [ Resp3.String k; Resp3.Array leader_names ] when String.equal k keyword ->
      let l = Array.to_list leader_names in
      let leader_names, other =
        List.partition_map l ~f:(function
          | Resp3.String s -> Either.First s
          | other -> Either.Second other)
      in
      if not (List.is_empty other)
      then
        Or_error.error_s
          [%message
            "Not all leader names are strings:"
              (leader_names : string list)
              (other : Resp3.t list)]
      else Ok leader_names
    | resp3 ->
      Or_error.error_s [%message "Invalid Sentinel resp3:" (resp3 : Resp3.t list)]
  ;;
end

type t =
  | Leader of Leader.t
  | Replica of Replica.t
  | Sentinel of Sentinel.t
[@@deriving sexp_of, compare]

let of_resp3 = function
  | Resp3.Array a ->
    let l = Array.to_list a in
    (match List.hd l with
     | Some (Resp3.String x) when String.equal x Replica.keyword ->
       Replica.of_resp3 l |> Or_error.map ~f:(fun r -> Replica r)
     | Some (Resp3.String x) when String.equal x Leader.keyword ->
       Leader.of_resp3 l |> Or_error.map ~f:(fun l -> Leader l)
     | Some (Resp3.String x) when String.equal x Sentinel.keyword ->
       Sentinel.of_resp3 l |> Or_error.map ~f:(fun s -> Sentinel s)
     | Some role ->
       Or_error.error_s [%message "Invalid Redis role type provided" (role : Resp3.t)]
     | None -> Or_error.error_s [%message "No Redis role type provided"])
  | other -> Or_error.error_s [%message "Unexpected role response:" (other : Resp3.t)]
;;

module For_test_with_deterministic_sexp = struct
  type nonrec t = t =
    | Leader of Leader.For_test_with_deterministic_sexp.t
    | Replica of Replica.For_test_with_deterministic_sexp.t
    | Sentinel of Sentinel.t
  [@@deriving compare, sexp_of]
end
