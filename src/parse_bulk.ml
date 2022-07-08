open Core

let backtrack_and_extract_error buf =
  Iobuf.unsafe_advance buf (-1);
  Error (Resp3.extract_error buf)
;;

let apply_single buf ~f =
  match Resp3.consume_char buf with
  | '$' ->
    let len    = Int.of_string (Resp3.simple_string buf) in
    let result = f ~len buf                              in
    Resp3.expect_crlf buf;
    result
  | _ -> backtrack_and_extract_error buf
;;

module Make (T : Bulk_io_intf.S) = struct
  type t = T.t

  let single buf = apply_single buf ~f:T.Redis_bulk_io.consume

  let single_opt buf =
    match Resp3.consume_char buf with
    | '$' ->
      let len    = Int.of_string (Resp3.simple_string buf) in
      let result = T.Redis_bulk_io.consume buf ~len        in
      Resp3.expect_crlf buf;
      Or_error.map result ~f:Option.some
    | '_' ->
      Resp3.expect_crlf buf;
      Ok None
    | _   -> backtrack_and_extract_error buf
  ;;

  let list_or_set_internal ~parse_each ~expected_initial_char buf =
    match Resp3.consume_char buf with
    | char when Char.(char = expected_initial_char) ->
      let len = Int.of_string (Resp3.simple_string buf) in
      let rec make n l =
        match n with
        | 0 -> Ok (List.rev l)
        | _ ->
          (match parse_each buf with
           | Ok e             -> make (n - 1) (e :: l)
           | Error _ as error -> error)
      in
      make len []
    | _ -> backtrack_and_extract_error buf
  ;;

  let list_internal = list_or_set_internal ~expected_initial_char:'*'
  let list buf      = list_internal ~parse_each:single buf
  let list_opt buf  = list_internal ~parse_each:single_opt buf
  let set_internal  = list_or_set_internal ~expected_initial_char:'~'
  let set buf       = set_internal ~parse_each:single buf

  let cursor_and_list buf =
    match Resp3.consume_char buf with
    | '*' ->
      Resp3.expect_char buf '2';
      Resp3.expect_crlf buf;
      let c =
        match Resp3.parse_exn buf with
        | String c -> Cursor.of_string c
        | r        ->
          raise
            (Resp3.Protocol_error
               (sprintf !"Expected String but received %{sexp#mach:Resp3.t}" r))
      in
      let%map.Or_error l = list_internal ~parse_each:single buf in
      c, l
    | _ -> backtrack_and_extract_error buf
  ;;
end

module Make_map (K : Parse_bulk_intf.S) (V : Parse_bulk_intf.S) = struct
  let map_generic ~expected_initial_char ~decrement buf =
    match Resp3.consume_char buf with
    | char when Char.(char = expected_initial_char) ->
      let len = Int.of_string (Resp3.simple_string buf) in
      let rec make n l =
        match n with
        | 0 -> Ok (List.rev l)
        | _ ->
          let%bind.Or_error key   = K.single buf in
          let%bind.Or_error value = V.single buf in
          make (n - decrement) ((key, value) :: l)
      in
      make len []
    | _ -> backtrack_and_extract_error buf
  ;;

  let map            buf = map_generic ~expected_initial_char:'%' ~decrement:1 buf
  let alternating_kv buf = map_generic ~expected_initial_char:'*' ~decrement:2 buf

  let cursor_and_alternating_key_value buf =
    match Resp3.consume_char buf with
    | '*' ->
      Resp3.expect_char buf '2';
      Resp3.expect_crlf buf;
      let c =
        match Resp3.parse_exn buf with
        | String c -> Cursor.of_string c
        | r        ->
          raise
            (Resp3.Protocol_error
               (sprintf !"Expected String but received %{sexp#mach:Resp3.t}" r))
      in
      let%map.Or_error l = alternating_kv buf in
      c, l
    | _ -> backtrack_and_extract_error buf
  ;;
end
