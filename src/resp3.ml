(** Development note:

    There are both omissions and errors in the spec. When in doubt, try sniffing the
    protocol live by putting socat in the middle:

    Start a Redis
    $ socat -v -d -d TCP-LISTEN:6380,bind=127.0.0.1,fork,reuseaddr TCP:localhost:6379 |& cat -A |sed 's/\$$/\\n/'
    $ redis-cli -3 -p 6380
*)

open Core

type t =
  | String  of string
  | Error   of string
  | Int     of int
  | Null
  | Double  of float
  | Boolean of bool
  | Bignum  of Bignum.t
  | Array   of t array
  | Map     of (t * t) array
  | Set     of t array
[@@deriving compare, equal, sexp_of]

exception Termination_not_found
exception Protocol_error of string

let consume_char buf =
  Common.check_length_exn ~len:1 buf;
  Iobuf.Unsafe.Consume.char buf
;;

let crlf = Iobuf.(Consume.uint16_le (of_string "\r\n"))

let expect_crlf buf =
  Common.check_length_exn ~len:2 buf;
  if Iobuf.Unsafe.Consume.uint16_le buf <> crlf then raise Termination_not_found
;;

let ends_in_crlf buf =
  let pos = Iobuf.length buf - 2 in
  pos >= 0 && Iobuf.Unsafe.Peek.uint16_le buf ~pos = crlf
;;

let expect_char buf expected =
  let observed = consume_char buf in
  if Char.(observed <> expected)
  then
    raise (Protocol_error (sprintf "Expected '%c' but observed '%c'" expected observed))
;;

let peek_char buf =
  Common.check_length_exn ~len:1 buf;
  Iobuf.Unsafe.Peek.char ~pos:0 buf
;;

let rec len_to_crlf pos buf =
  let next = pos + 1 in
  if next = Iobuf.Expert.hi buf then raise Termination_not_found;
  if Iobuf.Unsafe.Peek.uint16_le buf ~pos = crlf then pos else len_to_crlf next buf
;;

let len_to_crlf buf = len_to_crlf 0 buf

let rec simple_string buf =
  let str = Iobuf.Unsafe.Consume.stringo buf ~len:(len_to_crlf buf) in
  expect_crlf buf;
  str

and blob_string buf =
  let len = Int.of_string (simple_string buf) in
  Common.check_length_exn ~len buf;
  let str = Iobuf.Unsafe.Consume.stringo buf ~len in
  expect_crlf buf;
  str

and number buf =
  Int.of_string (simple_string buf)

and null buf =
  expect_crlf buf;
  Null

and double buf = Float.of_string (simple_string buf)

and boolean buf =
  let observed = consume_char buf in
  let tf =
    match observed with
    | 't' -> true
    | 'f' -> false
    | _   ->
      raise (Protocol_error (sprintf "Expected 't' or 'f' but observed '%c'" observed))
  in
  expect_crlf buf;
  tf

and big_number buf = Bignum.of_string (simple_string buf)

and array buf =
  let e = number buf in
  Array.init e ~f:(fun _ -> parse_exn buf)

and map buf =
  let e = number buf in
  Array.init e ~f:(fun _ ->
    let fst = parse_exn buf in
    let snd = parse_exn buf in
    fst, snd)

and parse_exn buf =
  match consume_char buf with
  | '$' -> String  (blob_string buf)
  | '+' -> String  (simple_string buf)
  | '-' -> Error   (simple_string buf)
  | ':' -> Int     (number buf)
  | '_' -> null    buf
  | ',' -> Double  (double buf)
  | '#' -> Boolean (boolean buf)
  | '!' -> Error   (blob_string buf)
  | '=' -> String  (blob_string buf)
  | '(' -> Bignum  (big_number buf)
  | '*' -> Array   (array buf)
  | '%' -> Map     (map buf)
  | '~' -> Set     (array buf)
  | c   -> raise   (Protocol_error (sprintf "Unknown message type '%c'" c))
;;

let extract_error buf =
  match parse_exn buf with
  | Error err  -> Error.of_string err
  | unexpected -> Error.createf !"Unexpected response: %{sexp#mach:t}" unexpected
;;
