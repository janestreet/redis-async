(** RESP3 protocol parsing

    Test inputs are copy/pasted from the protocol spec as much as possible:
    https://github.com/antirez/RESP3/blob/master/spec.md *)

open Core
open Redis

let istr str =
  (* prefix with garbage to test that I don't assume zero offset anywhere *)
  let buf = Iobuf.of_string ("garbage\r\n" ^ str) in
  Iobuf.advance buf 9;
  buf
;;

let p buf = print_s ([%sexp_of: Resp3.t] (Resp3.parse_exn buf))

let%expect_test "Simple String" =
  let buf = istr "+hello world\r\n" in
  p buf;
  [%expect {| (String "hello world") |}]
;;

let%expect_test "Blob String" =
  let buf = istr "$11\r\nhello world\r\n" in
  p buf;
  [%expect {| (String "hello world") |}]
;;

let%expect_test "Simple Error" =
  let buf = istr "-ERR this is the error description\r\n" in
  p buf;
  [%expect {| (Error "ERR this is the error description") |}]
;;

let%expect_test "Number" =
  let buf = istr ":1234\r\n" in
  p buf;
  [%expect {| (Int 1234) |}]
;;

let%expect_test "Null" =
  let buf = istr "_\r\n" in
  p buf;
  [%expect {| Null |}]
;;

let%expect_test "Double" =
  let buf = istr ",1.23\r\n" in
  p buf;
  [%expect {| (Double 1.23) |}];
  let buf = istr ",inf\r\n" in
  p buf;
  [%expect {| (Double INF) |}];
  let buf = istr ",-inf\r\n" in
  p buf;
  [%expect {| (Double -INF) |}]
;;

let%expect_test "Boolean" =
  let buf = istr "#t\r\n" in
  p buf;
  [%expect {| (Boolean true) |}]
;;

let%expect_test "Blob Error" =
  let buf = istr "!21\r\nSYNTAX invalid syntax\r\n" in
  p buf;
  [%expect {| (Error "SYNTAX invalid syntax") |}]
;;

let%expect_test "Verbatim String" =
  let buf = istr "=15\r\ntxt:Some string\r\n" in
  p buf;
  [%expect {| (String "txt:Some string") |}]
;;

let%expect_test "Big Number" =
  let buf = istr "(3492890328409238509324850943850943825024385\r\n" in
  p buf;
  [%expect {| (Bignum 3492890328409238509324850943850943825024385) |}]
;;

let%expect_test "Array" =
  let buf = istr "*3\r\n:1\r\n:2\r\n:3\r\n" in
  p buf;
  [%expect {| (Array ((Int 1) (Int 2) (Int 3))) |}];
  let buf = istr "*2\r\n*3\r\n:1\r\n$5\r\nhello\r\n:2\r\n#f\r\n" in
  p buf;
  [%expect {| (Array ((Array ((Int 1) (String hello) (Int 2))) (Boolean false))) |}]
;;

let%expect_test "Array" =
  let buf = istr "*3\r\n:1\r\n:2\r\n:3\r\n" in
  p buf;
  [%expect {| (Array ((Int 1) (Int 2) (Int 3))) |}];
  let buf = istr "*2\r\n*3\r\n:1\r\n$5\r\nhello\r\n:2\r\n#f\r\n" in
  p buf;
  [%expect {| (Array ((Array ((Int 1) (String hello) (Int 2))) (Boolean false))) |}]
;;

let%expect_test "Array" =
  let buf = istr "%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n" in
  p buf;
  [%expect {| (Map (((String first) (Int 1)) ((String second) (Int 2)))) |}]
;;

let%expect_test "Length check" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    Resp3.parse_exn (Iobuf.of_string "$3\r\nfour"));
  [%expect {| (Redis__Common.Need_more_data) |}]
;;

let%test "short buffer does not end in crlf" =
  not (Resp3.ends_in_crlf (Iobuf.of_string "x"))
;;

let%test "buffer without crlf" =
  not (Resp3.ends_in_crlf (Iobuf.of_string "hello\r\nworld"))
;;

let%test "buffer with crlf" = Resp3.ends_in_crlf (Iobuf.of_string "hello\r\n")
