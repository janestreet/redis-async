open! Core
open Async

(* We want to handle the unusual case of needing more data via exception because
   - Exceptions perform very well
   - The alternative would be a variant return type that you have to allocate and then
     bind at every step of parsing the protocol
*)
exception Need_more_data

let check_length_exn ~len buf = if len > Iobuf.length buf then raise Need_more_data
let write_crlf writer = Writer.write writer "\r\n"

(** Int.to_string is slow. Cache some small values. *)
let itoa =
  let len = 1024 in
  let itoa = Array.init len ~f:Int.to_string in
  fun i -> if i >= 0 && i < len then itoa.(i) else Int.to_string i
;;
