open Core

type t =
  [ `del
  | `expire
  | `new_
  | `expired
  | `set
  | `hset
  ]
[@@deriving sexp_of, enumerate]

let to_string = function
  | `del -> "del"
  | `expire -> "expire"
  | `expired -> "expired"
  | `new_ -> "new"
  | `set -> "set"
  | `hset -> "hset"
;;

let of_string = function
  | "del" -> `del
  | "expire" -> `expire
  | "new" -> `new_
  | "expired" -> `expired
  | "set" -> `set
  | "hset" -> `hset
  | str -> raise_s [%message [%here] "Unexpected" (str : string)]
;;
