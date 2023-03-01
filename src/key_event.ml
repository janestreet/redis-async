open Core

type t =
  [ `del
  | `expire
  | `new_
  | `expired
  ]
[@@deriving sexp_of]

let to_string = function
  | `del     -> "del"
  | `expire  -> "expire"
  | `expired -> "expired"
  | `new_    -> "new"
;;

let of_string = function
  | "del"     -> `del
  | "expire"  -> `expire
  | "new"     -> `new_
  | "expired" -> `expired
  | str       -> raise_s [%message [%here] "Unexpected" (str : string)]
;;
