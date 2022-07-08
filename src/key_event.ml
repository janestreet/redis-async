open Core

type t =
  [ `del
  | `expire
  | `new_
  ]
[@@deriving sexp_of]

let to_string = function
  | `del    -> "del"
  | `expire -> "expire"
  | `new_   -> "new"
;;

let of_string = function
  | "del"    -> `del
  | "expire" -> `expire
  | "new"    -> `new_
  | str      -> raise_s [%message [%here] "Unexpected" (str : string)]
;;
