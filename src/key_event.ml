open! Core

type t =
  [ `del
  | `expire
  | `new_ [@rename "new"]
  | `expired
  | `set
  | `hset
  | `incrby
  ]
[@@deriving sexp_of, enumerate, string ~capitalize:"snake_case"]
