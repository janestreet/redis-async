open! Core

module T = struct
  type t = Redis.Stream_id.t

  let to_string _t = "<stream id>"
  let of_string = Redis.Stream_id.of_string
end

include T
include Sexpable.Of_stringable (T)
