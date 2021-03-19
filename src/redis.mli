open! Core
module Bulk_io = Bulk_io
module Resp3   = Resp3

module type S = Redis_intf.S

module Make (Key : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Value = Value
