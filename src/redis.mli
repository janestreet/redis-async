open! Core
module Bulk_io = Bulk_io
module Resp3   = Resp3

module type S_generic = Redis_intf.S_generic
module type S         = Redis_intf.S
module type S_hash    = Redis_intf.S_hash

module Make (Key : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Value = Value

module Make_hash (Key : Bulk_io.S) (Field : Bulk_io.S) (Value : Bulk_io.S) :
  S_hash with module Key = Key and module Field = Field and module Value = Value
