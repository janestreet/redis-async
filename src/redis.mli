open! Core
module Bulk_io   = Bulk_io
module Resp3     = Resp3
module Key_event = Key_event
module Cursor    = Cursor

module type S = Redis_intf.S

module Make (Key : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Field = Bulk_io.String and module Value = Value

module Make_field (Key : Bulk_io.S) (Field : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Field = Field and module Value = Value
