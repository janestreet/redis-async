open! Core
module Auth = Auth
module Bulk_io = Bulk_io
module Consumer = Consumer
module Consumer_info = Consumer_info
module Cursor = Cursor
module Group = Group
module Key_event = Key_event
module Pending_info = Pending_info
module Resp3 = Resp3
module Role = Role
module Sentinel = Sentinel
module Sha1 = Sha1
module Stream_id = Stream_id

type ('a, 'key, 'field, 'value) t = ('a, 'key, 'field, 'value) Client.t

module type S = Redis_intf.S

module Make (Key : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Field = Bulk_io.String and module Value = Value

module Make_field (Key : Bulk_io.S) (Field : Bulk_io.S) (Value : Bulk_io.S) :
  S with module Key = Key and module Field = Field and module Value = Value

(* A client that operates only on strings. This is used in many places where a basic
   client is all that is needed. *)
module String :
  S
  with module Key = Bulk_io.String
   and module Field = Bulk_io.String
   and module Value = Bulk_io.String
