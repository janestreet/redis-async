open! Core

module type S = Bulk_io_intf.S

module String                             : S with type t =  string
module Make_stringable (T : Stringable.S) : S with type t := T.t
module Make_binable (T : Binable.S)       : S with type t := T.t
