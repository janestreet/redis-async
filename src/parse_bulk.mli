module Make (T : Bulk_io_intf.S) : Parse_bulk_intf.S with type t := T.t
