module Make (T : Bulk_io_intf.S) : Parse_bulk_intf.S with type t = T.t

module Make_map (K : Parse_bulk_intf.S) (V : Parse_bulk_intf.S) :
  Parse_bulk_intf.S_map with type key := K.t and type value := V.t
