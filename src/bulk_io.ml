open Core
open Async

module type S = Bulk_io_intf.S

module Make_binable (T : Binable.S) = struct
  module Redis_bulk_io = struct
    let length = T.bin_size_t

    let consume ~len buf =
      Common.check_length_exn ~len buf;
      let pos_ref = ref (Iobuf.Expert.lo buf) in
      let bstr    = Iobuf.Expert.buf buf      in
      match T.bin_reader_t.read bstr ~pos_ref with
      | result ->
        let consumed = !pos_ref - Iobuf.Expert.lo buf in
        Iobuf.unsafe_advance buf len;
        if len <> consumed
        then Or_error.errorf "Bin_prot should have read %i bytes but read %i" len consumed
        else Or_error.return result
      | exception exn ->
        Iobuf.unsafe_advance buf len;
        Or_error.of_exn exn
    ;;

    let write ~len writer t =
      Writer.write_bin_prot_no_size_header writer ~size:len T.bin_write_t t
    ;;

    let to_string t = Binable.to_string (module T) t
    let of_string t = Binable.of_string (module T) t
  end
end

module Make_stringable (T : Stringable.S) = struct
  module Redis_bulk_io = struct
    let length t = String.length (T.to_string t)

    let consume ~len buf =
      Common.check_length_exn ~len buf;
      let str = Iobuf.Unsafe.Consume.stringo ~len buf in
      match T.of_string str with
      | t             -> Or_error.return t
      | exception exn -> Or_error.of_exn exn
    ;;

    let write ~len writer t = Writer.write writer ~len (T.to_string t)
    let to_string           = T.to_string
    let of_string           = T.of_string
  end
end

module String = struct
  include String
  include Make_stringable (String)
end

module Int = struct
  include Int
  include Make_stringable (Int)
end

module Float = struct
  include Float
  include Make_stringable (Float)
end
