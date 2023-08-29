open Core

module T = struct
  include String

  let module_name = "Redis.Stream_id"
end

include T
include Identifiable.Make_plain (T)
