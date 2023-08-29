open Core

module T = struct
  include String

  let module_name = "Redis.Consumer"
end

include T
include Identifiable.Make_plain (T)
