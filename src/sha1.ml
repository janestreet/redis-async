open Core
include String

include Identifiable.Make_plain (struct
    let module_name = "Redis.Sha1"

    include String
  end)
