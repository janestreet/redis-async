# Redis Async

A Redis client library for OCaml Async applications. Provides a
strongly-typed API with transparent (de)serialization for
application-defined types. Includes support for client tracking.

Compatible with Redis versions 6 and higher. Internally, implements
the RESP3 protocol.

## Example Usage

Here are a few basic examples. Please see the included tests for additional
detail and features.

### Plain

Redis natively works with plain strings for many commands. Users of
other Redis clients will be most familiar this style of interaction.

```ocaml
open Core
open Async
open Deferred.Or_error.Let_syntax

module Redis = Redis.Make (Redis.Bulk_io.String) (Redis.Bulk_io.String) in
let%bind redis =
  Redis.create
    ~where_to_connect:
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:"localhost" ~port:6379))
    ()
in
let%map reply = Redis.echo redis "Hello, world!" in
print_endline reply
```

### Typeful 

Application-defined types can be used directly as Redis keys and
values. Convenience functors that implement the required
(de)serialization for Stringable and Binable types are provided.

```ocaml
open Core
open Async
open Deferred.Or_error.Let_syntax

module Key = struct
  module T = struct
    type t = { name : string }

    let to_string { name } = name
    let of_string name = { name }
  end

  include T
  include Redis.Bulk_io.Make_stringable (T)
end

module Value = struct
  module T = struct
    type t =
      { color  : string
      ; number : int
      }
    [@@deriving bin_io, sexp]
  end

  include T
  include Redis.Bulk_io.Make_binable (T)
end

module Redis = Redis.Make (Key) (Value) in
let%bind redis =
  Redis.create
    ~where_to_connect:
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:"localhost" ~port:6379))
    ()
in
let key   = { Key.name    = "Jane"              } in
let value = { Value.color = "blue"; number = 42 } in
let%bind ()    = Redis.set redis key value in
let%map  value = Redis.get redis key       in
print_s ([%sexp_of: Value.t option] value)
```
