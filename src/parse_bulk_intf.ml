(** Many RESP3 replies to Redis commands are composed of bulk strings. There are a limited
    number of common patterns that are used by many commands.

    Provide a set of common parsers for a bulk string type that can be used to handle bulk
    command responses efficiently.
*)

open Core

module type S = sig
  type t

  (** Always a single bulk string. Example: ECHO *)
  val single       : ([> read ], Iobuf.seek) Iobuf.t -> t                          Or_error.t

  (** A optional single bulk string. Example: GET *)
  val single_opt   : ([> read ], Iobuf.seek) Iobuf.t -> t              option      Or_error.t

  (** A list of optional single bulk strings. Example: MGET *)
  val list_opt     : ([> read ], Iobuf.seek) Iobuf.t -> t              option list Or_error.t

  (** A list of single bulk strings. Example: KEYS *)
  val list         : ([> read ], Iobuf.seek) Iobuf.t -> t              list        Or_error.t

  (** A set of single bulk strings. Represented as a list for simplicity, and to avoid
      requiring all value implementations implement Comparable. Example: SMEMBERS. *)
  val set          : ([> read ], Iobuf.seek) Iobuf.t -> t              list        Or_error.t

  (** An int followed by a list. Example: SCAN *)
  val int_and_list : ([> read ], Iobuf.seek) Iobuf.t -> (int * t list)             Or_error.t
end
