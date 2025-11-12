(** Many RESP3 replies to Redis commands are composed of bulk strings. There are a limited
    number of common patterns that are used by many commands.

    Provide a set of common parsers for a bulk string type that can be used to handle bulk
    command responses efficiently. *)

open Core

module type S = sig
  type t

  (** Always a single bulk string. Example: ECHO *)
  val single : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t Or_error.t

  (** A optional single bulk string. Example: GET *)
  val single_opt : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t option Or_error.t

  (** A list of optional single bulk strings. Example: MGET *)
  val list_opt : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t option list Or_error.t

  (** A list of single bulk strings. Example: KEYS *)
  val list : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t list Or_error.t

  (** A set of single bulk strings. Represented as a list for simplicity, and to avoid
      requiring all value implementations implement Comparable. Example: SMEMBERS. *)
  val set : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> t list Or_error.t

  (** A cursor followed by a list. Example: SCAN *)
  val cursor_and_list
    :  ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
    -> (Cursor.t * t list) Or_error.t

  (** An array of tuples of [value * score]. Example: [ZRANGEBYSCORE ... WITHSCORES] *)
  val with_scores
    :  ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
    -> (t * [ `Score of float ]) list Or_error.t
end

(** The term "map" here refers to a map node in the RESP3 protocol, which is represented
    as an associative list. *)
module type S_map = sig
  type key
  type value

  (** field/value pairs. Example: HGETALL *)
  val map : ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t -> (key * value) list Or_error.t

  (** A flat array containing repeating key value pairs. Example: XRANGE *)
  val alternating_kv
    :  ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
    -> (key * value) list Or_error.t

  (** A cursor followed by field/value pairs. Example: HSCAN *)
  val cursor_and_alternating_key_value
    :  ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
    -> (Cursor.t * (key * value) list) Or_error.t
end
