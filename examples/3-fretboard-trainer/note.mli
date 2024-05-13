type t = { note : string; frequency : float }

val diff : t -> t -> float
val closest : float -> t
val random : ?unlike:t -> unit -> t
val to_string_pretty : t -> string
