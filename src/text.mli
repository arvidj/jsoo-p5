type horiz_align = Left | Center | Right
type vert_align = Top | Bottom | Center | Baseline

val align : ?sketch:Sketch.t -> ?vert_align:vert_align -> horiz_align -> unit
val set_size : ?sketch:Sketch.t -> int -> unit
val get_size : ?sketch:Sketch.t -> unit -> int

val draw :
  ?sketch:Sketch.t ->
  ?x:float ->
  ?y:float ->
  ?max_width:float ->
  ?max_height:float ->
  string ->
  unit
