type pitch_detection

val pitch_detection :
  string ->
  P5.Sound.audio_context ->
  P5.Sound.AudioIn.stream ->
  (unit -> unit) ->
  pitch_detection

val get_pitch :
  pitch_detection ->
  ((float option, Jv.t) result -> unit) ->
  unit
