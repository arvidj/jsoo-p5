open Brr

(** Bindings to the {{:https://p5js.org/reference/#/libraries/p5.sound}p5.sound library} *)

module AudioIn = struct
  type t = Jv.t

  let make () =
    let p5 = Jv.get (Window.to_jv G.window) "p5" in
    let audio_in = Jv.get p5 "AudioIn" in
    Jv.new' audio_in [| |]

  let start audio_in fn =
    Jv.call audio_in "start"  [| Jv.repr fn |] |> ignore

  let stream audio_in =
    Jv.get audio_in "stream"

  let get_level ?smoothing audio_in =
    let smoothing = Jv.of_option ~none:Jv.null Jv.of_float smoothing  in
    Jv.call audio_in "getLevel"  [| smoothing |] |> Jv.to_float
end

type audio_context = Jv.t

let get_audio_context ?sketch () : audio_context =
  Sketch.call sketch "getAudioContext" [||]