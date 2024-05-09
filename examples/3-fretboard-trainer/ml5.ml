let ml5 () =
  let open Brr in
  Jv.get (Window.to_jv G.window) "ml5"

type pitch_detection = Jv.t

(* https://learn.ml5js.org/#/reference/pitch-detection *)
let pitch_detection model_url audio_context stream on_model_loaded :
    pitch_detection =
  let ml5 = ml5 () in
  Jv.call ml5 "pitchDetection"
    [| Jv.of_string model_url; audio_context; stream; Jv.repr on_model_loaded |]

let get_pitch pitch_detection fn =
  let callback error frequency =
    let result =
      if Jv.is_some error then Error error
      else Ok Jv.(to_option to_float frequency)
    in
    fn result
  in
  Jv.call pitch_detection "getPitch" [| Jv.repr callback |] |> ignore
