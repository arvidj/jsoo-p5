open P5

(*

  - [ ] Present average score on screen
  - [ ] Restrict the set of notes
  - [ ] Add a timer for 3 minutes sessions (present final average)
  - [ ] Detect errors

A simple model for increasing the set of notes trained on:

Define a ordering on the notes (the order in which they shall be learned):
 - notes[0] .. notes[6*13 - 1]
such that notes[0] is the first note to learn.

Let learnt : int be the index of the last note which is not yet learnt.

Let avg_score[i] be the avg_score of the ith note.
 - This can be defined as the weighted moving average of the last set of scores for that note

Let repeats[i] be the number of times the ith note has been repeated.

After each repeat of note j with scores:
  repeats[j]++
  update avg_score[j]

  if for all i in 0 .. i: repeats[i] >= repeat_threshold && avg_score[i] >= score_threshold:
    learnt++

*)

let dbg fmt = Printf.ksprintf (fun s -> Brr.Console.(debug [ str s ])) fmt

type config = {
  mic_threshold : float;
  freq_threshold : float;
  sustain_treshold : int;
}

let config =
  { mic_threshold = 0.0010; freq_threshold = 3.0; sustain_treshold = 50 }

module C = struct
  let black () = Color.make_string "black"
  let white () = Color.make_string "white"
  let red () = Color.make_string "red"
  let green () = Color.make_string "green"
  let blue () = Color.make_string "blue"
end

let model_url =
  "https://cdn.jsdelivr.net/gh/ml5js/ml5-data-and-models/models/pitch-detection/crepe/"

type sound = { frequency : float; level : float }

type state =
  | Loading
  | Listening of {
      mutable sound : sound;
      mutable target : Note.t;
      mutable target_start : int;
      mutable sustain : int option;
      mutable scores : int list;
    }

let state : state ref = ref Loading

let setup () =
  Canvas.create 400 600 |> ignore;
  let audio_context = Sound.get_audio_context () in
  let mic = Sound.AudioIn.make () in
  Sound.AudioIn.start mic @@ fun () ->
  dbg "Listening";
  let stream = Sound.AudioIn.stream mic in
  let pitch_detection =
    let pitch_detection = ref None in
    fun fn ->
      let rec aux () =
        match !pitch_detection with
        | None ->
            let model_loaded () =
              dbg "model loaded";
              aux ()
            in
            dbg "Loading model...";
            pitch_detection :=
              Some
                (Ml5.pitch_detection model_url audio_context stream model_loaded)
        | Some pitch_detection -> Ml5.get_pitch pitch_detection fn
      in
      aux ()
  in
  let rec got_pitch = function
    | Error err ->
        Brr.Console.(error [ str "[got_pitch] error"; err ]) |> ignore
    | Ok frequency_opt ->
        (match frequency_opt with
        | Some frequency -> (
            (*             dbg "Frequency: %f" frequency; *)
            let level = Sound.AudioIn.get_level mic in
            let sound = { frequency; level } in
            match !state with
            | Loading ->
                state :=
                  Listening
                    {
                      sound;
                      target = Note.random ();
                      target_start = Time.millis ();
                      sustain = None;
                      scores = [];
                    }
            | Listening state -> state.sound <- sound)
        | None -> ());
        pitch_detection got_pitch
  in
  pitch_detection got_pitch

let draw () =
  let width = Canvas.(width ()) in
  let height = Canvas.(height ()) in
  let now = Time.millis () in
  match !state with
  | Loading ->
      Color.background (C.black ());
      Color.fill (C.white ());
      Text.(
        align ~vert_align:Center Center;
        set_size 24;
        draw "Loading..." ~x:(width /. 2.0) ~y:(height /. 2.0));
      ()
  | Listening
      ({ sound = { frequency; level }; target; target_start; sustain; scores }
       as state) ->
      let next_note () =
        let score = now - target_start - config.sustain_treshold in
        let scores = score :: scores in
        let avg =
          let sum = List.fold_left ( + ) 0 scores in
          let length = List.length scores in
          float_of_int sum /. float_of_int length
        in
        dbg "score: %d (avg: %f)" score avg;
        state.scores <- scores;
        state.target <- Note.random ~unlike:target ();
        state.target_start <- now;
        state.sustain <- None
      in
      (* Do nothing if too quiet *)
      Color.background (C.black ());
      (* How far the detected note is to the target note *)
      let freq_diff = Float.abs (frequency -. target.frequency) in
      let freq_in_threshold = freq_diff <= config.freq_threshold in
      (* Compute sustain *)
      let () =
        let audible = level >= config.mic_threshold in
        if (not audible) || not freq_in_threshold then (
          if (not audible) && Option.is_some sustain then
            dbg "mic too low %f" level;
          if (not freq_in_threshold) && Option.is_some sustain then
            dbg "frequency off, diff %f"
              Float.(abs (frequency -. target.frequency));
          state.sustain <- None)
        else
          match sustain with None -> state.sustain <- Some now | Some _ -> ()
      in
      (* Print the sustain / compute set next note *)
      (match state.sustain with
      | None -> ()
      | Some start ->
          let hold = now - start in
          (*           dbg "sustain.hold: %d" hold; *)
          Color.fill (C.white ());
          Text.(
            align ~vert_align:Center Center;
            set_size 24;
            draw (string_of_int hold) ~x:(width /. 2.0) ~y:(height -. 200.0));
          if hold >= config.sustain_treshold then (
            dbg "nice!";
            next_note ()));
      (* Print the frequency *)
      let () =
        Color.fill (C.white ());
        Text.(
          align ~vert_align:Center Center;
          set_size 32;
          draw
            (string_of_int @@ Float.to_int frequency)
            ~x:(width /. 2.0) ~y:(height -. 150.0))
      in
      (* Print the closest note *)
      let closest_note = Note.closest frequency in
      let () =
        Color.fill
        @@ if Option.is_some state.sustain then C.green () else C.white ();
        Text.(
          align ~vert_align:Center Center;
          set_size 24;
          draw
            ("got: " ^ Note.to_string_pretty closest_note)
            ~x:(width /. 2.0) ~y:(height -. 100.0))
      in
      (* Print the target note *)
      let () =
        Color.fill (C.white ());
        Text.(
          set_size 64;
          draw
            ("tgt: " ^ Note.to_string_pretty target)
            ~x:(width /. 2.0) ~y:(height -. 50.0))
      in
      (* Draw frequency gauge *)
      let () =
        let alpha = 100 (* TODO *) in
        Shape.rect_mode Center;
        Color.stroke (C.white ());
        Color.stroke_weight 1;
        Color.fill
        @@ if freq_in_threshold then C.green () else Color.make_gray ~alpha 255;
        Shape.rect 200.0 100.0 200.0 50.0;
        (* Draw how far off the mark we are *)
        Color.stroke (C.white ());
        Color.stroke_weight 4;
        Shape.line_2d ~x1:200.0 ~y1:0.0 ~x2:200.0 ~y2:200.0 ();
        Color.no_stroke ();
        Color.fill @@ if freq_in_threshold then C.green () else C.red ();
        Shape.rect (200.0 +. (freq_diff /. 2.0)) 100.0 10.0 75.0
      in
      ()

let () = Sketch.run ~setup ~draw ()
