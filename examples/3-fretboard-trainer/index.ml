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
let sf = Printf.sprintf

type config = {
  mic_threshold : float;
  freq_threshold : float;
  sustain_treshold : int; (* Time of each game in milliseconds *)
  session_time : int;
}

let config =
  {
    mic_threshold = 0.0010;
    freq_threshold = 3.0;
    sustain_treshold = 50;
    session_time = 180 * 1000;
  }

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

module Scores = struct
  type t = int list

  let average scores =
    let sum = List.fold_left ( + ) 0 scores in
    let length = List.length scores in
    float_of_int sum /. float_of_int length

  let add scores score = score :: scores
  let empty = []
  let length = List.length
end

type state =
  | Loading
  | Listening of {
      synth : Sound.MonoSynth.t;
      game_start : int;
      mutable sound : sound;
      mutable target : Note.t;
      mutable target_start : int;
      mutable sustain : int option;
      mutable scores : Scores.t;
    }
  | End_screen of { scores : Scores.t }

let state : state ref = ref Loading
let teardown : (unit -> unit) list ref = ref []
let register_teardown fn = teardown := fn :: !teardown

let setup () =
  Canvas.create 400 600 |> ignore;
  let synth = Sound.MonoSynth.make () in
  let audio_context = Sound.get_audio_context () in
  let mic = Sound.AudioIn.make () in
  Sound.AudioIn.start mic @@ fun () ->
  dbg "Listening";
  register_teardown (fun () ->
      dbg "Stopping mic";
      Sound.AudioIn.stop mic);
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
                (* The game starts *)
                Sound.MonoSynth.play synth "E4";
                let now = Time.millis () in
                state :=
                  Listening
                    {
                      synth;
                      game_start = now;
                      sound;
                      target = Note.random ();
                      target_start = now;
                      sustain = None;
                      scores = Scores.empty;
                    }
            | Listening state -> state.sound <- sound
            | End_screen _ -> ())
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
      ({
         sound = { frequency; level };
         game_start;
         target;
         target_start;
         sustain;
         scores;
         synth;
       } as state)
    when now - game_start < config.session_time ->
      let next_note () =
        dbg "nice!";
        (* TODO: we should probably stop listening while this sound
           plays (it plays during it's [sustain_time] which defaults
           to 0.15s).  Perhaps we can set the amp to 0 on the mic, or
           just change the code so that detected frequencies are
           ignored during this time. *)
        Sound.MonoSynth.play synth "Gb4";
        let score = now - target_start - config.sustain_treshold in
        let scores = Scores.add scores score in
        let avg = Scores.average scores in
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
          if hold >= config.sustain_treshold then next_note ());
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
      let () =
        let closest_note = Note.closest frequency in
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
  | Listening { scores; synth; _ } ->
      Sound.MonoSynth.play synth "C4";
      state := End_screen { scores }
  | End_screen { scores } ->
      let len_scores = Scores.length scores in
      let average =
        if len_scores > 0 then
          Scores.(average scores /. 1000.) |> string_of_float
        else "-"
      in
      (* Print the average score *)
      let () =
        Color.background (C.black ());
        Color.fill (C.white ());
        Text.(
          align ~vert_align:Center Center;
          set_size 32;
          draw
            (sf "Average score: %s" average)
            ~x:(width /. 2.0)
            ~y:((height /. 2.0) -. 25.);
          set_size 24;
          draw
            (sf "Notes played: %d" len_scores)
            ~x:(width /. 2.0)
            ~y:((height /. 2.0) +. 25.));
        ()
      in
      (* Call teardown functions *)
      List.iter (fun f -> f ()) !teardown;
      (* Stop calling draw *)
      Sketch.no_loop ()

let () = Sketch.run ~setup ~draw ()
