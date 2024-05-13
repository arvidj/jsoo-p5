type t = { note : string; frequency : float }

let to_string_pretty { note; _ } =
  let buf = Buffer.create (String.length note) in
  Fun.flip String.iter note (fun c ->
      Buffer.add_string buf
        (match c with
        | '0' -> "₀"
        | '1' -> "₁"
        | '2' -> "₂"
        | '3' -> "₃"
        | '4' -> "₄"
        | '5' -> "₅"
        | '6' -> "₆"
        | '7' -> "₇"
        | '8' -> "₈"
        | '9' -> "₉"
        | '#' -> "♯"
        | 'b' -> "♭"
        | c -> String.make 1 c));
  Buffer.contents buf

let notes =
  [|
    { note = "E2"; frequency = 82.41 };
    { note = "F2"; frequency = 87.31 };
    { note = "F#2/Gb2"; frequency = 92.50 };
    { note = "G2"; frequency = 98.00 };
    { note = "G#2/Ab2"; frequency = 103.83 };
    { note = "A2"; frequency = 110.00 };
    { note = "A#2/Bb2"; frequency = 116.54 };
    { note = "B2"; frequency = 123.47 };
    { note = "C3"; frequency = 130.81 };
    { note = "C#3/Db3"; frequency = 138.59 };
    { note = "D3"; frequency = 146.83 };
    { note = "D#3/Eb3"; frequency = 155.56 };
    { note = "E3"; frequency = 164.81 };
  |]

let diff f1 f2 =
  Float.abs (f1.frequency -. f2.frequency)

let closest frequency =
  let closest = ref notes.(0) in
  let diff f1 f2 = Float.abs (f1 -. f2) in
  for i = 1 to Array.length notes - 1 do
    if diff frequency notes.(i).frequency < frequency -. !closest.frequency then
      closest := notes.(i)
  done;
  !closest

let rec random ?unlike () =
  let i = Random.int (Array.length notes) in
  let note = notes.(i) in
  match unlike with
  | Some unlike when note = unlike -> random ~unlike ()
  | _ -> note
