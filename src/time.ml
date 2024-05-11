let millis ?sketch () =
  Sketch.call sketch "millis" [||] |> Jv.to_int
