module Canvas = struct
  type t

  let create ?sketch ?renderer w h =
    let renderer = Jv.of_option ~none:Jv.null Jv.Id.to_jv renderer in
    Sketch.call sketch "createCanvas" [| Jv.of_int w; Jv.of_int h; renderer |]

  let width ?sketch () =
    Sketch.get sketch "width" |> Jv.to_float

  let height ?sketch () =
    Sketch.get sketch "height" |> Jv.to_float
end
