type horiz_align = Left | Center | Right
type vert_align = Top | Bottom | Center | Baseline

let align ?sketch ?vert_align horiz_align =
  let horiz_align =
    Sketch.get None
      (match horiz_align with
      | Left -> "LEFT"
      | Center -> "CENTER"
      | Right -> "RIGHT")
  in
  let vert_align =
    Fun.flip Option.map vert_align @@ fun vert_align ->
    Sketch.get None
      (match vert_align with
      | Top -> "TOP"
      | Bottom -> "BOTTOM"
      | Center -> "CENTER"
      | Baseline -> "BASELINE")
  in
  Sketch.call sketch "textAlign"
    (match vert_align with
    | None -> [| horiz_align |]
    | Some vert_align -> [| horiz_align; vert_align |])
  |> ignore

let set_size ?sketch size =
  Sketch.call sketch "textSize" [| Jv.of_int size |] |> ignore

let get_size ?sketch () = Sketch.call sketch "textSize" [||] |> Jv.to_int

let draw ?sketch ?x ?y ?max_width ?max_height str =
  let prune_none args =
    let rec aux i =
      if i < 0 then [||]
      else if Jv.is_some args.(i) then Array.sub args 0 (i + 1)
      else aux (i - 1)
    in
    aux (Array.length args - 1)
  in
  let fl v = Jv.of_option ~none:Jv.null Jv.of_float v in
  let str = Jv.of_string str in
  let arguments =
    prune_none [| str; fl x; fl y; fl max_width; fl max_height |]
  in
  Sketch.call sketch "text" arguments |> ignore
