open P5

(* Porting of https://github.com/CodingTrain/website/blob/main/CodingChallenges/CC_012_LorenzAttractor/P5/sketch.js *)

module S : Sketch = struct
  open Color
  open Transform
  open Shape

  let width = 800

  let widthf = float_of_int width

  let height = 800

  let heightf = float_of_int height

  let x = ref 0.01

  let y = ref 0.

  let z = ref 0.

  let a = 10.

  let b = 28.

  let c = 8.0 /. 3.0

  let points = ref [||]

  let setup () =
    color_mode HSB;
    Core.(create_canvas ~webgl:true width height)

  let draw () =
    background 0 0 0;
    let dt = 0.01 in
    let dx = a *. (!y -. !x) *. dt in
    let dy = ((!x *. (b -. !z)) -. !y) *. dt in
    let dz = ((!x *. !y) -. (c *. !z)) *. dt in
    x := !x +. dx;
    y := !y +. dy;
    z := !z +. dz;
    points := Array.append !points [| Vector.create_vector !x !y ~z:!z () |];
    translate 0. 0. ~z:(-80.);
    let cam_x = Math.Calc.map (Mouse.mouse_x ()) 0. widthf (-400.) 400. in
    let cam_y = Math.Calc.map (Mouse.mouse_y ()) 0. heightf (-400.) 400. in
    Camera.camera cam_x cam_y
      (heightf /. 2. /. Math.Trig.tan (3.14 *. 30. /. 180.))
      0. 0. 0. 0. 1. 0.;
    scale 5.;
    Vertex.stroke 255. 255. 255.;
    no_fill ();
    let hu = ref 0. in
    let open Shape.Vertex in
    let open Vector in
    begin_shape ();
    !points
    |> Array.iter (fun v ->
           stroke !hu 255. 255.;
           vertex (get_x v) (get_y v) ~z:(get_z v);
           hu := !hu +. 1.;
           if !hu > 255. then hu := 0.);
    end_shape ()
end

module T = Make (S)
