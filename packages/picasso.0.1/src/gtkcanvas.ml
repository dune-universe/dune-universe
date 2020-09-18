type internal = GDraw.drawable

let drawable = ref (None:GDraw.drawable option)
let font = ref None

(* text font loading *)
let load_font () =
  let font_list = [
      "-misc-fixed-medium-r-*-*-*-130-*-*-*-*-iso8859-1";
      "-schumacher-clean-medium-r-normal--6-140-75-75-c-60-iso646.1991-irv";
      "-bitstream-courier 10 pitch-bold-r-normal--0-0-0-0-m-0-adobe-standard";
      "rk24";
      "10x20";
    ]
  in
  let fontname = ref "" in
  let exception FontFound of Gdk.font in
  try
    List.iter
      (fun fn ->
        match Gdk.Font.load fn with
        | f -> fontname := fn; raise (FontFound f)
        | exception Gpointer.Null -> ()
      )
      font_list;
    failwith "no font found"
  with FontFound f -> font := Some f

let set_drawable d =
  load_font();
  drawable := Some d

let get_drawable () =
  try Option.get !drawable
  with Invalid_argument _ -> failwith "drawable should be set before canvas operations"

let ending () = ()

let width () =
  let d = get_drawable() in
  fst (d#size)

let height () =
  let d = get_drawable() in
  snd (d#size)

let normalize _ _ (x,y) = x,(float (height())-.y)

type color = GDraw.color

let rgb r g b : color = `RGB(256*r, 256*g, 256*b)

(* Draw text left, centre or right justified at point. (x,y) point is
   either top left, top middle or top right of text. *)
let draw_text col position p text =
  let x,y = Geometry.to_int_point p in
  match !font with
  | Some font -> begin
      let drawable = get_drawable() in
      drawable#set_foreground col;
      let string_width = Gdk.Font.string_width font text in
      let string_height = Gdk.Font.string_height font text in
      match position with
      | `Left ->
         drawable#string text ~font ~x ~y:(y+string_height)
      | `Center ->
         drawable#string text ~font ~x:(x - string_width/2) ~y:(y+string_height)
      | `Right ->
         drawable#string text ~font ~x:(x - string_width) ~y:(y+string_height)
    end
  | None -> failwith "no font found"

let draw_line col a b =
  let ax,ay = Geometry.to_int_point a in
  let bx,by = Geometry.to_int_point b in
  let drawable = get_drawable() in
  drawable#set_foreground col;
  drawable#line ~x:ax ~y:ay ~x:bx ~y:by

let circle fill col (x,y) rad =
  let x,y = Geometry.to_int_point ((x-. rad/.2.),(y -. rad/.2.)) in
  let rad = int_of_float rad in
  let drawable = get_drawable () in
  drawable#set_foreground col;
  drawable#arc ~x:x ~y:y ~height:rad ~width:rad ~filled:fill ~angle:360. ()

let fill_circle = circle true

let draw_circle = circle false

(* polygons *)
let poly fill col vertices =
  let vertices = List.rev_map Geometry.to_int_point vertices in
  let drawable = get_drawable () in
  drawable#set_foreground col;
  drawable#polygon vertices ~filled:fill

let fill_poly = poly true

let draw_poly = poly false
