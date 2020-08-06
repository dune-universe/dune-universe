open Js_of_ocaml

class type  configuration = object
  method auto : bool Js.prop
  method el : Dom_html.divElement Js.t Js.prop
  method value : int Js.prop
  method format : Js.js_string Js.t Js.prop
  method theme : Js.js_string Js.t Js.prop
  method animation : Js.js_string Js.t
end

class type odometer = object
  method update : int -> unit Js.meth
end

(* [auto]: Don't automatically initialize everything with class
   'odometer' duration: Change how long the javascript expects the CSS
   animation to take
   [theme]: Specify the theme (if you have more than one theme css file
   on the page)
   [animation]: Count is a simpler animation method which just
   increments the value, use it when you're looking for something more
   subtle.
   format:
     (,ddd) - 12,345,678
     (,ddd).dd - 12,345,678.09
     (.ddd),dd - 12.345.678,09
     ( ddd),dd - 12 345 678,09
     d - 12345678 *)
type configuraton = {
  auto : bool option ;
  element : Dom_html.divElement Js.t ;
  value : int ;
  format : string option ;
  theme : string option ;
  animation : string option ;
 }

let from_config config =
  let configuration : configuration Js.t =  Js.Unsafe.obj [||] in
  configuration##.el := config.element  ;
  configuration##.value := config.value ;
  begin match config.format with
    | None -> ()
    | Some format -> configuration##.format := Js.string format
  end  ;
  begin match config.auto with
    | None -> ()
    | Some auto -> configuration##.auto := auto
  end ;
  begin match config.theme with
    | None -> ()
    | Some theme -> configuration##.theme := Js.string theme
  end


(* With specific configuration *)
let odometer_with_config config =
  let configuration = from_config config in
  let odometer_ctsr = Js.Unsafe.global##._Odometer in
  (Js.Unsafe.new_obj odometer_ctsr
     [| Js.Unsafe.inject configuration |] : odometer Js.t)

(* With default config *)
let odometer el =
  let configuration : configuration Js.t =  Js.Unsafe.obj [||] in
  configuration##.el := el ;
  configuration##.value := 0 ;
  configuration##.format := Js.string "d" ;
  let odometer_ctsr = Js.Unsafe.global##._Odometer in
  (Js.Unsafe.new_obj odometer_ctsr
      [| Js.Unsafe.inject configuration |] : odometer Js.t)

let update od value =
   od##(update value)
