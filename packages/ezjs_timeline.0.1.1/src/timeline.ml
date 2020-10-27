open Js_of_ocaml
open Js

class type options =
  object
    method debug : bool t optdef prop

    method height : int optdef prop

    method width : int optdef prop

    method is_embed_ : bool t optdef prop

    method hash_bookmark_ : bool t optdef prop

    method default_bg_color_ : js_string t optdef prop

    method scale_factor_ : int optdef prop

    method initial_zoom_ : number t optdef prop

    method zoom_sequence_ : number t js_array t optdef prop

    method timenav_position_ : js_string t optdef prop

    method optimal_tick_width_ : int optdef prop

    method base_class_ : js_string t optdef prop

    method timenav_height_ : int optdef prop

    method timenav_height_percentage_ : int optdef prop

    method timenav_mobile_height_percentage_ : int optdef prop

    method timenav_height_min_ : int optdef prop

    method marker_height_min_ : int optdef prop

    method marker_width_min_ : int optdef prop

    method marker_padding_ : int optdef prop

    method start_at_slide_ : int optdef prop

    method menubar_height_ : int optdef prop

    method use_bc_ : bool t optdef prop

    method duration : int optdef prop

    method ease : js_string t optdef prop

    method dragging : bool t optdef prop

    method trackResize : bool t optdef prop

    method slide_padding_ : int optdef prop

    method slide_default_fade_ : js_string t optdef prop

    method language : js_string t optdef prop

    method ga_property_id_ : js_string t opt prop

    method track_events_ : js_string t js_array t optdef prop

    method script_path_ : js_string t optdef prop
  end

class type date =
  object
    method year : int readonly_prop

    method month : int optdef readonly_prop

    method day : int optdef readonly_prop

    method hour : int optdef readonly_prop

    method minute : int optdef readonly_prop

    method second : int optdef readonly_prop

    method millisecond : int optdef readonly_prop

    method format : js_string t optdef readonly_prop

    method display_text : js_string t optdef readonly_prop
  end

class type location =
  object
    method icon : js_string t readonly_prop

    method lat : number t readonly_prop

    method lon : number t readonly_prop

    method line : bool t readonly_prop

    method name : js_string t readonly_prop

    method zoom : int optdef readonly_prop
  end

class type media =
  object
    method caption : js_string t readonly_prop

    method credit : js_string t readonly_prop

    method url : js_string t readonly_prop

    method thumbnail : js_string t readonly_prop
  end

class type text =
  object
    method headline : js_string t readonly_prop

    method text : js_string t readonly_prop
  end

class type ['a] data =
  object
    method data : 'a t readonly_prop
  end

class type ['a] event_data =
  object
    method start_date_ : 'a readonly_prop

    method end_date_ : 'a optdef readonly_prop

    method location : location t optdef readonly_prop

    method media : media t optdef readonly_prop

    method text : text t readonly_prop

    method unique_id_ : js_string t optdef readonly_prop
  end

class type timeline =
  object
    method ready : bool t readonly_prop

    method version : js_string t readonly_prop

    method on :
      js_string t -> (date t data t event_data t -> unit) callback -> unit meth

    method goTo : int -> unit meth

    method goToId : js_string t -> unit meth

    method goToNext : unit meth

    method goToPrev : unit meth

    method goToStart : unit meth

    method goToEnd : unit meth

    method remove : int -> unit meth

    method removeId : js_string t -> unit meth

    method add : date t event_data t -> unit meth

    method getData : int -> date t data t event_data t meth

    method getDataById : js_string t -> date t data t event_data t meth

    method getSlide : int -> date t data t event_data t meth

    method getSlideById : js_string t -> date t data t event_data t meth
  end

type timeline_cs =
  (js_string t -> Unsafe.any -> options t optdef -> timeline t) constr

let options ?debug ?height ?width ?is_embed ?hash_bookmark ?default_bg_color
    ?scale_factor ?initial_zoom ?zoom_sequence ?timenav_position
    ?optimal_tick_width ?base_class ?timenav_height ?timenav_height_percentage
    ?timenav_mobile_height_percentage ?timenav_height_min ?marker_height_min
    ?marker_width_min ?marker_padding ?start_at_slide ?menubar_height ?use_bc
    ?duration ?ease ?dragging ?trackResize ?slide_padding ?slide_default_fade
    ?language ?ga_property_id ?track_events ?script_path () =
  let o : options t = Unsafe.obj [||] in
  (match debug with None -> () | Some d -> o##.debug := def (bool d)) ;
  (match height with None -> () | Some h -> o##.height := def h) ;
  (match width with None -> () | Some w -> o##.width := def w) ;
  (match is_embed with None -> () | Some e -> o##.is_embed_ := def (bool e)) ;
  ( match hash_bookmark with
  | None ->
      ()
  | Some h ->
      o##.hash_bookmark_ := def (bool h) ) ;
  ( match default_bg_color with
  | None ->
      ()
  | Some d ->
      o##.default_bg_color_ := def (string d) ) ;
  (match scale_factor with None -> () | Some f -> o##.scale_factor_ := def f) ;
  ( match initial_zoom with
  | None ->
      ()
  | Some z ->
      o##.initial_zoom_ := def (number_of_float z) ) ;
  ( match zoom_sequence with
  | None ->
      ()
  | Some a ->
      o##.zoom_sequence_ :=
        def (array @@ Array.of_list @@ List.map number_of_float a) ) ;
  ( match timenav_position with
  | None ->
      ()
  | Some p ->
      o##.timenav_position_ := def (string p) ) ;
  ( match optimal_tick_width with
  | None ->
      ()
  | Some w ->
      o##.optimal_tick_width_ := def w ) ;
  ( match base_class with
  | None ->
      ()
  | Some c ->
      o##.base_class_ := def (string c) ) ;
  ( match timenav_height with
  | None ->
      ()
  | Some h ->
      o##.timenav_height_ := def h ) ;
  ( match timenav_height_percentage with
  | None ->
      ()
  | Some p ->
      o##.timenav_height_percentage_ := def p ) ;
  ( match timenav_mobile_height_percentage with
  | None ->
      ()
  | Some p ->
      o##.timenav_mobile_height_percentage_ := def p ) ;
  ( match timenav_height_min with
  | None ->
      ()
  | Some h ->
      o##.timenav_height_min_ := def h ) ;
  ( match marker_height_min with
  | None ->
      ()
  | Some h ->
      o##.marker_height_min_ := def h ) ;
  ( match marker_width_min with
  | None ->
      ()
  | Some w ->
      o##.marker_width_min_ := def w ) ;
  ( match marker_padding with
  | None ->
      ()
  | Some p ->
      o##.marker_padding_ := def p ) ;
  ( match start_at_slide with
  | None ->
      ()
  | Some s ->
      o##.start_at_slide_ := def s ) ;
  ( match menubar_height with
  | None ->
      ()
  | Some h ->
      o##.menubar_height_ := def h ) ;
  (match use_bc with None -> () | Some u -> o##.use_bc_ := def (bool u)) ;
  (match duration with None -> () | Some d -> o##.duration := def d) ;
  (match ease with None -> () | Some s -> o##.ease := def (string s)) ;
  (match dragging with None -> () | Some d -> o##.dragging := def (bool d)) ;
  ( match trackResize with
  | None ->
      ()
  | Some r ->
      o##.trackResize := def (bool r) ) ;
  ( match slide_padding with
  | None ->
      ()
  | Some p ->
      o##.slide_padding_ := def p ) ;
  ( match slide_default_fade with
  | None ->
      ()
  | Some s ->
      o##.slide_default_fade_ := def (string s) ) ;
  (match language with None -> () | Some l -> o##.language := def (string l)) ;
  ( match ga_property_id with
  | None ->
      ()
  | Some i ->
      o##.ga_property_id_ := some (string i) ) ;
  ( match track_events with
  | None ->
      ()
  | Some t ->
      o##.track_events_ := def (array @@ Array.of_list @@ List.map string t) ) ;
  ( match script_path with
  | None ->
      ()
  | Some p ->
      o##.script_path_ := def (string p) ) ;
  o

type json_source = SUrl of string | SStr of string

let make ?options id json =
  let json =
    match json with
    | SUrl s ->
        Unsafe.inject (string s)
    | SStr s ->
        Unsafe.inject (_JSON##parse (string s))
  in
  let options = match options with None -> undefined | Some o -> def o in
  let timeline_cs : timeline_cs = Unsafe.variable "TL.Timeline" in
  let timeline = new%js timeline_cs (string id) json options in
  export "timeline" timeline

let optdef f = function None -> undefined | Some x -> def (f x)

let make_date ?month ?day ?hour ?minute ?second ?millisecond ?format
    ?display_text year : date t =
  object%js
    val year = year

    val month = Optdef.option month

    val day = Optdef.option day

    val hour = Optdef.option hour

    val minute = Optdef.option minute

    val second = Optdef.option second

    val millisecond = Optdef.option millisecond

    val format = optdef string format

    val display_text = optdef string display_text
  end

let make_media ?(caption = "") ?(credit = "") ?(thumbnail = "") url : media t =
  object%js
    val caption = string caption

    val credit = string credit

    val url = string url

    val thumbnail = string thumbnail
  end

let make_text ?(headline = "") text : text t =
  object%js
    val headline = string headline

    val text = string text
  end

let make_event ?end_date ?media ?id text start_date : date t event_data t =
  object%js
    val start_date_ = start_date

    val end_date_ = Optdef.option end_date

    val location = undefined

    val media = Optdef.option media

    val text = text

    val unique_id_ = optdef string id
  end

let add_event ?end_date ?media ?id text start_date (timeline : timeline t) =
  let event = make_event ?end_date ?media ?id text start_date in
  timeline##add event
