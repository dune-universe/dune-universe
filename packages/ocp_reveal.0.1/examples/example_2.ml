open Ocp_reveal.Html
open Ocp_reveal.Slides

(* New frame *)
let _ = frame { default with
  title = title5 "Make your presentation with OCaml";
  content = {|
You can write your presentation in OCaml thant to `ocp-reveal`:
```ocaml
let my_slide =
  frame { defaut with
    title = title3 "Some title";
    content = "Some dummy content";
  }
```|}
}

(* New frame *)
let _ = frame { default with
  title = title5 "Settings";
  content = {|
All the fields are optional, so you can edit and set
the properties that you want to change :

```ocaml
type slide = {
   title : Omd.element;
   content : string;
   transition : transition;
   video: path option;
   text_color : color;
   background_color : color;
   background_img : path option;
   background_video : path option;
   background_embed : path option;
}
```|}
}

(* New frame *)
let _ = frame { default with
  title = title3 "Some video inside the slide !";
  video = Some "http://clips.vorwaerts-gmbh.de/big_buck_bunny.mp4";
  background_color = Some White;
}

(* New frame *)
let _ = frame { default with
  title = title3 "Slide 2.1";
  content = {|
Hey ! **How** are *you*
  1. first
  2. second
  3. third |};
  background_color = Some Black;
}

(* New frame *)
let _ = frame { slide with
  background_video = Some "https://s3.amazonaws.com/static.slid.es/site/homepage/v1/homepage-video-editor.mp4";
}


let () =
  auto_make_config "Demo ocp-reveal - auto"
