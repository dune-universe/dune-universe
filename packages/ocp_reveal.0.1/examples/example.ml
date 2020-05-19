open Ocp_reveal.Html
open Ocp_reveal.Slides

let slide_0 =
  frame { default with
    title = title5 "Make your presentation with OCaml";
    content = {|
You can write your presentation in OCaml thant to `ocp-reveal`:
```ocaml
let my_slide =
  frame { defaut with
    title = title3 "Some title";
    content = "Some dummy content";
  }
```
|}
  }

let slide_00 =
  let content = "
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
```
" in

  frame { default with
    title = title5 "Settings";
    content;
  }

let slide_1 =
  let open Omd in
  let content = "You can use Omd syntax to create your slide :" in
  let code = "let slide =
   Single ([ H3 [Text title]; Paragraph [Text content]],
            slide_transition_effect)" in
  Single ([ H3 [Text "Omd Syntax"];
            Paragraph [Text content];
            Code_block ("OCaml", code)],
          convex)

let slide_video =
  frame { default with
    title = title3 "Some video inside the slide !";
    video = Some "http://clips.vorwaerts-gmbh.de/big_buck_bunny.mp4";
    background_color = Some White;

  }


let slide_2_0 =
  frame { default with
    title = title3 "Vertical slides !";
    content = " Hey slide down !";
    background_color = Some Black;
  }

let slide_2_1 =
  frame { default with
    title = title3 "Slide 2.1";
    content = "
Hey ! **How** are *you*
  1. first
  2. second
  3. third";
    background_color = Some Black;
  }

let slide_2 = Multiple [slide_2_0; slide_2_1]

let slide_3 =
  frame { slide with
    background_video = Some "https://s3.amazonaws.com/static.slid.es/site/homepage/v1/homepage-video-editor.mp4";
  }

let slide_4 =
  let content =
    "
    frame { slide with
      title = title3 \"Your slide title\";
      content = \"Some content\";
      background_color = Some Black;
    }" in
  frame { slide with
    title = title3 "Black background";
    content;
    background_color = Some Black;
  }

let slide_5 =
  let content =
    "
    frame { slide with
      title = title3 \"Your slide title\";
      content = \"Some content\";
      background_color = Some White;
    }" in
  frame { slide with
    title = title3 "White background";
    content = content;
    background_color = Some White;
  }


let slide_6 =
  let content =
    "
    frame { slide with
      title = title3 \"Your slide title\";
      content = \"Some content\";
      background_img = path_to_img;
    }" in
  frame { convex with
    title = title3 "Add a background image to your slide";
    content;
    background_color = Some White;
    background_img = Some "tux.png";
  }

let slide_7 =
  frame { convex with
    title = title3 "Math equations";
    content = {| $\delta \rightarrow \Lambda$ |};
    background_color = Some Black;
  }

let slides =
  [ slide_0; slide_00; slide_1; slide_video; slide_2; slide_3; slide_4; slide_5; slide_6; slide_7]

let () =
  make_config "Demo ocp-reveal" slides
