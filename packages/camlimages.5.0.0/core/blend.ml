type mode =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *)


(* look at gxblend.c of ghostscript *)
let blend = function
  | Normal -> fun src _dst -> src
  | Multiply ->
      fun src dst ->
        let t = dst * src + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8
  | Screen ->
      fun src dst ->
        let t = (0xff - dst) * (0xff - src) + 0x80 in
        let t = t + t lsr 8 in
        0xff - t lsr 8
  | Overlay ->
      fun src dst ->
        let t =
          if dst < 0x80 then 2 * dst * src
          else 0xf301 - 2 * (0xff - dst) * (0xff - src) in
        let t = t + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8
(*
  | SoftLight ->
      if s < 0x80 then begin
        let t = (0xff - (src lsl 1)) * art_blend_sq_diff_8[dst] in
        let t = t + 0x8000 in
        dst - t lsr 16
      end else begin
        let t = ((src lsl 1) - 0xff) * art_blend_soft_light_8[dst] in
        let t = t + 0x80 in
        let t = t + t lsr 8 in
        dst + t lsr 8
      end
*)
  | ColorDodge ->
      fun src dst ->
        if dst = 0 then 0 else if dst >= src then 0xff
        else (0x1fe * dst + src) / (src lsl 1)
  | ColorBurn ->
      fun src dst ->
        let dst = 0xff - dst in
        if dst = 0 then 0xff else if dst >= src then 0
        else 0xff - (0x1fe * dst + src) / (src lsl 1)
  | Darken ->
      fun src dst -> if dst < src then dst else src
  | Lighten ->
      fun src dst -> if dst > src then dst else src
  | Difference ->
      fun src dst ->
        let t = dst - src in
        if t < 0 then -t else t
  | Exclusion ->
      fun src dst ->
        let t = (0xff - dst) * src + dst * (0xff - src) in
        let t = t + 0x80 in
        let t = t + t lsr 8 in
        t lsr 8

open Color

let f blendmode srcalpha =
  let blender = blend blendmode in

  match srcalpha with
  | 0 -> fun _src dst -> dst
  | 255 ->
      fun src dst ->
        {r = blender src.r dst.r;
         g = blender src.g dst.g;
         b = blender src.b dst.b}
  | _ ->
      let a' = 255 - srcalpha in
      fun src dst ->
        {r = (blender src.r dst.r * srcalpha + dst.r * a') / 255;
         g = (blender src.g dst.g * srcalpha + dst.g * a') / 255;
         b = (blender src.b dst.b * srcalpha + dst.b * a') / 255}

