open Mlpost

let () =
  if not Concrete.supported then (
    Format.eprintf "Concrete is needed for this test@.";
    exit 1 )

let unitsquare = Path.scale (Num.cm 2.) Path.unitsquare

let fig1 =
  let t = [ Transform.slanted (Num.bp 3.); Transform.slanted (Num.bp 3.) ] in
  let square = Path.transform t unitsquare in
  Path.fill square

let fig2 =
  let t = [ Transform.slanted (Num.bp 3.); Transform.slanted (Num.bp 3.) ] in
  let t = Concrete.ctransform_of_transform t in
  let t = Concrete.transform_of_ctransform t in
  let square = Path.transform t unitsquare in
  Path.fill square

open Transform
open Num

let fig3 =
  let t =
    [
      explicit
        {
          x0 = bp 1.;
          y0 = bp 0.;
          xx = bp 1.;
          xy = bp 3.;
          yx = bp 0.;
          yy = bp 1.;
        };
    ]
  in
  let square = Path.transform t unitsquare in
  Path.fill square

let () =
  List.iter
    (fun (s, l) -> Metapost.emit s l)
    [ ("fig1", fig1); ("fig2", fig2); ("fig3", fig3) ]
