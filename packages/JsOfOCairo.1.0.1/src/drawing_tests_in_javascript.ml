(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

module Tests = DrawingTests.Make(JsOfOCairo)

let drawing_tests =
  Tests.tests
  |> List.map ~f:(fun {Tests.name; width; height; draw; known_failure} ->
    object%js (_)
      val name = Js.string name
      val width = width
      val height = height
      method draw canvas = draw (JsOfOCairo.create canvas)
      val known_failure_ = Js.bool known_failure
    end
  )
  |> Array.of_list

let () = Js.export "drawing_tests" drawing_tests
