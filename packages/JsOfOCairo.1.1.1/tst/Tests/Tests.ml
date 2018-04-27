(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let drawing_tests =
  let module T = Drawing.Make(CairoMock) in
  Li.map ~f:(fun {T.name; _} -> name) T.tests

module Make(X: sig
  val title: string

  module C: CairoMock.S

  module N: sig
    val name: string
    val create: unit -> C.context
    val backend: [`Cairo | `Node | `Browser | `CairoMock ]
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end): sig
    val run: T.t -> unit
  end

  module Limitation(L: sig
    type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}
  end): sig
    val run: L.t -> unit
  end
end) = struct
  open X

  module Limitations = Limitations.Make(C)
  module Limitation = Limitation(Limitations)

  let () = Li.iter ~f:Limitation.run Limitations.limitations

  let test = title >:: [
    (
      let module T = Decoration.Make(CairoMock.Mock)(struct
        let name = "CairoMock.Mock"
        let create = CairoMock.Mock.create
        let backend = `CairoMock
      end) in
      T.test
    );
    (
      let module T = Decoration.Make(C)(N) in
      T.test
    );
    (
      let module T = Universal.Make(CairoMock.Mock)(struct
        let name = "CairoMock.Mock"
        let create = CairoMock.Mock.create
        let backend = `CairoMock
      end) in
      T.test
    );
    (
      let module T = Universal.Make(CairoMock)(struct
        let name = "CairoMock"
        let create = CairoMock.create
        let backend = `CairoMock
      end) in
      T.test
    );
    (
      let module T = Universal.Make(C)(N) in
      T.test
    );
    (
      let module DecoratedC = CairoMock.Decorate(C) in
      let module T = Universal.Make(DecoratedC)(struct
        let name = Frmt.apply "CairoMock.Decorate(%s)" N.name
        let create () = DecoratedC.create (N.create ())
        let backend = N.backend
      end) in
      T.test
    );
    ~:: "Drawing tests on %s" N.name (
      let module T = Drawing.Make(C) in
      let module DT = DrawingTest(T) in
      T.tests
      |> Li.map ~f:(fun test ->
        test.T.name >: (lazy (DT.run test))
      )
    );
  ]
end
