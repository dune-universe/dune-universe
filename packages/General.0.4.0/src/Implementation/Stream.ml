module Self = Foundations.Stream

include Self

module Tests = struct
  open Testing

  let test = "Stream" >:: [
    (let module T = Traits.FilterMapable.Tests.Make1(Self) in T.test);
    (let module T = Traits.FilterMapable.ToList.Tests.Make1(Self)(Self.ToList)(List) in T.test);
  ]
end
