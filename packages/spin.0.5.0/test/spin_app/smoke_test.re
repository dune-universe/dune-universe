open Test_framework;

describe("Smoke test", ({test, _}) => {
  test("Get version", ({expect}) => {
    let version = Test_utils.run([|"--version"|]);
    let expected = expect.string(version |> String.strip);
    expected.toEqual("0.5.0");
  })
});
