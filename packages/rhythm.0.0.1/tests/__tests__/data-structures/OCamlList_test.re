open TestFramework;
open Rhythm;

describe("OCamlList", ({test}) => {
  test("make", ({expect}) => {
    let list = OCamlList.make();
    expect.int(OCamlList.length(list)).toBe(0);
    expect.bool(OCamlList.isEmpty(list)).toBeTrue();
  });

  test("init", ({expect}) => {
    let list = OCamlList.init(5, i => i);
    expect.int(OCamlList.length(list)).toBe(5);
    expect.bool(OCamlList.isEmpty(list)).toBeFalse();
    expect.int(OCamlList.getFirstExn(list)).toBe(0);
    expect.int(OCamlList.getLastExn(list)).toBe(4);
  });

  test("Syntax", ({expect}) => {
    let list = [0, 1, 2, 3, 4];
    /* This is an error, because an Array syntax has not been opened. */
    /* let one = list[1]; */
    let _ = list;
    /*
     * A standard, safe syntax for accessing into an array.
     */
    open OCamlList.Syntax;
    let list = [0, 1, 2, 3, 4];
    let one = list[1] |> Option.getExn;
    expect.int(one).toBe(1);
    /*
     * The nested syntax accepts an optional array as input so that deeply
     * nested arrays can still be easily accessed.
     */
    open OCamlList.SyntaxNested;
    let list = [0, 1, 2, 3, 4];
    let list = [list, list, list, list, list];
    let list = [list, list, list, list, list];
    let list = Some(list);
    let one = list[4][2][1] |> Option.getExn;
    expect.int(one).toBe(1);
    /*
     * This is the standard array syntax that is unsafe and can throw
     * exceptions when given invalid indices.
     */
    open OCamlList.SyntaxExn;
    let list = [0, 1, 2, 3, 4];
    let list = [list, list, list, list, list];
    let one = list[2][1];
    expect.int(one).toBe(1);
    /* When the index is too big an exception is thrown. */
    expect.fn(() => list[1000]).toThrow();
    /*
     * Because it may be recommended to open modules to enable syntax we
     * provide a module to reset syntax that was opened.
     */
    open OCamlList.SyntaxReset;
    let list = [0, 1, 2, 3, 4];
    /* This is an error, because we reset the Array syntax. */
    /* let one = list[1]; */
    let _ = list;
    ();
  });
});
