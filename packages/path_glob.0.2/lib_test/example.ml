open Path_glob

(* paths starting with 'foo/' or ending with a '.ml' extension *)
let globber = Glob.parse "<foo/**> or <**/*.ml>"

let () =
    assert (Glob.eval globber "foo/blah" = true);
    assert (Glob.eval globber "bar/blah/baz" = false);
    assert (Glob.eval globber "bar/blah/baz.ml" = true);
