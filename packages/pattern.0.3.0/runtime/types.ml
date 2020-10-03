type pattern =
    ((Ppxlib.pattern [@opaque]) [@refl.printer Ppxlib.Pprintast.pattern])

and expression =
    ((Ppxlib.expression [@opaque])
       [@refl.printer Ppxlib.Pprintast.expression])

and mismatch = {
    ident : string;
    expected : pattern;
    got : expression option;
  }

and failure = {
    common : pattern;
    mismatches : mismatch list;
  }
      [@@deriving refl]

type 'a pattern_result = ('a, failure) result
      [@@deriving refl]

type ('a, 'b) matcher = ?quoted:Ppxlib.expression -> 'a -> 'b pattern_result
