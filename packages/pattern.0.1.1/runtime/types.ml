type pattern = (Ppxlib.pattern [@printer Ppxlib.Pprintast.pattern])

and expression = (Ppxlib.expression [@printer Ppxlib.Pprintast.expression])

and mismatch = {
    ident : string;
    expected : pattern;
    got : expression option;
  }

and failure = {
    common : pattern;
    mismatches : mismatch list;
  }

and 'a pattern_result = ('a, failure) result
      [@@deriving show]

type ('a, 'b) matcher = ?quoted:Ppxlib.expression -> 'a -> 'b pattern_result
