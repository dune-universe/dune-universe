type pattern = (Parsetree.pattern [@printer Pprintast.pattern])

and expression = (Parsetree.expression [@printer Pprintast.expression])

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

type ('a, 'b) matcher = ?quoted:Parsetree.expression -> 'a -> 'b pattern_result
