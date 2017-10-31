include Casetype

let cases = [
  (* test0 *)
  Parse
    {
      nbits = None;
      desc = Some "Wrong file";
      input = None;
      expect = Some "\"xx: No such file or directory\"";
    };
  (* test1 *)
  Parse
    {
      nbits = None;
      desc = Some "Empty file";
      input = Some "";
      expect = Some "(((default()))())";
    };
  (* test2 *)
  Parse
    {
      nbits = None;
      desc = Some "One newline";
      input = Some "\n";
      expect = Some "(((default()))())";
    };
  (* test3 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char, name-value assignment, EOF";
      input = Some "     \\\n x = y";
      expect = Some "(((default((x y))))(((default x)y)))";
    };
  (* test4 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char, name-value assignment, newline";
      input = Some "     \\\n x = y\n";
      expect = Some "(((default((x y))))(((default x)y)))";
    };
  (* test5 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char, failed name-value assignment, newline";
      input = Some "     \\\nn x = y\n";
      expect = Some "\"_|line 2 col 2|func parse_line|equal sign not found\"";
    };
  (* test6 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char, EOF";
      input = Some "   \\";
      expect = Some "(((default()))())";
    };
  (* test7 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char offset 509, long line";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             \\      ";
      expect = Some "(((default()))())";
    };
  (* test8 *)
  Parse
    {
      nbits = None;
      desc = Some "full line, newline at end";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               \n ";
      expect = Some "(((default()))())";
    };
  (* test9 *)
  Parse
    {
      nbits = None;
      desc = Some "Continuation char offset 509, name-value assignment";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             \\ xxx = yyy";
      expect = Some "(((default((xxx yyy))))(((default xxx)yyy)))";
    };
  (* test10 *)
  Parse
    {
      nbits = None;
      desc = Some "Escape char offset 510, name-value assignment";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              \\ xxx = yyy";
      expect = Some "(((default((\"\\\\ xxx\"yyy))))(((default\"\\\\ xxx\")yyy)))";
    };
  (* test11 *)
  Parse
    {
      nbits = None;
      desc = Some "escaped escape char, name-value assignment";
      input = Some "  \\ xxx = yyy";
      expect = Some "(((default((\"\\\\ xxx\"yyy))))(((default\"\\\\ xxx\")yyy)))";
    };
  (* test12 *)
  Parse
    {
      nbits = None;
      desc = Some "Escaped continuation char offset 509,510";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             \\\\";
      expect = Some "(((default()))())";
    };
  (* test13 *)
  Parse
    {
      nbits = None;
      desc = Some "Escaped continuation char offset 510,511";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              \\\\";
      expect = Some "\"_|line 1 col 512|func parse_line|equal sign not found\"";
    };
  (* test14 *)
  Parse
    {
      nbits = None;
      desc = Some "empty lines";
      input = Some "\n\n\n\n\n";
      expect = Some "(((default()))())";
    };
  (* test15 *)
  Parse
    {
      nbits = None;
      desc = Some "empty line, semicolon comment, no end of line";
      input = Some "\n\n; = y";
      expect = Some "(((default((\";\"y))))(((default\";\")y)))";
    };
  (* test16 *)
  Parse
    {
      nbits = None;
      desc = Some "whitespace hashchar comment";
      input = Some "   # comment to end of line\n";
      expect = Some "(((default()))())";
    };
  (* test17 *)
  Parse
    {
      nbits = None;
      desc = Some "section, no close square bracket";
      input = Some "[first1\n";
      expect = Some "\"_|line 1 col 7|func parse_line|closing square bracket not found\"";
    };
  (* test18 *)
  Parse
    {
      nbits = None;
      desc = Some "section, no open square bracket";
      input = Some "first1]\n";
      expect = Some "\"_|line 1 col 6|func parse_line|equal sign not found\"";
    };
  (* test19 *)
  Parse
    {
      nbits = None;
      desc = Some "section, newline";
      input = Some "[first1]\nx = y\n";
      expect = Some "(((default())(first1((x y))))(((first1 x)y)))";
    };
  (* test20 *)
  Parse
    {
      nbits = None;
      desc = Some "section, punctuation, hashchar, newline";
      input = Some "[first1]\nx.y! = my name # testing\n";
      expect = Some "(((default())(first1((x.y!\"my name\"))))(((first1 x.y!)\"my name\")))";
    };
  (* test21 *)
  Parse
    {
      nbits = None;
      desc = Some "qualified name, punctuation, hashchar, newline";
      input = Some "a.b!::x.y! = my name # testing\n";
      expect = Some "(((default())(a.b!((x.y!\"my name\"))))(((a.b! x.y!)\"my name\")))";
    };
  (* test22 *)
  Parse
    {
      nbits = None;
      desc = Some "section with underscore, newline";
      input = Some "[first1_section]\nx = y\n";
      expect = Some "(((first1_section((x y)))(default()))(((first1_section x)y)))";
    };
  (* test23 *)
  Parse
    {
      nbits = None;
      desc = Some "section with escaped chars";
      input = Some "[first1_\\\\\\\"\\'\\$\\r\\n\\b\\t\\#]\nx = y\n";
      expect = Some "(((\"first1_\\\\\\\"'$\\r\\n\\b\\t#\"((x y)))(default()))(((\"first1_\\\\\\\"'$\\r\\n\\b\\t#\"x)y)))";
    };
  (* test24 *)
  Parse
    {
      nbits = None;
      desc = Some "section with underscore, punctuation";
      input = Some "[first1_!.%&*+;?@^~|-]\nx = y\n";
      expect = Some "(((default())(\"first1_!.%&*+;?@^~|-\"((x y))))(((\"first1_!.%&*+;?@^~|-\"x)y)))";
    };
  (* test25 *)
  Parse
    {
      nbits = None;
      desc = Some "section, CRLF";
      input = Some "[first1]\r\nx = y\n";
      expect = Some "(((default())(first1((x y))))(((first1 x)y)))";
    };
  (* test26 *)
  Parse
    {
      nbits = None;
      desc = Some "section, continuation char, CRLF";
      input = Some "[fir\\\r\nst1]\r\nx = y\n";
      expect = Some "(((default())(first1((x y))))(((first1 x)y)))";
    };
  (* test27 *)
  Parse
    {
      nbits = None;
      desc = Some "hashchar (non-punctuation) in section";
      input = Some "[fir#st]\nx = y\n";
      expect = Some "\"_|line 1 col 4|func parse_line|closing square bracket not found\"";
    };
  (* test28 *)
  Parse
    {
      nbits = None;
      desc = Some "hashchar (non-punctuation) in section, with continuation chars";
      input = Some "[f\\\nir#s\\\nt]\nx = y\na = b\n";
      expect = Some "\"_|line 2 col 2|func parse_line|closing square bracket not found\"";
    };
  (* test29 *)
  Parse
    {
      nbits = None;
      desc = Some "escape+hashchar (non-punctuation) in section";
      input = Some "[fir\\#st]\nx = y\n";
      expect = Some "(((default())(fir#st((x y))))(((fir#st x)y)))";
    };
  (* test30 *)
  Parse
    {
      nbits = None;
      desc = Some "empty name";
      input = Some "[fir\\#st]\n= y\n";
      expect = Some "(((default())(fir#st((\"\"y))))(((fir#st\"\")y)))";
    };
  (* test31 *)
  Parse
    {
      nbits = None;
      desc = Some "empty name with spaces";
      input = Some "[fir\\#st]\n     = y\n";
      expect = Some "(((default())(fir#st((\"\"y))))(((fir#st\"\")y)))";
    };
  (* test32 *)
  Parse
    {
      nbits = None;
      desc = Some "non-punct char ends name";
      input = Some "[fir\\#st]\nx=y\n    b#= y\n";
      expect = Some "\"_|line 3 col 5|func parse_line|equal sign not found\"";
    };
  (* test33 *)
  Parse
    {
      nbits = None;
      desc = Some "escaped non-punct char in name";
      input = Some "[first]\nx = y\n    \\#= y1\n";
      expect = Some "(((first((x y)(\"\\\\#\"y1)))(default()))(((first\"\\\\#\")y1)((first x)y)))";
    };
  (* test34 *)
  Parse
    {
      nbits = None;
      desc = Some "single colon in name";
      input = Some "[first]\nx = y\n    first:= y1\n";
      expect = Some "\"_|line 3 col 10|func parse_name|two-colon token not found\"";
    };
  (* test35 *)
  Parse
    {
      nbits = None;
      desc = Some "single colon in name";
      input = Some "[first]\nx = y\n    first:\n";
      expect = Some "\"_|line 3 col 10|func parse_name|two-colon token not found\"";
    };
  (* test36 *)
  Parse
    {
      nbits = None;
      desc = Some "qualified, no name";
      input = Some "[first]\nx = y\nfirst:: = y1\n";
      expect = Some "(((first((x y)(\"\"y1)))(default()))(((first\"\")y1)((first x)y)))";
    };
  (* test37 *)
  Parse
    {
      nbits = None;
      desc = Some "qualified, found name";
      input = Some "[first]\nx = y\n[second]\nx1 = y1\nfirst::x2 = 60\n";
      expect = Some "(((first((x y)(x2 60)))(default())(second((x1 y1))))(((first x2)60)((second x1)y1)((first x)y)))";
    };
  (* test38 *)
  Parse
    {
      nbits = None;
      desc = Some "overriding";
      input = Some "[first1]\nx = y\nx  = z1\n";
      expect = Some "(((default())(first1((x y)(x z1))))(((first1 x)y)((first1 x)z1)))";
    };
  (* test39 *)
  Parse
    {
      nbits = None;
      desc = Some "double-quoted substring";
      input = Some "[first1]\nx = y\nx  = z\"#\"\"$%\"1\n";
      expect = Some "(((default())(first1((x y)(x z#$%1))))(((first1 x)y)((first1 x)z#$%1)))";
    };
  (* test40 *)
  Parse
    {
      nbits = None;
      desc = Some "double-quoted substring, no end double quote";
      input = Some "[first1]\nx = y\nx  = z\"#$%1\n";
      expect = Some "(((default())(first1((x y)(x z#$%1))))(((first1 x)y)((first1 x)z#$%1)))";
    };
  (* test41 *)
  Parse
    {
      nbits = None;
      desc = Some "double-quoted substring, ending at end of line";
      input = Some "[first1]\nx = y\nx  = z\"#\"\"$%\"\n";
      expect = Some "(((default())(first1((x y)(x z#$%))))(((first1 x)y)((first1 x)z#$%)))";
    };
  (* test42 *)
  Parse
    {
      nbits = None;
      desc = Some "single-quoted substring with escaped single quote";
      input = Some "[first1]\nx = y\nx  = z'#\\'\\#$%'1\n";
      expect = Some "(((default())(first1((x y)(x z#'#$%1))))(((first1 x)y)((first1 x)z#'#$%1)))";
    };
  (* test43 *)
  Parse
    {
      nbits = None;
      desc = Some "single-quoted substring, no end single quote";
      input = Some "[first1]\nx = y\nx  = z'#$%1\n";
      expect = Some "(((default())(first1((x y)(x z#$%1))))(((first1 x)y)((first1 x)z#$%1)))";
    };
  (* test44 *)
  Parse
    {
      nbits = None;
      desc = Some "space in value";
      input = Some "[first1]\nx = y\ny  = z first  \n";
      expect = Some "(((default())(first1((x y)(y\"z first\"))))(((first1 x)y)((first1 y)\"z first\")))";
    };
  (* test45 *)
  Parse
    {
      nbits = None;
      desc = Some "space in value, comment at end of value";
      input = Some "[first1]\nx = y\ny  = z firs  # comment from here\n";
      expect = Some "(((default())(first1((x y)(y\"z firs\"))))(((first1 x)y)((first1 y)\"z firs\")))";
    };
  (* test46 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, found";
      input = Some "[first1]\nx = y\nz = $x\n";
      expect = Some "(((default())(first1((x y)(z y))))(((first1 x)y)((first1 z)y)))";
    };
  (* test47 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap {}, found";
      input = Some "[first1]\nx = y\nz = ${x}\n";
      expect = Some "(((default())(first1((x y)(z y))))(((first1 x)y)((first1 z)y)))";
    };
  (* test48 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap {}, no end wrap";
      input = Some "[first1]\nx = y\nz = ${x\n";
      expect = Some "\"_|line 3 col 7|func subst|missing closing wrap char\"";
    };
  (* test49 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap {}, punct end wrap";
      input = Some "[first1]\nx = y\nz = ${x!\n";
      expect = Some "\"_|line 3 col 7|func subst|missing closing wrap char\"";
    };
  (* test50 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap (), found";
      input = Some "[first1]\nx = y\nz = $(x)\n";
      expect = Some "(((default())(first1((x y)(z y))))(((first1 x)y)((first1 z)y)))";
    };
  (* test51 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap (), no end wrap";
      input = Some "[first1]\nx = y\nz = $(x\n";
      expect = Some "\"_|line 3 col 7|func subst|missing closing wrap char\"";
    };
  (* test52 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, wrap (), punct end wrap";
      input = Some "[first1]\nx = y\nz = $(x%\n";
      expect = Some "\"_|line 3 col 7|func subst|missing closing wrap char\"";
    };
  (* test53 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, not found";
      input = Some "[first1]\nx = y\nz = $p\n";
      expect = Some "\"_|line 3 col 6|func subst|variable has no value\"";
    };
  (* test54 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, found from other section";
      input = Some "[first1]\nx = y\n[first2]\nz = 45\nr = ${first1::x}";
      expect = Some "(((first2((z 45)(r y)))(default())(first1((x y))))(((first1 x)y)((first2 z)45)((first2 r)y)))";
    };
  (* test55 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, bad name";
      input = Some "[first1]\nx = y\n[first2]\nz = 45\nr = ${first1:z}";
      expect = Some "\"_|line 5 col 13|func parse_name|two-colon token not found\"";
    };
  (* test56 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, env, empty name";
      input = Some "[first1]\nx = y\nz = $ENV::\n";
      expect = Some "\"_|line 3 col 10|func subst|variable has no value\"";
    };
  (* test57 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, env, found";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "(((default())(first1((x y)(z testvalue))))(((first1 x)y)((first1 z)testvalue)))";
    };
  (* test58 *)
  Parse
    {
      nbits = None;
      desc = Some "subst, env, not found";
      input = Some "[first1]\nx = y\nz = $ENV::NOTDEFINED\n";
      expect = Some "\"_|line 3 col 20|func subst|variable has no value\"";
    };
  (* test59 *)
  Parse
    {
      nbits = Some 10;
      desc = Some "10bit buffer, Empty file";
      input = Some "";
      expect = Some "(((default()))())";
    };
  (* test60 *)
  Parse
    {
      nbits = Some 10;
      desc = Some "10bit buffer, One newline";
      input = Some "\n";
      expect = Some "(((default()))())";
    };
  (* test61 *)
  Parse
    {
      nbits = Some 2;
      desc = Some "2bit buffer, One newline";
      input = Some "\n";
      expect = Some "\"Invalid_argument: too few bits: nbits=2,len=510,limit=0\"";
    };
  (* test62 *)
  Parse
    {
      nbits = Some 10;
      desc = Some "10bit buffer, Continuation char offset 512";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                \\";
      expect = Some "\"len_exceeds_limit: nbits=10,len=1020,limit=764\"";
    };
  (* test63 *)
  Parse
    {
      nbits = Some 10;
      desc = Some "10bit buffer, Continuation char offset 512, name-value assignment";
      input = Some "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                \n xxx = yyy";
      expect = Some "\"len_exceeds_limit: nbits=10,len=1020,limit=764\"";
    };
  (* test64 *)
  Parse
    {
      nbits = Some 10;
      desc = Some "10bit buffer, Section, name-value assignment";
      input = Some "[first]\nfirstName = happy\n";
      expect = Some "(((first((firstName happy)))(default()))(((first firstName)happy)))";
    };
  (* test65 *)
  Stack (
    {
      nbits = None;
      desc = Some "get stack from section";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "((x y)(z testvalue))";
    },
    {
      section = "first1";
    }
  );
  (* test66 *)
  Stack (
    {
      nbits = None;
      desc = Some "get stack from wrong section";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "stack for section \"first3\" not found";
    },
    {
      section = "first3";
    }
  );
  (* test67 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from section";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "testvalue";
    },
    {
      usehashtable = true;
      optsection = Some "first1";
      name = "z";
    }
  );
  (* test68 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from section, wrong name";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "value for name \"t\" not found";
    },
    {
      usehashtable = true;
      optsection = Some "first1";
      name = "t";
    }
  );
  (* test69 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from wrong section";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "value for name \"z\" not found";
    },
    {
      usehashtable = true;
      optsection = Some "first3";
      name = "z";
    }
  );
  (* test70 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from default section";
      input = Some "x = y\nz = $ENV::TESTENV\n";
      expect = Some "testvalue";
    },
    {
      usehashtable = true;
      optsection = None;
      name = "z";
    }
  );
  (* test71 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from default section, when section nonexistent";
      input = Some "x = y\nz = $ENV::TESTENV\n";
      expect = Some "testvalue";
    },
    {
      usehashtable = true;
      optsection = Some "first";
      name = "z";
    }
  );
  (* test72 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from environment, no hashtable";
      input = Some "x = y\nz = $ENV::TESTENV\n";
      expect = Some "testvalue";
    },
    {
      usehashtable = false;
      optsection = None;
      name = "TESTENV";
    }
  );
  (* test73 *)
  Value (
    {
      nbits = None;
      desc = Some "get value from environment, no hashtable";
      input = Some "x = y\nz = $ENV::TESTENV\n";
      expect = Some "value for name \"NOTINENV\" not found";
    },
    {
      usehashtable = false;
      optsection = None;
      name = "NOTINENV";
    }
  );
  (* test74 *)
  ParseLoad
    {
      nbits = Some 10;
      desc = Some "10bit buffer, Section, name-value assignment";
      input = Some "[first]\nfirstName = happy\n";
      expect = Some "(((first((firstName happy)))(default()))(((first firstName)happy)))";
    };
  (* test75 *)
  StackLoad (
    {
      nbits = None;
      desc = Some "get stack from section";
      input = Some "[first1]\nx = y\nz = $ENV::TESTENV\n";
      expect = Some "((x y)(z testvalue))";
    },
    {
      section = "first1";
    }
  );
]
