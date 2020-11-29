% Process with "mpost -tex=latex total_ordering"

outputformat := "svg";
outputtemplate := "%j.svg";
prologues := 3;

verbatimtex
\documentclass[12pt]{article}
\begin{document}
etex

input boxes;

vardef cuta(suffix a, b) expr p =
  drawarrow p cutbefore bpath.a cutafter bpath.b
enddef;

vardef cutb(suffix a, b) =
  cuta(a,b) a.c .. b.c
enddef;

beginfig(0);

  w = 3cm;
  h = 16mm;

  boxit.b1(btex \texttt{a = a, b = b} etex);
  boxit.b2(btex \texttt{num(a), num(b), a = a, b = b} etex);
  boxit.b3(btex $\begin{array}{c}
      \texttt{num(a), num(b),}\\
      \texttt{lt(a, b), a = a, b = b}
      \end{array}$ etex);
  boxit.b4(btex \texttt{num(a), a = a, b = a} etex);
  boxit.b5(btex $\begin{array}{c}
      \texttt{num(a), num(b),}\\
      \texttt{lt(b, a), a = a, b = b}
      \end{array}$ etex);
  b1.c = origin;
  b2.c = b1.c - (0, h);
  b3.c = b2.c - (w, h);
  b4.c = b2.c - (0, 1.9h);
  b5.c = b2.c - (-w, h);

  for i = 1 upto 2:
    drawunboxed(b[i]);
  endfor;

  for i = 3 upto 5:
    drawboxed(b[i]);
  endfor;

  cutb(b1, b2);
  cutb(b2, b3);
  cutb(b2, b4);
  cutb(b2, b5);

endfig;

verbatimtex
\end{document}
etex
bye;
