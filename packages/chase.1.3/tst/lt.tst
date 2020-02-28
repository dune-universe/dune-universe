% chase version 1.3
% bound = 250, limit = 2000
% ********
% lt(o, s(s(s(s(o))))). % (0)
% lt(X, s(Y)) => lt(Y, s(Y)). % (1)
% lt(s(X), s(Y)) => lt(X, Y). % (2)
% lt(X, Y) & lt(Y, Z) => lt(X, Z). % (3)
% ********

(10,9){3}![lt(o, c), lt(o, c_0), lt(o, c_1), lt(o, c_2), lt(c_0, c),
  lt(c_1, c), lt(c_1, c_0), lt(c_2, c), lt(c_2, c_0), lt(c_2, c_1), o = o,
  s(o) = c_2, s(c_0) = c, s(c_1) = c_0, s(c_2) = c_1]
