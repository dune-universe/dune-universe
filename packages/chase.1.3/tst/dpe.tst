% chase version 1.3
% bound = 250, limit = 2000
% ********
% exists(a) & exists(b) & exists(c). % (0)
% re(b, X) & re(c, X) => goal. % (1)
% exists(X) => e(X, X). % (2)
% re(a, b) & re(a, c). % (3)
% e(X, Y) => e(Y, X). % (4)
% e(X, Y) & re(Y, Z) => re(X, Z). % (5)
% e(X, Y) => re(X, Y). % (6)
% r(X, Y) => re(X, Y). % (7)
% re(X, Y) => e(X, Y) | r(X, Y). % (8)
% r(X, Y) & r(X, Z) => exists(U) & r(Y, U) & r(Z, U). % (9)
% ********

(57,50){4}![exists(a), exists(b), exists(c), re(a, a), re(a, b), re(a, c),
  re(b, a), re(b, b), re(b, c), re(c, a), re(c, b), re(c, c), e(a, a),
  e(a, b), e(a, c), e(b, a), e(b, b), e(b, c), e(c, a), e(c, b), e(c, c),
  a = a, b = b, c = c]

(59,51){8}![exists(a), exists(b), exists(c), re(a, a), re(a, b), re(a, c),
  re(b, a), re(b, b), re(b, c), re(c, a), re(c, b), re(c, c), e(a, a),
  e(a, b), e(a, c), e(b, a), e(b, b), e(c, a), e(c, c), r(b, c), r(c, b),
  a = a, b = b, c = c]

(61,52){8}![exists(a), exists(b), exists(c), re(a, a), re(a, b), re(a, c),
  re(b, a), re(b, b), re(b, c), re(c, a), re(c, b), re(c, c), e(a, a),
  e(a, b), e(b, a), e(b, b), e(b, c), e(c, b), e(c, c), r(a, c), r(c, a),
  a = a, b = b, c = c]

(64,54){8}![exists(a), exists(b), exists(c), re(a, a), re(a, b), re(a, c),
  re(b, a), re(b, b), re(b, c), re(c, a), re(c, b), re(c, c), e(a, a),
  e(a, c), e(b, b), e(b, c), e(c, a), e(c, b), e(c, c), r(a, b), r(b, a),
  a = a, b = b, c = c]

(208,192){8}![exists(a), exists(b), exists(c), exists(u), re(a, a), re(a, b),
  re(a, c), re(a, u), re(b, a), re(b, b), re(b, c), re(b, u), re(c, a),
  re(c, b), re(c, c), re(c, u), re(u, a), re(u, b), re(u, c), re(u, u),
  e(a, a), e(a, b), e(a, c), e(b, a), e(b, b), e(b, c), e(b, u), e(c, a),
  e(c, b), e(c, c), e(u, b), e(u, u), r(a, u), r(b, c), r(c, u), r(u, a),
  r(u, c), a = a, b = b, c = c]

(225,206){8}![exists(a), exists(b), exists(c), exists(u), re(a, a), re(a, b),
  re(a, c), re(a, u), re(b, a), re(b, b), re(b, c), re(b, u), re(c, a),
  re(c, b), re(c, c), re(c, u), re(u, a), re(u, b), re(u, c), re(u, u),
  e(a, a), e(a, b), e(a, c), e(a, u), e(b, a), e(b, b), e(b, c), e(b, u),
  e(c, a), e(c, b), e(c, c), e(u, a), e(u, b), e(u, u), r(a, u), r(b, c),
  r(c, u), r(u, c), a = a, b = b, c = c]

(226,207){4}![exists(a), exists(b), exists(c), exists(u), re(a, a), re(a, b),
  re(a, c), re(a, u), re(b, a), re(b, b), re(b, c), re(b, u), re(c, a),
  re(c, b), re(c, c), re(c, u), re(u, a), re(u, b), re(u, c), re(u, u),
  e(a, a), e(a, b), e(a, c), e(b, a), e(b, b), e(b, c), e(b, u), e(c, a),
  e(c, b), e(c, c), e(c, u), e(u, b), e(u, c), e(u, u), r(a, u), r(b, c),
  r(c, u), r(u, a), a = a, b = b, c = c]
