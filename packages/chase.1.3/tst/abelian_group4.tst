% chase version 1.3
% bound = 250, limit = 2000
% ********
% X = X & Y = Y & Z = Z => dot(dot(X, Y), Z) = dot(X, dot(Y, Z)). % (0)
% X = X => dot(X, one) = X. % (1)
% X = X => dot(one, X) = X. % (2)
% X = X => dot(X, inv(X)) = one. % (3)
% X = X & Y = Y => dot(X, Y) = dot(Y, X). % (4)
% X = X => X = one | X = two | X = three | X = four. % (5)
% one = two => false. % (6)
% two = three => false. % (7)
% three = four => false. % (8)
% four = one => false. % (9)
% ********

(320,279){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(322,279){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(328,281){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(330,281){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(820,744){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(822,744){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(828,746){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(830,746){5}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]
