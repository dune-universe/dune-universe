% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% X = X & Y = Y & Z = Z => dot(dot(X, Y), Z) = dot(X, dot(Y, Z)). % (0)
% X = X => dot(X, one) = X. % (1)
% X = X => dot(one, X) = X. % (2)
% X = X => dot(X, inv(X)) = one. % (3)
% X = X => X = one | X = two | X = three | X = four. % (4)
% one = two => false. % (5)
% two = three => false. % (6)
% three = four => false. % (7)
% four = one => false. % (8)
% ********

(303,262){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(305,262){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(311,264){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(313,264){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(755,679){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(757,679){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(763,681){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(765,681){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(827,697){4}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, inv(one) = one, inv(two) = two, two = two,
  three = one, four = two]

(1346,1284){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]

(1350,1285){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]

(1362,1288){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]

(1366,1289){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]

(1414,1301){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]

(1458,1312){4}![dot(one, one) = one, dot(one, two) = two,
  dot(two, one) = two, dot(two, two) = one, one = one, inv(one) = one,
  inv(two) = two, two = two, three = one, four = two]
