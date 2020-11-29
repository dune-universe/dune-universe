% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% X = X & Y = Y & Z = Z => dot(dot(X, Y), Z) = dot(X, dot(Y, Z)). % (0)
% X = X => dot(X, one) = X. % (1)
% X = X => dot(one, X) = X. % (2)
% X = X => dot(X, inv(X)) = one. % (3)
% X = X => X = one | X = two | X = three. % (4)
% one = two => false. % (5)
% two = three => false. % (6)
% three = one => false. % (7)
% ********

(319,286){4}![dot(one, one) = one, dot(one, two) = two,
  dot(one, three) = three, dot(two, one) = two, dot(two, two) = three,
  dot(two, three) = one, dot(three, one) = three, dot(three, two) = one,
  dot(three, three) = two, one = one, inv(one) = one, inv(two) = three,
  inv(three) = two, two = two, three = three]
