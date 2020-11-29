% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% X = X & Y = Y & Z = Z => dot(dot(X, Y), Z) = dot(X, dot(Y, Z)). % (0)
% X = X => dot(X, one) = X. % (1)
% X = X => dot(one, X) = X. % (2)
% X = X => X = one | X = two. % (3)
% one = two => false. % (4)
% ********

(8,7){3}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = one, one = one, two = two]

(9,7){3}![dot(one, one) = one, dot(one, two) = two, dot(two, one) = two,
  dot(two, two) = two, one = one, two = two]
