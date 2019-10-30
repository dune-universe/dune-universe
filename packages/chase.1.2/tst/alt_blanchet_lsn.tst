% chase version 1.2
% bound = 250, limit = 2000
% ********
% l_resp(strd(z), idx$2) & p_resp$a(strd(z)) = mesg(akey(a))
%   & p_resp$d(strd(z)) = mesg(text(d)) & l_(strd(w), idx$2)
%   & p_$x(strd(w)) = mesg(text(d)) & non(mesg(akey(invk(a)))). % (0)
% strd(X) = strd(Y) => X = Y. % (1)
% mesg(X) = mesg(Y) => X = Y. % (2)
% akey(X) = akey(Y) => X = Y. % (3)
% skey(X) = skey(Y) => X = Y. % (4)
% text(X) = text(Y) => X = Y. % (5)
% name(X) = name(Y) => X = Y. % (6)
% invk(invk(X)) = Y => X = Y. % (7)
% invk(X) = X => false. % (8)
% invk(X) = invk(Y) => X = Y. % (9)
% pubk(X) = pubk(Y) => X = Y. % (10)
% ltk(W, X) = ltk(Y, Z) => W = Y & X = Z. % (11)
% cat(W, X) = cat(Y, Z) => W = Y & X = Z. % (12)
% aenc(W, X) = aenc(Y, Z) => W = Y & X = Z. % (13)
% senc(W, X) = senc(Y, Z) => W = Y & X = Z. % (14)
% prec(X, I, X, I) => false. % (15)
% prec(X, I, Y, J) & prec(Y, J, Z, K) => prec(X, I, Z, K). % (16)
% l_(Z, idx$2) => l_(Z, idx$1) & prec(Z, idx$1, Z, idx$2). % (17)
% l_(Z, idx$1) => p_$x(Z) = mesg(X). % (18)
% lt(idx$1, idx$2). % (19)
% idx$1 = idx$2 => false. % (20)
% l_init(Z, H) & lt(I, H) => l_init(Z, I) & prec(Z, I, Z, H). % (21)
% l_resp(Z, H) & lt(I, H) => l_resp(Z, I) & prec(Z, I, Z, H). % (22)
% l_init(Z, idx$2) => p_init$d(Z) = mesg(text(X)). % (23)
% l_init(Z, idx$1) => p_init$s(Z) = mesg(skey(X)). % (24)
% l_init(Z, idx$1) => p_init$a(Z) = mesg(akey(X)). % (25)
% l_init(Z, idx$1) => p_init$b(Z) = mesg(akey(X)). % (26)
% l_resp(Z, idx$2) => p_resp$d(Z) = mesg(text(X)). % (27)
% l_resp(Z, idx$1) => p_resp$s(Z) = mesg(skey(X)). % (28)
% l_resp(Z, idx$1) => p_resp$a(Z) = mesg(akey(X)). % (29)
% l_resp(Z, idx$1) => p_resp$b(Z) = mesg(akey(X)). % (30)
% l_init(strd(Z), idx$1) & p_init$s(strd(Z)) = mesg(skey(S))
%   => uniq_at(mesg(skey(S)), strd(Z), idx$1). % (31)
% l_resp(strd(Z), idx$2) & p_resp$d(strd(Z)) = mesg(text(D))
%   => uniq_at(mesg(text(D)), strd(Z), idx$2). % (32)
% ********

(7,6){30}![l_resp(c, idx$2), l_resp(c, idx$1), l_(c_0, idx$2),
  l_(c_0, idx$1), non(c_1), prec(c_0, idx$1, c_0, idx$2),
  prec(c, idx$1, c, idx$2), lt(idx$1, idx$2), uniq_at(c_2, c, idx$2), z = z,
  strd(z) = c, strd(w) = c_0, idx$2 = idx$2, a = a, akey(a) = c_3,
  akey(c_4) = c_5, akey(x) = c_6, mesg(c_5) = c_1, mesg(c_7) = c_2,
  mesg(c_3) = c_8, mesg(c_9) = c_10, mesg(c_6) = c_11, p_resp$a(c) = c_8,
  d = d, text(d) = c_7, p_resp$d(c) = c_2, w = w, p_$x(c_0) = c_2,
  invk(a) = c_4, skey(x_0) = c_9, idx$1 = idx$1, p_resp$s(c) = c_10,
  p_resp$b(c) = c_11]
