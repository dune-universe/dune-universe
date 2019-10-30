% chase version 1.2
% bound = 250, limit = 2000
% ********
% l_init(strd(z), idx$2) & p_init$k(strd(z)) = mesg(akey(k))
%   & non(mesg(akey(invk(k)))). % (0)
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
% l_init(Z, idx$1) => p_init$n(Z) = mesg(text(X)). % (23)
% l_init(Z, idx$1) => p_init$k(Z) = mesg(akey(X)). % (24)
% l_resp(Z, idx$1) => p_resp$n(Z) = mesg(text(X)). % (25)
% l_resp(Z, idx$1) => p_resp$k(Z) = mesg(akey(X)). % (26)
% l_init(strd(Z), idx$1) & p_init$n(strd(Z)) = mesg(text(N))
%   => uniq_at(mesg(text(N)), strd(Z), idx$1). % (27)
% l_init(strd(Z), idx$2) & p_init$n(strd(Z)) = mesg(text(N))
%   & p_init$k(strd(Z)) = mesg(akey(K)) & non(mesg(akey(invk(K))))
%   & uniq_at(mesg(text(N)), strd(Z), idx$1)
%   => l_resp(strd(Z_0), idx$2) & p_resp$n(strd(Z_0)) = mesg(text(N))
%        & p_resp$k(strd(Z_0)) = mesg(akey(K))
%        & prec(strd(Z), idx$1, strd(Z_0), idx$1)
%        & prec(strd(Z_0), idx$2, strd(Z), idx$2). % (28)
% ********

(9,8){16}![l_init(c, idx$2), l_init(c, idx$1), non(c_0),
  prec(c, idx$1, c, idx$2), prec(c, idx$1, c_1, idx$2),
  prec(c, idx$1, c_1, idx$1), prec(c_1, idx$2, c, idx$2),
  prec(c_1, idx$1, c, idx$2), prec(c_1, idx$1, c_1, idx$2), lt(idx$1, idx$2),
  l_resp(c_1, idx$2), l_resp(c_1, idx$1), uniq_at(c_2, c, idx$1), z = z,
  strd(z) = c, strd(z_0) = c_1, idx$2 = idx$2, k = k, akey(k) = c_3,
  akey(c_4) = c_5, mesg(c_5) = c_0, mesg(c_3) = c_6, mesg(c_7) = c_2,
  p_init$k(c) = c_6, invk(k) = c_4, text(x) = c_7, idx$1 = idx$1,
  p_init$n(c) = c_2, p_resp$n(c_1) = c_2, p_resp$k(c_1) = c_6]
