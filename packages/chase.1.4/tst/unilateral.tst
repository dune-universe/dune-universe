% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% l_init(z, idx$2) & p_init$k(z) = k & non(invk(k)). % (0)
% mesg(X) & strd(X) => false. % (1)
% akey(X) & skey(X) => false. % (2)
% akey(X) & text(X) => false. % (3)
% akey(X) & name(X) => false. % (4)
% skey(X) & text(X) => false. % (5)
% skey(X) & name(X) => false. % (6)
% text(X) & name(X) => false. % (7)
% akey(X) => mesg(X). % (8)
% skey(X) => mesg(X). % (9)
% text(X) => mesg(X). % (10)
% name(X) => mesg(X). % (11)
% invk(X) = Y => akey(X) & akey(Y). % (12)
% akey(X) => invk(X) = Y & akey(Y). % (13)
% invk(X) = X => false. % (14)
% akey(X) => invk(invk(X)) = X. % (15)
% invk(X) = invk(Y) => X = Y. % (16)
% pubk(X) = Y => name(X) & akey(Y). % (17)
% name(X) => pubk(X) = Y & akey(Y). % (18)
% pubk(X) = pubk(Y) => X = Y. % (19)
% ltk(X, Y) = Z => name(X) & name(Y) & skey(Z). % (20)
% ltk(W, X) = ltk(Y, Z) => W = Y & X = Z. % (21)
% cat(X, Y) = Z => mesg(X) & mesg(Y) & mesg(Z). % (22)
% cat(W, X) = cat(Y, Z) => W = Y & X = Z. % (23)
% aenc(X, Y) = Z => mesg(X) & mesg(Y) & akey(Z). % (24)
% aenc(W, X) = aenc(Y, Z) => W = Y & X = Z. % (25)
% senc(X, Y) = Z => mesg(X) & mesg(Y) & skey(Z). % (26)
% senc(W, X) = senc(Y, Z) => W = Y & X = Z. % (27)
% prec(X, I, X, I) => false. % (28)
% prec(X, I, Y, J) & prec(Y, J, Z, K) => prec(X, I, Z, K). % (29)
% l_lsn(Z, H) => strd(Z). % (30)
% l_lsn(Z, idx$2) => l_lsn(Z, idx$1) & prec(Z, idx$1, Z, idx$2). % (31)
% l_lsn_x(Z) = X => strd(Z) & mesg(X). % (32)
% l_lsn(Z, idx$1) => p_lsn$x(Z) = X & mesg(X). % (33)
% lt(idx$1, idx$2). % (34)
% idx$1 = idx$2 => false. % (35)
% l_init(Z, H) => strd(Z). % (36)
% l_init(Z, H) & lt(I, H) => l_init(Z, I) & prec(Z, I, Z, H). % (37)
% l_resp(Z, H) => strd(Z). % (38)
% l_resp(Z, H) & lt(I, H) => l_resp(Z, I) & prec(Z, I, Z, H). % (39)
% p_init$n(Z) = X => strd(Z) & text(X). % (40)
% l_init(Z, idx$1) => p_init$n(Z) = X & text(X). % (41)
% p_init$k(Z) = X => strd(Z) & akey(X). % (42)
% l_init(Z, idx$1) => p_init$k(Z) = X & akey(X). % (43)
% p_resp$n(Z) = X => strd(Z) & text(X). % (44)
% l_resp(Z, idx$1) => p_resp$n(Z) = X & text(X). % (45)
% p_resp$k(Z) = X => strd(Z) & akey(X). % (46)
% l_resp(Z, idx$1) => p_resp$k(Z) = X & akey(X). % (47)
% l_init(Z$, idx$1) & p_init$n(Z$) = N => uniq_at(N, Z$, idx$1). % (48)
% l_init(Z, idx$2) & p_init$n(Z) = N & p_init$k(Z) = K & non(invk(K))
%   & uniq_at(N, Z, idx$1) & text(N) & akey(K) & strd(Z)
%   => l_resp(Z_0, idx$2) & p_resp$n(Z_0) = N & p_resp$k(Z_0) = K
%        & prec(Z, idx$1, Z_0, idx$1) & prec(Z_0, idx$2, Z, idx$2)
%        & strd(Z_0). % (49)
% ********

(25,24){29}![l_init(z, idx$2), l_init(z, idx$1), non(c), mesg(k), mesg(c),
  mesg(x), strd(z), strd(z_0), akey(k), akey(c), text(x),
  prec(z, idx$1, z, idx$2), prec(z, idx$1, z_0, idx$2),
  prec(z, idx$1, z_0, idx$1), prec(z_0, idx$2, z, idx$2),
  prec(z_0, idx$1, z, idx$2), prec(z_0, idx$1, z_0, idx$2), lt(idx$1, idx$2),
  l_resp(z_0, idx$2), l_resp(z_0, idx$1), uniq_at(x, z, idx$1), z = z,
  idx$2 = idx$2, k = k, p_init$k(z) = k, invk(k) = c, invk(c) = k,
  idx$1 = idx$1, p_init$n(z) = x, p_resp$n(z_0) = x, p_resp$k(z_0) = k]
