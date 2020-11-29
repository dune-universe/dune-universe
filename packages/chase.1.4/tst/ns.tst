% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% l_resp(z, idx$3) & p_resp$a(z) = a & non(invk(a)). % (0)
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
% lt(idx$1, idx$3). % (35)
% lt(idx$2, idx$3). % (36)
% idx$1 = idx$2 => false. % (37)
% idx$2 = idx$3 => false. % (38)
% idx$3 = idx$1 => false. % (39)
% l_init(Z, H) => strd(Z). % (40)
% l_init(Z, H) & lt(I, H) => l_init(Z, I) & prec(Z, I, Z, H). % (41)
% l_resp(Z, H) => strd(Z). % (42)
% l_resp(Z, H) & lt(I, H) => l_resp(Z, I) & prec(Z, I, Z, H). % (43)
% p_init$n(Z) = X => strd(Z) & text(X). % (44)
% l_init(Z, idx$2) => p_init$n(Z) = X & text(X). % (45)
% p_init$a(Z) = X => strd(Z) & akey(X). % (46)
% l_init(Z, idx$1) => p_init$a(Z) = X & akey(X). % (47)
% p_init$m(Z) = X => strd(Z) & text(X). % (48)
% l_init(Z, idx$1) => p_init$m(Z) = X & text(X). % (49)
% p_init$b(Z) = X => strd(Z) & akey(X). % (50)
% l_init(Z, idx$1) => p_init$b(Z) = X & akey(X). % (51)
% p_resp$n(Z) = X => strd(Z) & text(X). % (52)
% l_resp(Z, idx$2) => p_resp$n(Z) = X & text(X). % (53)
% p_resp$a(Z) = X => strd(Z) & akey(X). % (54)
% l_resp(Z, idx$1) => p_resp$a(Z) = X & akey(X). % (55)
% p_resp$m(Z) = X => strd(Z) & text(X). % (56)
% l_resp(Z, idx$1) => p_resp$m(Z) = X & text(X). % (57)
% p_resp$b(Z) = X => strd(Z) & akey(X). % (58)
% l_resp(Z, idx$1) => p_resp$b(Z) = X & akey(X). % (59)
% l_init(Z$, idx$1) & p_init$m(Z$) = M => uniq_at(M, Z$, idx$1). % (60)
% l_resp(Z$, idx$2) & p_resp$n(Z$) = N => uniq_at(N, Z$, idx$2). % (61)
% l_resp(Z, idx$3) & p_resp$m(Z) = M & p_resp$n(Z) = N & p_resp$a(Z) = A
%   & p_resp$b(Z) = B & non(invk(A)) & uniq_at(N, Z, idx$2) & text(N)
%   & text(M) & akey(A) & akey(B) & strd(Z)
%   => l_init(Z_0, idx$3) & p_init$m(Z_0) = M & p_init$n(Z_0) = N
%        & p_init$a(Z_0) = A & p_init$b(Z_0) = B_0
%        & prec(Z, idx$2, Z_0, idx$2) & prec(Z_0, idx$1, Z, idx$1)
%        & prec(Z_0, idx$3, Z, idx$3) & uniq_at(M, Z_0, idx$1) & akey(B_0)
%        & strd(Z_0). % (62)
% ********

(79,78){15}![l_resp(z, idx$3), l_resp(z, idx$2), l_resp(z, idx$1), non(c),
  mesg(a), mesg(c), mesg(x), mesg(x_0), mesg(x_1), mesg(b), mesg(y),
  mesg(y_0), strd(z), strd(z_0), akey(a), akey(c), akey(x_1), akey(b),
  akey(y), akey(y_0), text(x), text(x_0), prec(z, idx$2, z, idx$3),
  prec(z, idx$2, z_0, idx$3), prec(z, idx$2, z_0, idx$2),
  prec(z, idx$1, z, idx$3), prec(z, idx$1, z, idx$2),
  prec(z, idx$1, z_0, idx$3), prec(z, idx$1, z_0, idx$2),
  prec(z_0, idx$3, z, idx$3), prec(z_0, idx$2, z, idx$3),
  prec(z_0, idx$2, z_0, idx$3), prec(z_0, idx$1, z, idx$3),
  prec(z_0, idx$1, z, idx$2), prec(z_0, idx$1, z, idx$1),
  prec(z_0, idx$1, z_0, idx$3), prec(z_0, idx$1, z_0, idx$2),
  lt(idx$2, idx$3), lt(idx$1, idx$3), lt(idx$1, idx$2), l_init(z_0, idx$3),
  l_init(z_0, idx$2), l_init(z_0, idx$1), uniq_at(x, z, idx$2),
  uniq_at(x_0, z_0, idx$1), z = z, idx$3 = idx$3, a = a, p_resp$a(z) = a,
  invk(a) = c, invk(c) = a, invk(x_1) = y, invk(b) = y_0, invk(y) = x_1,
  invk(y_0) = b, idx$2 = idx$2, idx$1 = idx$1, p_init$n(z_0) = x,
  p_init$a(z_0) = a, p_init$m(z_0) = x_0, p_init$b(z_0) = b, p_resp$n(z) = x,
  p_resp$m(z) = x_0, p_resp$b(z) = x_1]
