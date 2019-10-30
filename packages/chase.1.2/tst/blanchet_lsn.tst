% chase version 1.2
% bound = 250, limit = 2000
% ********
% l_resp(z, idx$2) & p_resp$a(z) = a & p_resp$d(z) = d & l_lsn(w, idx$2)
%   & p_lsn$x(w) = d & non(invk(a)). % (0)
% prec(z, idx$2, w, idx$1). % (1)
% mesg(X) & strd(X) => false. % (2)
% akey(X) & skey(X) => false. % (3)
% akey(X) & text(X) => false. % (4)
% akey(X) & name(X) => false. % (5)
% skey(X) & text(X) => false. % (6)
% skey(X) & name(X) => false. % (7)
% text(X) & name(X) => false. % (8)
% akey(X) => mesg(X). % (9)
% skey(X) => mesg(X). % (10)
% text(X) => mesg(X). % (11)
% name(X) => mesg(X). % (12)
% invk(X) = Y => akey(X) & akey(Y). % (13)
% akey(X) => invk(X) = Y & akey(Y). % (14)
% invk(X) = X => false. % (15)
% akey(X) => invk(invk(X)) = X. % (16)
% invk(X) = invk(Y) => X = Y. % (17)
% pubk(X) = Y => name(X) & akey(Y). % (18)
% name(X) => pubk(X) = Y & akey(Y). % (19)
% pubk(X) = pubk(Y) => X = Y. % (20)
% ltk(X, Y) = Z => name(X) & name(Y) & skey(Z). % (21)
% ltk(W, X) = ltk(Y, Z) => W = Y & X = Z. % (22)
% cat(X, Y) = Z => mesg(X) & mesg(Y) & mesg(Z). % (23)
% cat(W, X) = cat(Y, Z) => W = Y & X = Z. % (24)
% aenc(X, Y) = Z => mesg(X) & mesg(Y) & akey(Z). % (25)
% aenc(W, X) = aenc(Y, Z) => W = Y & X = Z. % (26)
% senc(X, Y) = Z => mesg(X) & mesg(Y) & skey(Z). % (27)
% senc(W, X) = senc(Y, Z) => W = Y & X = Z. % (28)
% prec(X, I, X, I) => false. % (29)
% prec(X, I, Y, J) & prec(Y, J, Z, K) => prec(X, I, Z, K). % (30)
% l_lsn(Z, H) => strd(Z). % (31)
% l_lsn(Z, idx$2) => l_lsn(Z, idx$1) & prec(Z, idx$1, Z, idx$2). % (32)
% l_lsn_x(Z) = X => strd(Z) & mesg(X). % (33)
% l_lsn(Z, idx$1) => p_lsn$x(Z) = X & mesg(X). % (34)
% lt(idx$1, idx$2). % (35)
% idx$1 = idx$2 => false. % (36)
% l_init(Z, H) => strd(Z). % (37)
% l_init(Z, H) & lt(I, H) => l_init(Z, I) & prec(Z, I, Z, H). % (38)
% l_resp(Z, H) => strd(Z). % (39)
% l_resp(Z, H) & lt(I, H) => l_resp(Z, I) & prec(Z, I, Z, H). % (40)
% p_init$d(Z) = X => strd(Z) & text(X). % (41)
% l_init(Z, idx$2) => p_init$d(Z) = X & text(X). % (42)
% p_init$s(Z) = X => strd(Z) & skey(X). % (43)
% l_init(Z, idx$1) => p_init$s(Z) = X & skey(X). % (44)
% p_init$a(Z) = X => strd(Z) & akey(X). % (45)
% l_init(Z, idx$1) => p_init$a(Z) = X & akey(X). % (46)
% p_init$b(Z) = X => strd(Z) & akey(X). % (47)
% l_init(Z, idx$1) => p_init$b(Z) = X & akey(X). % (48)
% p_resp$d(Z) = X => strd(Z) & text(X). % (49)
% l_resp(Z, idx$2) => p_resp$d(Z) = X & text(X). % (50)
% p_resp$s(Z) = X => strd(Z) & skey(X). % (51)
% l_resp(Z, idx$1) => p_resp$s(Z) = X & skey(X). % (52)
% p_resp$a(Z) = X => strd(Z) & akey(X). % (53)
% l_resp(Z, idx$1) => p_resp$a(Z) = X & akey(X). % (54)
% p_resp$b(Z) = X => strd(Z) & akey(X). % (55)
% l_resp(Z, idx$1) => p_resp$b(Z) = X & akey(X). % (56)
% l_init(Z$, idx$1) & p_init$s(Z$) = S => uniq_at(S, Z$, idx$1). % (57)
% l_resp(Z$, idx$2) & p_resp$d(Z$) = D => uniq_at(D, Z$, idx$2). % (58)
% l_resp(Z, idx$2) & l_lsn(Z_0, idx$2) & p_resp$d(Z) = D & p_resp$s(Z) = S
%   & p_resp$a(Z) = A & p_resp$b(Z) = B & p_lsn$x(Z_0) = D
%   & prec(Z, idx$2, Z_0, idx$1) & non(invk(A)) & uniq_at(D, Z, idx$2)
%   & text(D) & skey(S) & akey(A) & akey(B) & strd(Z) & strd(Z_0)
%   => l_init(Z_1, idx$1) & p_init$s(Z_1) = S & p_init$a(Z_1) = A
%        & p_init$b(Z_1) = B_0 & prec(Z_1, idx$1, Z, idx$1)
%        & uniq_at(S, Z_1, idx$1) & akey(B_0) & strd(Z_1). % (59)
% ********

(136,135){16}![l_resp(z, idx$2), l_resp(z, idx$1), l_lsn(w, idx$2),
  l_lsn(w, idx$1), non(c), prec(z, idx$2, w, idx$2),
  prec(z, idx$2, w, idx$1), prec(z, idx$1, z, idx$2),
  prec(z, idx$1, w, idx$2), prec(z, idx$1, w, idx$1),
  prec(w, idx$1, w, idx$2), prec(z_0, idx$1, z, idx$2),
  prec(z_0, idx$1, z, idx$1), prec(z_0, idx$1, w, idx$2),
  prec(z_0, idx$1, w, idx$1), mesg(a), mesg(d), mesg(c), mesg(x), mesg(x_0),
  mesg(b), mesg(y), mesg(y_0), strd(z), strd(w), strd(z_0), akey(a), akey(c),
  akey(x_0), akey(b), akey(y), akey(y_0), skey(x), text(d), lt(idx$1, idx$2),
  l_init(z_0, idx$1), uniq_at(d, z, idx$2), uniq_at(x, z_0, idx$1), z = z,
  idx$2 = idx$2, a = a, p_resp$a(z) = a, d = d, p_resp$d(z) = d, w = w,
  p_lsn$x(w) = d, invk(a) = c, invk(c) = a, invk(x_0) = y, invk(b) = y_0,
  invk(y) = x_0, invk(y_0) = b, idx$1 = idx$1, p_init$s(z_0) = x,
  p_init$a(z_0) = a, p_init$b(z_0) = b, p_resp$s(z) = x, p_resp$b(z) = x_0]
