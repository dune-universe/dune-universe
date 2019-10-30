% chase version 1.2
% bound = 250, limit = 2000
% ********
% l_resp(z, idx$2) & p_resp$a(z) = a & non(invk(a)). % (0)
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
% p_init$d(Z) = X => strd(Z) & text(X). % (40)
% l_init(Z, idx$2) => p_init$d(Z) = X & text(X). % (41)
% p_init$s(Z) = X => strd(Z) & skey(X). % (42)
% l_init(Z, idx$1) => p_init$s(Z) = X & skey(X). % (43)
% p_init$a(Z) = X => strd(Z) & akey(X). % (44)
% l_init(Z, idx$1) => p_init$a(Z) = X & akey(X). % (45)
% p_init$b(Z) = X => strd(Z) & akey(X). % (46)
% l_init(Z, idx$1) => p_init$b(Z) = X & akey(X). % (47)
% p_resp$d(Z) = X => strd(Z) & text(X). % (48)
% l_resp(Z, idx$2) => p_resp$d(Z) = X & text(X). % (49)
% p_resp$s(Z) = X => strd(Z) & skey(X). % (50)
% l_resp(Z, idx$1) => p_resp$s(Z) = X & skey(X). % (51)
% p_resp$a(Z) = X => strd(Z) & akey(X). % (52)
% l_resp(Z, idx$1) => p_resp$a(Z) = X & akey(X). % (53)
% p_resp$b(Z) = X => strd(Z) & akey(X). % (54)
% l_resp(Z, idx$1) => p_resp$b(Z) = X & akey(X). % (55)
% l_init(Z$, idx$1) & p_init$s(Z$) = S => uniq_at(S, Z$, idx$1). % (56)
% l_resp(Z$, idx$2) & p_resp$d(Z$) = D => uniq_at(D, Z$, idx$2). % (57)
% l_resp(Z, idx$2) & p_resp$d(Z) = D & p_resp$s(Z) = S & p_resp$a(Z) = A
%   & p_resp$b(Z) = B & non(invk(A)) & uniq_at(D, Z, idx$2) & text(D)
%   & skey(S) & akey(A) & akey(B) & strd(Z)
%   => l_init(Z_0, idx$1) & p_init$s(Z_0) = S & p_init$a(Z_0) = A
%        & p_init$b(Z_0) = B_0 & prec(Z_0, idx$1, Z, idx$1)
%        & uniq_at(S, Z_0, idx$1) & akey(B_0) & strd(Z_0). % (58)
% ********

(134,133){15}![l_resp(z, idx$2), l_resp(z, idx$1), non(c), mesg(a), mesg(c),
  mesg(x), mesg(x_0), mesg(x_1), mesg(b), mesg(y), mesg(y_0), strd(z),
  strd(z_0), akey(a), akey(c), akey(x_1), akey(b), akey(y), akey(y_0),
  skey(x_0), text(x), prec(z, idx$1, z, idx$2), prec(z_0, idx$1, z, idx$2),
  prec(z_0, idx$1, z, idx$1), lt(idx$1, idx$2), l_init(z_0, idx$1),
  uniq_at(x, z, idx$2), uniq_at(x_0, z_0, idx$1), z = z, idx$2 = idx$2,
  a = a, p_resp$a(z) = a, invk(a) = c, invk(c) = a, invk(x_1) = y,
  invk(b) = y_0, invk(y) = x_1, invk(y_0) = b, idx$1 = idx$1,
  p_init$s(z_0) = x_0, p_init$a(z_0) = a, p_init$b(z_0) = b, p_resp$d(z) = x,
  p_resp$s(z) = x_0, p_resp$b(z) = x_1]
