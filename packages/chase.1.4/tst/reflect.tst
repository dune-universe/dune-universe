% chase version 1.4
% bound = 250, limit = 2000, input_order = false
% ********
% l_resp(z, idx$1) & p_resp$a(z) = a & non(invk(a)) & p_resp$b(z) = b
%   & non(invk(b)). % (0)
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
% p_init$b(Z) = X => strd(Z) & akey(X). % (40)
% l_init(Z, idx$1) => p_init$b(Z) = X & akey(X). % (41)
% p_init$a(Z) = X => strd(Z) & akey(X). % (42)
% l_init(Z, idx$1) => p_init$a(Z) = X & akey(X). % (43)
% p_resp$b(Z) = X => strd(Z) & akey(X). % (44)
% l_resp(Z, idx$1) => p_resp$b(Z) = X & akey(X). % (45)
% p_resp$a(Z) = X => strd(Z) & akey(X). % (46)
% l_resp(Z, idx$1) => p_resp$a(Z) = X & akey(X). % (47)
% l_resp(Z, idx$1) & p_resp$a(Z) = A & p_resp$b(Z) = B & non(invk(A))
%   & non(invk(B)) & akey(A) & akey(B) & strd(Z)
%   => l_init(Z_0, idx$1) & p_init$a(Z_0) = A & p_init$b(Z_0) = B
%        & prec(Z_0, idx$1, Z, idx$1) & strd(Z_0)
%        | l_resp(Z_1, idx$2) & l_init(Z_2, idx$1) & p_resp$a(Z_1) = B
%            & p_resp$b(Z_1) = A & p_init$a(Z_2) = B & p_init$b(Z_2) = A
%            & prec(Z_1, idx$2, Z, idx$1) & prec(Z_2, idx$1, Z_1, idx$1)
%            & strd(Z_2) & strd(Z_1). % (48)
% ********

(18,17){48}![l_resp(z, idx$1), non(c), non(c_0), mesg(a), mesg(b), mesg(c),
  mesg(c_0), strd(z), strd(z_0), akey(a), akey(b), akey(c), akey(c_0),
  prec(z_0, idx$1, z, idx$1), lt(idx$1, idx$2), l_init(z_0, idx$1), z = z,
  idx$1 = idx$1, a = a, p_resp$a(z) = a, invk(a) = c_0, invk(b) = c,
  invk(c) = b, invk(c_0) = a, b = b, p_resp$b(z) = b, idx$2 = idx$2,
  p_init$b(z_0) = b, p_init$a(z_0) = a]

(23,22){29}![l_resp(z, idx$1), l_resp(z_1, idx$1), l_resp(z_1, idx$2),
  non(c), non(c_0), mesg(a), mesg(b), mesg(c), mesg(c_0), strd(z), strd(z_2),
  strd(z_1), akey(a), akey(b), akey(c), akey(c_0),
  prec(z_2, idx$1, z, idx$1), prec(z_2, idx$1, z_1, idx$1),
  prec(z_2, idx$1, z_1, idx$2), prec(z_1, idx$1, z, idx$1),
  prec(z_1, idx$1, z_1, idx$2), prec(z_1, idx$2, z, idx$1), lt(idx$1, idx$2),
  l_init(z_2, idx$1), z = z, idx$1 = idx$1, a = a, p_resp$a(z) = a,
  p_resp$a(z_1) = b, invk(a) = c_0, invk(b) = c, invk(c) = b, invk(c_0) = a,
  b = b, p_resp$b(z) = b, p_resp$b(z_1) = a, idx$2 = idx$2,
  p_init$b(z_2) = a, p_init$a(z_2) = b]
