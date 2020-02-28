% chase version 1.3
% bound = 250, limit = 2000
% ********
% src(z). % (0)
% src(X) => tail(Z) & p(X, Z). % (1)
% tail(Z) => src(X) & p(X, Z). % (2)
% ********

(2,1){1}![src(z), tail(z_0), p(z, z_0), z = z]
