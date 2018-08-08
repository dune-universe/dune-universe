module (* F => *) F (* <= F *) (A : sig end) = struct end 

module (* N => *) N (* <= N *) = struct end 

module M = F (* ? F *) (N (* ? N *))
