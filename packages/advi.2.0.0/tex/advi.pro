%!
% PostScript prologue for advi.
%
/advi@Dict 20 dict def 
advi@Dict begin
/floatstring 20 string def 
/printfloat { floatstring cvs print } def
/rcoor { exch 4 1 roll sub exch 3 -1 roll sub exch } def
/printpos { (dvia) print printfloat  (,) print printfloat (\n) print } def
/printcoor { (dvia) print printfloat  (,) print printfloat (\n) print } def
/printrcoor { (dvir) print printfloat  (,) print printfloat (\n) print } def
end
tx@Dict begin
/PutCoor
{ dup exec 2 copy advi@Dict begin printrcoor end moveto
gsave CP T CM STV exch exec moveto setmatrix CP grestore } def
/LPut
{ tx@NodeDict /LPutPos known { LPutPos } { CP /Y ED /X ED /NAngle 0
def } ifelse LPutCoor  } def 
/LPutCoor
{ tx@NodeDict /LPutPos known { LPutPos } 
{ CP /Y ED /X ED /NAngle 0 def } ifelse NAngle tx@Dict begin /NAngle ED end
gsave CM STV CP Y sub neg exch X sub neg exch moveto
LPutPos advi@Dict begin printcoor end 
setmatrix CP grestore } def
end

% END advi.pro
