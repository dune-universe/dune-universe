beginfig(1)
  u:=2cm;
  vardef koch(expr A,B,n) =
    save C; pair C;
    C = A rotatedaround(1/3[A,B], 120);
    if n>0:
      koch( A,        1/3[A,B], n-1);
      koch( 1/3[A,B], C,        n-1);
      koch( C,        2/3[A,B], n-1);
      koch( 2/3[A,B], B,        n-1);
    else:
      draw A--1/3[A,B]--C--2/3[A,B]--B;
    fi;
  enddef;
  z0=(u,0);
  z1=z0 rotated 120;
  z2=z1 rotated 120;
  koch( z0, z1, 4 );
  koch( z1, z2, 4 );
  koch( z2, z0, 4 );
endfig;
