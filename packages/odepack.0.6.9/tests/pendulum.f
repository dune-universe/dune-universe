      EXTERNAL FEX
      DOUBLE PRECISION ATOL, RTOL, RWORK, T, TOUT, Y
      DIMENSION Y(4), RWORK(100), IWORK(100)
      NEQ = 4
      Y(1) = 1.
      Y(2) = 0.
      Y(3) = 0.
      Y(4) = 0.
      T = 0.
      TOUT = 10.
      ITOL = 1
      RTOL = 1.D-6
      ATOL = 1.D-6
      ITASK = 1
      ISTATE = 1
      IOPT = 0
      LRW = 100
      LIW = 100
      JT = 2
      DO I = 1,100000
         CALL DLSODA(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     *        IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT)
         TOUT = TOUT + 1.
      END DO
      WRITE(*,*) "T,Y:", T,Y(1),Y(2),Y(3),Y(4)
      END
 
      SUBROUTINE FEX (NEQ, T, Y, YDOT)
      DOUBLE PRECISION T, Y, YDOT, GL, AL, DTH
      DIMENSION Y(4), YDOT(4)
      GL = -4.905
      AL = 1.25
C      WRITE(*,*)T,Y(1),Y(3)
      DTH = Y(3) - Y(1)
      YDOT(1) = Y(2)
      YDOT(2) = GL * sin(Y(1)) + AL / 2 * DTH
      YDOT(3) = Y(4)
      YDOT(4) = GL * sin(Y(3)) - AL * DTH
      RETURN
      END

c$$$Local Variables:
c$$$compile-command: "make -k pendulum"
c$$$End:
