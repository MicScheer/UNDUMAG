*CMZ :  1.16/04 17/04/2014  11.20.34  by  Michael Scheer
*-- Author :    Michael Scheer   17/04/2014
*# 1 "dfeqn.F"
*# 1 "<command-line>"
*# 1 "dfeqn.F"
*
* $Id: dfeqn.F,v 1.1.1.1 1996/02/15 17:49:03 mclareni Exp $
*
* $Log: dfeqn.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:03  mclareni
* Kernlib
*
*

*# 1 "kernnum/pilot.h" 1
*# 21 "kernnum/pilot.h"

*# 33 "kernnum/pilot.h"

*# 10 "dfeqn.F" 2
          SUBROUTINE          DFEQN(N,A,IDIM,IR,K,B)
          INTEGER             IR(*)
          DOUBLE PRECISION    A(IDIM,*),B(IDIM,*),X,Y,TE
          DOUBLE PRECISION    S21, S22, DOTF
          CHARACTER*6         HNAME
          DOTF(X,Y,S21)  =  X*Y + S21
          DATA      HNAME               /  ' DFEQN'  /

*# 1 "feqn.inc" 1
*
* $Id: feqn.inc,v 1.1.1.1 1996/02/15 17:49:03 mclareni Exp $
*
* $Log: feqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:03  mclareni
* Kernlib
*
*
*
* feqn.inc
*
          IF(IDIM .GE. N  .AND.  N .GT. 0  .AND.  K .GT. 0)  GOTO 210
          CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
 210      NXCH  =  IR(N)
          IF(NXCH .EQ. 0)  GOTO 220
          DO 212    M  =  1, NXCH
             IJ  =  IR(M)
             I   =  IJ / 4096
             J   =  MOD(IJ,4096)
             DO 211   L  =  1, K
                TE      =  B(I,L)
                B(I,L)  =  B(J,L)
                B(J,L)  =  TE
 211            CONTINUE
 212         CONTINUE
 220      DO 221    L  =  1, K
             B(1,L)  =  A(1,1)*B(1,L)
 221         CONTINUE
          IF(N .EQ. 1)  GOTO 299
          DO 243    L  =  1, K
             DO 232   I  =  2, N
                IM1  =  I-1
                S21  =  - B(I,L)
                DO 231   J  =  1, IM1
                   S21  =  DOTF(A(I,J),B(J,L),S21)
 231               CONTINUE
                B(I,L)  =  - A(I,I)*S21
 232            CONTINUE
             NM1  =  N-1
             DO 242   I  =  1, NM1
                NMI  =  N-I
                S22  =  - B(NMI,L)
                DO 241   J  =  1, I
                   NMJP1  =  N - J+1
                   S22    =  DOTF(A(NMI,NMJP1),B(NMJP1,L),S22)
 241               CONTINUE
                B(NMI,L)  =  - S22
 242            CONTINUE
 243         CONTINUE
 299      CONTINUE
*# 18 "dfeqn.F" 2
          RETURN
          END
