*CMZ :          28/08/2014  14.01.08  by  Michael Scheer
*-- Author :    Michael Scheer   28/08/2014

cmsh Generated with: cpp -E -DCERNLIB_DOUBLE -DCERNLIB_UNIX dfinv.F

# 1 "dfinv.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "dfinv.F"
*
* $Id: dfinv.F,v 1.1.1.1 1996/02/15 17:49:03 mclareni Exp $
*
* $Log: dfinv.F,v $
* Revision 1.1.1.1 1996/02/15 17:49:03 mclareni
* Kernlib
*
*
# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 10 "dfinv.F" 2
          SUBROUTINE DFINV(N,A,IDIM,IR)
          INTEGER IR(*)
          DOUBLE PRECISION A(IDIM,*),ZERO, X, Y, TI
          DOUBLE PRECISION S31, S32, S33, S34, DOTF
          CHARACTER*6 HNAME
          DATA HNAME / ' DFINV' /
          DOTF(X,Y,S31) = X*Y + S31
          DATA ZERO / 0.D0 /
# 1 "finv.inc" 1
*
* $Id: finv.inc,v 1.1.1.1 1996/02/15 17:49:03 mclareni Exp $
*
* $Log: finv.inc,v $
* Revision 1.1.1.1 1996/02/15 17:49:03 mclareni
* Kernlib
*
*
*
* finv.inc
*
          IF(IDIM .GE. N .AND. N .GT. 0) GOTO 310
             CALL TMPRNT(HNAME,N,IDIM,0)
             RETURN
 310     IF(N .EQ. 1) RETURN
          A(2,1) = -A(2,2) * DOTF(A(1,1),A(2,1),ZERO)
          A(1,2) = -A(1,2)
          IF(N .EQ. 2) GOTO 330
          DO 314 I = 3, N
             IM2 = I-2
             DO 312 J = 1, IM2
                S31 = ZERO
                S32 = A(J,I)
                DO 311 K = J, IM2
                   S31 = DOTF(A(K,J),A(I,K),S31)
                   S32 = DOTF(A(J,K+1),A(K+1,I),S32)
 311      CONTINUE
                A(I,J) = -A(I,I) * DOTF(A(I-1,J),A(I,I-1),S31)
                A(J,I) = -S32
 312      CONTINUE
             A(I,I-1) = -A(I,I) * DOTF(A(I-1,I-1),A(I,I-1),ZERO)
             A(I-1,I) = -A(I-1,I)
 314      CONTINUE
 330      NM1 = N-1
          DO 335 I = 1, NM1
             NMI = N-I
             DO 332 J = 1, I
                S33 = A(I,J)
                DO 331 K = 1, NMI
                   S33 = DOTF(A(I+K,J),A(I,I+K),S33)
 331      CONTINUE
                A(I,J) = S33
 332      CONTINUE
             DO 334 J = 1, NMI
                S34 = ZERO
                DO 333 K = J, NMI
                   S34 = DOTF(A(I+K,I+J),A(I,I+K),S34)
 333      CONTINUE
                A(I,I+J) = S34
 334      CONTINUE
 335      CONTINUE
          NXCH = IR(N)
          IF(NXCH .EQ. 0) RETURN
            DO 342 M = 1, NXCH
             K = NXCH - M+1
             IJ = IR(K)
             I = IJ / 4096
             J = MOD(IJ,4096)
             DO 341 K = 1, N
                TI = A(K,I)
                A(K,I) = A(K,J)
                A(K,J) = TI
 341         CONTINUE
 342       CONTINUE
# 19 "dfinv.F" 2
          RETURN
          END
