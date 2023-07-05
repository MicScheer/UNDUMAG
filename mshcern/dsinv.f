*CMZ :          02/05/2017  14.53.25  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 15 "dmutl.F" 2

cmsh # 1 "dsinv.F"
cmsh # 1 "<built-in>"
cmsh # 1 "<command-line>"
cmsh # 1 "dsinv.F"
*
* $Id: dsinv.F,v 1.2 1999/09/08 08:05:11 mclareni Exp $
*
* $Log: dsinv.F,v $
* Revision 1.2  1999/09/08 08:05:11  mclareni
* A problem was reported in DSINV which failed on very small numbers, probably
* due to converting to single before a test. The conversion has been removed her
* and also in DSFACT. This resulted in mods to sfact.inc and sfactd.inc which
* meant that some other routines had to be tidied also.
*
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 16 "dsinv.F" 2
          SUBROUTINE          DSINV(N,A,IDIM,IFAIL)
          DOUBLE PRECISION    A(IDIM,*),  ZERO,  ONE,  X, Y
          CHARACTER*6         HNAME
          DOUBLE PRECISION    S1, S31, S32, S33,  DOTF
          DOTF(X,Y,S1)  =  X * Y + S1
          DATA      HNAME               /  'DSINV '  /
          DATA      ZERO, ONE           /  0.D0, 1.D0 /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900

# 1 "sfact.inc" 1
*
* $Id: sfact.inc,v 1.2 1999/09/08 08:05:21 mclareni Exp $
*
* $Log: sfact.inc,v $
* Revision 1.2  1999/09/08 08:05:21  mclareni
* A problem was reported in DSINV which failed on very small numbers, probably
* due to converting to single before a test. The conversion has been removed her
* and also in DSFACT. This resulted in mods to sfact.inc and sfactd.inc which
* meant that some other routines had to be tidied also.
*
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
*
* sfact.inc
*
          IFAIL  =  0
          DO 144    J  =  1, N
             IF((A(J,J)) .LE. ZERO)  GOTO 150
             A(J,J)  =  ONE / A(J,J)
             IF(J .EQ. N)  GOTO 199
 140         JP1  =  J+1
             DO 143   L  =  JP1, N
                A(J,L)  =  A(J,J)*A(L,J)
                S1      =  -A(L,J+1)
                DO 141  I  =  1, J
                   S1  =  DOTF(A(L,I),A(I,J+1),S1)
 141               CONTINUE
                A(L,J+1)  =  -S1
 143            CONTINUE
 144         CONTINUE
 150      IFAIL  =  -1
          RETURN
 199      CONTINUE
cmsh # 25 "dsinv.F" 2

# 1 "sfinv.inc" 1
*
* $Id: sfinv.inc,v 1.1.1.1 1996/02/15 17:49:04 mclareni Exp $
*
* $Log: sfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
*
* sfinv.inc
*
          IF(N .EQ. 1)  GOTO 399
          A(1,2)  =  -A(1,2)
          A(2,1)  =   A(1,2)*A(2,2)
          IF(N .EQ. 2)  GOTO 320
          DO 314    J  =  3, N
             JM2  =  J - 2
             DO 312 K  =  1, JM2
                S31  =  A(K,J)
                DO 311  I  =  K, JM2
                   S31  =  DOTF(A(K,I+1),A(I+1,J),S31)
 311               CONTINUE
                A(K,J)  =  -S31
                A(J,K)  =  -S31*A(J,J)
 312            CONTINUE
             A(J-1,J)  =  -A(J-1,J)
             A(J,J-1)  =   A(J-1,J)*A(J,J)
 314         CONTINUE
 320      J  =  1
 323         S33  =  A(J,J)
             IF(J .EQ. N)  GOTO 325
             JP1  =  J + 1
             DO 324 I  =  JP1, N
                S33  =  DOTF(A(J,I),A(I,J),S33)
 324            CONTINUE
 325         A(J,J)  =  S33
          JM1  =  J
          J    =  JP1
             DO 328 K  =  1, JM1
                S32  =  ZERO
                DO 327  I  =  J, N
                   S32  =  DOTF(A(K,I),A(I,J),S32)
 327               CONTINUE
                A(K,J)  =  S32
                A(J,K)  =  S32
 328            CONTINUE
          IF(J .LT. N)  GOTO 323
 399      CONTINUE
cmsh # 26 "dsinv.F" 2
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
