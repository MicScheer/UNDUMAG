*CMZ :          02/05/2017  14.55.22  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 17 "dvcpy.F" 2

# 1 "dvdiv.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvdiv.F"
*
* $Id: dvdiv.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: dvdiv.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvdiv.F" 2
          SUBROUTINE          DVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*), T
          REALF(T)  =  SNGL(T)
          IFAIL     =  0
          IF(N .LE. 0)  RETURN

# 1 "dxj.inc" 1
*
* $Id: dxj.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: dxj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* dxj.inc
*

          JX  =  (LOCF(X2) - LOCF(X)) / 2
# 16 "dvdiv.F" 2

# 1 "dyj.inc" 1
*
* $Id: dyj.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: dyj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dyj.inc
*

          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
# 17 "dvdiv.F" 2

# 1 "dzj.inc" 1
*
* $Id: dzj.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dzj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* dzj.inc
*

          JZ  =  (LOCF(Z2) - LOCF(Z)) / 2
# 18 "dvdiv.F" 2

# 1 "vdiv.inc" 1
*
* $Id: vdiv.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vdiv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vdiv.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             IF(REALF(Y(LYJ)) .EQ. 0.)  GOTO 20
             Z(LZJ)  =  X(LXJ) / Y(LYJ)
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          J  =  0
  20      IFAIL  =  J
          RETURN
          END
