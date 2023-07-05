*CMZ :          02/05/2017  14.52.17  by  Michael Scheer
*-- Author :
cmsh # 19 "dvsca.F" 2

# 1 "dvscl.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvscl.F"
*
* $Id: dvscl.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvscl.F" 2
          SUBROUTINE          DVSCL(N,S,X,X2,Z,Z2)
          DOUBLE PRECISION    S, X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  S*A
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
# 15 "dvscl.F" 2

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
# 16 "dvscl.F" 2

# 1 "vcpy.inc" 1
*
* $Id: vcpy.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: vcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* vcpy.inc
*
          LXJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  FUNCT( X(LXJ) )
             LXJ     =  LXJ + JX
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
