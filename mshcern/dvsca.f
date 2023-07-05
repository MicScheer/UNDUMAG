*CMZ :          02/05/2017  14.52.17  by  Michael Scheer
*-- Author :
cmsh # 1 "dvsca.F"
# 1 "<built-in>"
# 1 "<command-line>"
cmsh # 1 "dvsca.F"
*
* $Id: dvsca.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvsca.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 10 "dvsca.F" 2
          SUBROUTINE          DVSCA(N,S,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    S, X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    ADD, A, B
          ADD(A,B)  =  S*A + B
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
cmsh # 16 "dvsca.F" 2

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
cmsh # 17 "dvsca.F" 2

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
cmsh # 18 "dvsca.F" 2

# 1 "vadd.inc" 1
*
* $Id: vadd.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: vadd.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* vadd.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  ADD( X(LXJ),Y(LYJ) )
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          RETURN
          END
