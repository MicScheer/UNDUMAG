*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 1 "dvmula.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvmula.F"
*
* $Id: dvmula.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvmula.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvmula.F" 2
          SUBROUTINE          DVMULA(N,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    MULA, A, B, C
          MULA(A,B,C)  =  A*B + C
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
# 16 "dvmula.F" 2

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
# 17 "dvmula.F" 2

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
# 18 "dvmula.F" 2

# 1 "vmula.inc" 1
*
* $Id: vmula.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vmula.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vmula.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  MULA( X(LXJ),Y(LYJ),Z(LZJ) )
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          RETURN
          END
