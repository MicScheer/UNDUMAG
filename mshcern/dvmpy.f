*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 1 "dvmpy.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvmpy.F"
*
* $Id: dvmpy.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvmpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvmpy.F" 2
          DOUBLE PRECISION FUNCTION DVMPY(N,X,X2,Y,Y2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), A, B
          DOUBLE PRECISION    SUM, MPA
          MPA(A,B,SUM)  =  A*B + SUM
          SUM  =  0.D0
          IF(N .LE. 0)  GOTO 20

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
# 17 "dvmpy.F" 2

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
# 18 "dvmpy.F" 2

# 1 "vmpa.inc" 1
*
* $Id: vmpa.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vmpa.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vmpa.inc
*
          LXJ  =  1
          LYJ  =  1
          DO 10     J  =  1, N
             SUM  =  MPA( X(LXJ),Y(LYJ), SUM)
             LXJ  =  LXJ + JX
             LYJ  =  LYJ + JY
  10         CONTINUE
# 19 "dvmpy.F" 2
  20      DVMPY  =  SUM
          RETURN
          END
