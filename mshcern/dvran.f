*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 19 "dvmuna.F" 2

# 1 "dvran.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "dvran.F"
*
* $Id: dvran.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvran.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

# 10 "dvran.F" 2
          SUBROUTINE          DVRAN(N,A,B,Z,Z2)
          DOUBLE PRECISION    A, B, C, Z(*), Z2(*)
          DOUBLE PRECISION    DRANF
          IF(N .LE. 0)  RETURN
          LZJ  =  1

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
# 16 "dvran.F" 2
          C    =  B - A
          DO 10     J  =  1, N
             Z(LZJ)  =  C*DRANF() + A
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
