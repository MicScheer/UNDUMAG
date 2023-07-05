*CMZ :          28/08/2014  14.10.36  by  Michael Scheer
*-- Author :    Michael Scheer   28/08/2014

cmsh Generated with: cpp -E -DCERNLIB_DOUBLE -DCERNLIB_UNIX icfnbl.F

# 1 "icfnbl.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "icfnbl.F"
*
* $Id: icfnbl.F,v 1.1.1.1 1996/02/15 17:49:45 mclareni Exp $
*
* $Log: icfnbl.F,v $
* Revision 1.1.1.1 1996/02/15 17:49:45 mclareni
* Kernlib
*
*
# 1 "/usr/include/kerngen/pilot.h" 1 3 4
# 10 "icfnbl.F" 2
      FUNCTION ICFNBL (CHV,JLP,JRP)
C
C CERN PROGLIB# M432 ICFNBL .VERSION KERNFOR 4.21 890323
C ORIG. 04/10/88, JZ
C
C- Find first non-blank character in CHV(JL:JR)

      DIMENSION JLP(9), JRP(9)

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT, DUMMY(36)
      CHARACTER CHV*(*)

      JJ = JLP(1)
      JR = JRP(1)

   12 IF (JJ.GT.JR) GO TO 19
      IF (CHV(JJ:JJ).EQ.' ') THEN
          JJ = JJ + 1
          GO TO 12
        ENDIF
      NGSLAT = JJ
      ICFNBL = JJ
      RETURN

   19 NGSLAT = 0
      ICFNBL = JJ
      RETURN
      END
