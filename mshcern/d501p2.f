*CMZ :          02/05/2017  13.26.29  by  Michael Scheer
*-- Author :
# 1 "d501p2.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "d501p2.F"
*
* $Id: d501p2.F,v 1.1.1.1 1996/04/01 15:02:20 mclareni Exp $
*
* $Log: d501p2.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:20  mclareni
* Mathlib gen
*
*

# 1 "/usr/include/gen/pilot.h" 1 3 4
























# 40 "/usr/include/gen/pilot.h" 3 4

# 57 "/usr/include/gen/pilot.h" 3 4



























































# 10 "d501p2.F" 2
      SUBROUTINE D501P2(LRP,N,A,B,C,LAMU,PHI,DPHINO,ITER,LFN,MODE,VERS)

************************************************************************
*   LEAMAX, VERSION: 15.03.1993
************************************************************************
*
*   THIS ROUTINE CONTROLS THE PRINTING OF THE PACKAGE LEAMAX.
*
************************************************************************


# 1 "/usr/include/gen/imp64.inc" 1 3 4
*
* $Id: imp64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: imp64.inc,v $
* Revision 1.1.1.1  1996/04/01 15:02:59  mclareni
* Mathlib gen
*
*
* imp64.inc
*







      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

# 21 "d501p2.F" 2

# 1 "/usr/include/gen/def64.inc" 1 3 4
*
* $Id: def64.inc,v 1.1.1.1 1996/04/01 15:02:59 mclareni Exp $
*
* $Log: def64.inc,v $
* Revision 1.1.1.1  1996/04/01 15:02:59  mclareni
* Mathlib gen
*
*
*
* def64.inc
*







      DOUBLE PRECISION
# 22 "d501p2.F" 2
     +   LAMBDA,LAMU
      LOGICAL LFN,LRP
      CHARACTER VERS*6,TIT(2)*18
      DIMENSION A(*),B(*),C(*),LAMU(*)


      DATA TIT(1),TIT(2) /' ','STANDARD DEVIATION'/

      IF(.NOT.LRP) THEN

       WRITE(6,1030)
       LRP=.TRUE.
      ENDIF

      IF(LFN) THEN
       WRITE(6,1010) 'END:',ITER,PHI,DPHINO
       IF(VERS .EQ. 'DFUNFT') THEN
        WRITE(6,1020) TIT(2)(1:8),TIT(2)(10:18)
       ELSE
        WRITE(6,1020) TIT(1)(1:8),TIT(1)(10:18)
       ENDIF
      ELSE
       WRITE(6,1010) '    ',ITER,PHI,DPHINO
       WRITE(6,1020) TIT(1)(1:8),TIT(1)(10:18)
      ENDIF

      IF(LFN .AND. VERS .EQ. 'DFUNFT') THEN
       WRITE(6,1040) (I,A(I),B(I),LAMU(I),C(I), I=1,N)
      ELSE
       WRITE(6,1050) (I,A(I),B(I),LAMU(I), I=1,N)
      ENDIF

      RETURN

 1010 FORMAT(/6X,A4,' ITERATION',I5,3X,'PHI = ',1PD12.5,6X,
     1       'GNO = ',1PD12.5/)
 1020 FORMAT(12X,'PARAMETER',7X,'PARAMETER',9X,'GRADIENT',
     1       10X,'LAGRANGE',8X,A8/
     2       14X,'NUMBER',10X,'VALUE',28X,'MULTIPLIER',7X,A9/)
 1030 FORMAT(//' ITERATION'//11X,'PHI = VALUE OF OBJECTIVE FUNCTION',
     1        10X,'GNO = NORM OF GRADIENT')
 1040 FORMAT (15X,I3,4X,1PD17.5,1PD17.5,1PD17.5,1PD17.5)
 1050 FORMAT (15X,I3,4X,1PD17.5,1PD17.5,1PD17.5)

      END
