*CMZ : 00.00/02 27/06/2005  19.03.55  by  Michael Scheer
*CMZ : 00.00/01 05/06/96  16.08.05  by  Michael Scheer
*-- Author :    Michael Scheer   03/06/96
      SUBROUTINE UTIL_DETERMINANTE_3(A,DET)

C CALCULATES DETERMINANT OF MATRIX A(3,3)

      IMPLICIT NONE

      DOUBLE PRECISION A(3,3),DET

      DET=
     &   A(1,1)*(A(2,2)*A(3,3)-A(3,2)*A(2,3))
     &  -A(2,1)*(A(1,2)*A(3,3)-A(3,2)*A(1,3))
     &  +A(3,1)*(A(1,2)*A(2,3)-A(2,2)*A(1,3))

      RETURN
      END
