*CMZ : 00.00/02 27/11/97  10.24.46  by  Michael Scheer
*-- Author :    Michael Scheer   27/11/97

      SUBROUTINE UTIL_VCROSS(X,Y,Z)

      IMPLICIT NONE

      REAL*8 X(3),Y(3),Z(3)

C     Z=[X,Y] vector product, cross product

      Z(1)=X(2)*Y(3)-X(3)*Y(2)
      Z(2)=X(3)*Y(1)-X(1)*Y(3)
      Z(3)=X(1)*Y(2)-X(2)*Y(1)


      RETURN
      END
