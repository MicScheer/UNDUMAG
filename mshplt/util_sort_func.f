*CMZ : 00.00/15 05/01/2012  13.52.39  by  Michael Scheer
*CMZ : 00.00/00 11/01/95  11.41.04  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SORT_FUNC(N,RA,YA)

C--- HEAPSORT ROUTINE; SEE NUMERICAL RECIPES 8.2 S 231
C--- ARRAY YA IS FUNCTION OF RA AND SORTED ACCORDINGLY

      IMPLICIT NONE

      INTEGER N,L,IR,I,J

      REAL*8 RA(N),RRA
      REAL*8 YA(N),YYA

      if (n.lt.2) return

      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
          YYA=YA(L)
        ELSE
          RRA=RA(IR)
          YYA=YA(IR)
          RA(IR)=RA(1)
          YA(IR)=YA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            YA(1)=YYA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            YA(I)=YA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
        YA(I)=YYA
      GO TO 10
      END
