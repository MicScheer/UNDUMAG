*CMZ :  3.03/04 19/12/2017  12.36.57  by  Michael Scheer
*CMZ : 00.00/15 05/01/2012  13.52.39  by  Michael Scheer
*CMZ : 00.00/00 11/01/95  11.41.04  by  Michael Scheer
*-- Author :
      SUBROUTINE UTIL_SORT_index(N,RA,IYA)

C--- HEAPSORT ROUTINE; SEE NUMERICAL RECIPES 8.2 S 231
C--- ARRAY IYA IS INDEX OF RA AND SORTED ACCORDINGLY

      IMPLICIT NONE

      INTEGER N,L,IR,I,J

      REAL*8 RA(N),RRA
      integer IYA(N),KYA

      if (n.lt.2) return

      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
          kya=iya(L)
        ELSE
          RRA=RA(IR)
          kya=iya(IR)
          RA(IR)=RA(1)
          iya(IR)=iya(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            iya(1)=kya
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
            iya(I)=iya(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
        iya(I)=kya
      GO TO 10
      END
