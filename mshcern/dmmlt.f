*CMZ :          02/05/2017  15.06.48  by  Michael Scheer
*-- Author :    Michael Scheer   02/05/2017

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 1 "dmmlt.F"
# 1 "<built-in>"
# 1 "<command-line>"
cmsh # 1 "dmmlt.F"
*
* $Id: dmmlt.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: dmmlt.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*

# 1 "/usr/include/kernnum/pilot.h" 1 3 4
# 21 "/usr/include/kernnum/pilot.h" 3 4

# 33 "/usr/include/kernnum/pilot.h" 3 4

# 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 10 "dmmlt.F" 2
          SUBROUTINE        DMMLT(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21,T)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*), T(*), A, B, CNJF
          DOUBLE PRECISION    ZERO, SUM, DOTF, SQRF
          DOUBLE PRECISION    S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  0.D0  /
          DOTF(A,B,SUM)  =  A*B + SUM
          SQRF(A,SUM)    =  A*A + SUM
          CNJF(A)        =  A

# 1 "dlocf.inc" 1
*
* $Id: dlocf.inc,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: dlocf.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
*
* dlocf.inc
*

          IF(MIN0(M,N,K) .LE. 0)  RETURN
          LOCX  =  LOCF(X(1))
          LOCY  =  LOCF(Y(1))
          LOCZ  =  LOCF(Z(1))
          IX  =  (LOCF(X21(1)) - LOCX) / 2
          JX  =  (LOCF(X12(1)) - LOCX) / 2
          JY  =  (LOCF(Y21(1)) - LOCY) / 2
          LY  =  (LOCF(Y12(1)) - LOCY) / 2
          IZ  =  (LOCF(Z21(1)) - LOCZ) / 2
          LZ  =  (LOCF(Z12(1)) - LOCZ) / 2
cmsh # 20 "dmmlt.F" 2

# 1 "mmlt.inc" 1
*
* $Id: mmlt.inc,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: mmlt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
*
* mmlt.inc
*

# 1 "zisxy.inc" 1
*
* $Id: zisxy.inc,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: zisxy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
*
* zisxy.inc
*
          IF(LOCZ .EQ. LOCX)  GOTO 30
          IF(LOCZ .EQ. LOCY)  GOTO 40
          IF(LOCX .EQ. LOCY)  GOTO 20
  10      LY1L  =  1
          LZ1L  =  1
          DO 13     L  =  1, K
             LXI1  =  1
             LZIL  =  LZ1L
             DO 12  I  =  1, M
                S11   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 11  J  =  1, N
                   S11   =  DOTF(X(LXIJ),Y(LYJL),S11)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  11               CONTINUE
                Z(LZIL)  =  S11
                LXI1     =  LXI1 + IX
                LZIL     =  LZIL + IZ
  12            CONTINUE
             LY1L  =  LY1L + LY
             LZ1L  =  LZ1L + LZ
  13         CONTINUE
          RETURN
  20      IF(M .NE. K  .OR.  IX .NE. LY  .OR.  JX .NE. JY)  GOTO 10
          LXI1  =  1
          LZII  =  1
          DO 24     I  =  1, M
             S21   =  ZERO
             LXIJ  =  LXI1
             DO 21  J  =  1, N
                S21   =  SQRF(X(LXIJ),S21)
                LXIJ  =  LXIJ + JX
  21            CONTINUE
             Z(LZII)  =  S21
             IF(I .EQ. M)  GOTO 24
             LXK1  =  LXI1 + IX
             LZIK  =  LZII + LZ
             LZKI  =  LZII + IZ
             DO 23  KDASH  =  I+1, M
                S22   =  ZERO
                LXIJ  =  LXI1
                LXKJ  =  LXK1
                DO 22  J  =  1, N
                   S22   =  DOTF(X(LXIJ),X(LXKJ),S22)
                   LXIJ  =  LXIJ + JX
                   LXKJ  =  LXKJ + JX
  22               CONTINUE
                Z(LZIK)  =  S22
                Z(LZKI)  =  CNJF( Z(LZIK) )
                LXK1  =  LXK1 + IX
                LZIK  =  LZIK + LZ
                LZKI  =  LZKI + IZ
  23            CONTINUE
             LXI1  =  LXI1 + IX
             LZII  =  LZII + IZ + LZ
  24         CONTINUE
          RETURN
  30      IF(LOCX .EQ. LOCY)  GOTO 50
          LXI1  =  1
          DO 34     I  =  1, M
             LY1L  =  1
             LTL   =  1
             DO 32  L  =  1, K
                S31   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 31  J  =  1, N
                   S31   =  DOTF(X(LXIJ),Y(LYJL),S31)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  31               CONTINUE
                T(LTL)  =  S31
                LY1L    =  LY1L + LY
                LTL     =  LTL + 1
  32            CONTINUE
             LXIL  =  LXI1
             LTL   =  1
             DO 33  L  =  1, K
                X(LXIL)  =  T(LTL)
                LXIL     =  LXIL + JX
                LTL      =  LTL + 1
  33            CONTINUE
             LXI1  =  LXI1 + IX
  34         CONTINUE
          RETURN
  40      LY1L  =  1
          DO 44     L  =  1, K
             LXI1  =  1
             LTI   =  1
             DO 42  I  =  1, M
                S41   =  ZERO
                LXIJ  =  LXI1
                LYJL  =  LY1L
                DO 41  J  =  1, N
                   S41   =  DOTF(X(LXIJ),Y(LYJL),S41)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  41               CONTINUE
                T(LTI)  =  S41
                LXI1    =  LXI1 + IX
                LTI     =  LTI + 1
  42            CONTINUE
             LYIL  =  LY1L
             LTI   =  1
             DO 43  I  =  1, M
                Y(LYIL)  =  T(LTI)
                LYIL     =  LYIL + JY
                LTI      =  LTI + 1
  43            CONTINUE
             LY1L  =  LY1L + LY
  44         CONTINUE
          RETURN
# 13 "mmlt.inc" 2

# 1 "xisxxtra.inc" 1
*
* $Id: xisxxtra.inc,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: xisxxtra.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
*
* xisxxtra.inc
*
  50      LXI1  =  1
          LXII  =  1
          DO 56     I  =  1, M
             S51   =  ZERO
             LXIJ  =  LXI1
             DO 51  J  =  1, N
                S51   =  SQRF(X(LXIJ),S51)
                LXIJ  =  LXIJ + JX
  51            CONTINUE
             T(1)  =  S51
             IF(I .EQ. M)  GOTO 54
             LXK1  =  LXI1 + IX
             LTK  =  2
             DO 53  KDASH  =  I+1, M
                S52   =  ZERO
                LXIJ  =  LXI1
                LXKJ  =  LXK1
                DO 52  J  =  1, N
                   S52   =  DOTF(X(LXIJ),X(LXKJ),S52)
                   LXIJ  =  LXIJ + JX
                   LXKJ  =  LXKJ + JX
  52               CONTINUE
                T(LTK)  =  S52
                LXK1    =  LXK1 + IX
                LTK     =  LTK + 1
  53            CONTINUE
  54         LXIK  =  LXII
             LTK   =  1
             DO 55  KDASH  =  I, M
                X(LXIK)  =  T(LTK)
                LXIK     =  LXIK + JX
                LTK      =  LTK + 1
  55            CONTINUE
             LXI1     =  LXI1 + IX
             LXII     =  LXII + IX + JX
  56         CONTINUE
          IF(M .EQ. 1)  RETURN
          LXII  =  1
          DO 58     I  =  1, M-1
             LXIK  =  LXII + JX
             LXKI  =  LXII + IX
             DO 57  KDASH  =  I+1, M
                X(LXKI)  =  CNJF( X(LXIK) )
                LXIK     =  LXIK + JX
                LXKI     =  LXKI + IX
  57            CONTINUE
             LXII  =  LXII + IX + JX
  58         CONTINUE
          RETURN
cmsh # 14 "mmlt.inc" 2
cmsh # 21 "dmmlt.F" 2
          END
