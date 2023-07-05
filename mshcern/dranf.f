*CMZ :          10/04/2019  12.04.16  by  Michael Scheer
*-- Author :    Michael Scheer   02/05/2017
cmsh # 1 "ranf.F"
# 1 "<built-in>"
# 1 "<command-line>"
cmsh # 1 "ranf.F"
*
* $Id: ranf.F,v 1.1.1.1 1996/02/15 17:49:05 mclareni Exp $
*
* $Log: ranf.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*

cmsh # 1 "/usr/include/kernnum/pilot.h" 1 3 4
cmsh # 21 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 33 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 45 "/usr/include/kernnum/pilot.h" 3 4

cmsh # 10 "ranf.F" 2
cmsh          REAL FUNCTION RANF()
          DOUBLE PRECISION FUNCTION DRANF()
CMSH          DOUBLE PRECISION    DRANF,    G900GT,   G900ST
          DOUBLE PRECISION    G900GT,   G900ST
          DOUBLE PRECISION    DS(2),    DM(2),    DSEED
          DOUBLE PRECISION    DX24,     DX48
          DOUBLE PRECISION    DL,       DC,       DU,       DR
          LOGICAL             SINGLE
          DATA      DS     /  1665 1885.D0, 286 8876.D0  /
          DATA      DM     /  1518 4245.D0, 265 1554.D0  /
          DATA      DX24   /  1677 7216.D0  /
          DATA      DX48   /  281 4749 7671 0656.D0  /
CMSH          SINGLE  =  .TRUE.
CMSH          GOTO 10
CMSH          ENTRY DRANF()
          SINGLE  =  .FALSE.
  10      DL  =  DS(1) * DM(1)
          DC  =  DINT(DL/DX24)
          DL  =  DL - DC*DX24
          DU  =  DS(1)*DM(2) + DS(2)*DM(1) + DC
          DS(2)  =  DU - DINT(DU/DX24)*DX24
          DS(1)  =  DL
          DR     =  (DS(2)*DX24 + DS(1)) / DX48
CMSH          IF(SINGLE)  THEN
CMSH             RANF  =  SNGL(DR)
CMSH          ELSE
             DRANF  =  DR
CMSH          ENDIF
          RETURN
cmsh          ENTRY G900GT()
cmsh          G900GT  =  DS(2)*DX24 + DS(1)
cmsh          RETURN
cmsh          ENTRY G900ST(DSEED)
cmsh          DS(2)  =  DINT(DSEED/DX24)
cmsh          DS(1)  =  DSEED - DS(2)*DX24
cmsh          G900ST =  DS(1)
cmsh          RETURN
          END
