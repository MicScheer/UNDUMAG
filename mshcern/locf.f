*CMZ :          02/05/2017  14.47.10  by  Michael Scheer
*-- Author :

*KEEP,cmsh,T=F77.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.

cmsh # 16 "dvxch.F" 2

* $Id: locf.F,v 1.1.1.1 1996/02/15 17:50:37 mclareni Exp $
*
* $Log: locf.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:37  mclareni
* Kernlib
*
*
*KEEP,CMSH.
!
!       Routine were taken from the CERNLIB
!       Changes by Michael Scheer are marked by "cmsh"
!
*KEND.
cmsh #include "kerngen/pilot.h"
cmsh #if defined(CERNLIB_QMMPW)
cmsh #include "mpwgs/locf.F"
cmsh #elif defined(CERNLIB_QMSUN)
cmsh #include "sungs/locf.F"
cmsh #elif defined(CERNLIB_QMVAX)
cmsh #include "vaxgs/locf.F"
cmsh #else
      FUNCTION LOCF (IVAR)
C
C CERN PROGLIBcmsh # N100    LOCF            .VERSION KERNFOR  4.34  930114
C
C-    This is a default which works on several machines
C
cmsh      DIMENSION    IVAR(9)
      double precision ivar(9) !cmsh
cmsh #if defined(CERNLIB_QMLXIA64)
      INTEGER*8    J
cmsh #endif
*    Number of ADdress Units Per Word for Fortran
C                         and its logarithm base 2
      PARAMETER    (NADUPW=4, LADUPW=2)

      J = LOC(IVAR)
cmsh #if defined(CERNLIB_QMLXIA64)
      IF (IAND (J, Z'FFFFFFFF00000000') .NE. 0 ) THEN
        WRITE(*,'(57(1H!))')
        WRITE(*,'(A,Z16,A)') 'LOCF: address #', J,
     &                       '# exceeds the 32 bit space'
        WRITE(*,'(57(1H!))')
       END IF
cmsh #endif
      LOCF = ISHFT (J, -LADUPW)
      END
