*CMZ :  2.02/01 28/05/2021  09.17.01  by  Michael Scheer
*CMZ :  1.15/12 04/05/2017  15.15.21  by  Michael Scheer
*CMZ :  3.02/00 24/09/2014  13.51.08  by  Michael Scheer
*CMZ :  3.01/03 19/03/2014  12.24.14  by  Michael Scheer
*CMZ :  2.70/12 01/03/2013  16.28.24  by  Michael Scheer
*CMZ :  2.66/19 07/06/2011  14.38.25  by  Michael Scheer
*CMZ : 00.00/08 15/12/2010  14.05.16  by  Michael Scheer
*CMZ : 00.00/07 07/05/2008  14.28.10  by  Michael Scheer
*CMZ : 00.00/02 25/08/2006  15.27.06  by  Michael Scheer
*CMZ : 00.00/01 23/02/96  14.56.50  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.54  by  Michael Scheer
*-- Author : Michael Scheer
      SUBROUTINE UTIL_SPLINE_INTER(XA,YA,Y2A,N,X,Y,MODE)
*KEEP,gplhint.
!******************************************************************************
!
!      Copyright 2013 Helmholtz-Zentrum Berlin (HZB)
!      Hahn-Meitner-Platz 1
!      D-14109 Berlin
!      Germany
!
!      Author Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de
!
! -----------------------------------------------------------------------
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy (wave_gpl.txt) of the GNU General Public
!    License along with this program.
!    If not, see <http://www.gnu.org/licenses/>.
!
!    Dieses Programm ist Freie Software: Sie koennen es unter den Bedingungen
!    der GNU General Public License, wie von der Free Software Foundation,
!    Version 3 der Lizenz oder (nach Ihrer Option) jeder spaeteren
!    veroeffentlichten Version, weiterverbreiten und/oder modifizieren.
!
!    Dieses Programm wird in der Hoffnung, dass es nuetzlich sein wird, aber
!    OHNE JEDE GEWAEHRLEISTUNG, bereitgestellt; sogar ohne die implizite
!    Gewaehrleistung der MARKTFAEHIGKEIT oder EIGNUNG FueR EINEN BESTIMMTEN ZWECK.
!    Siehe die GNU General Public License fuer weitere Details.
!
!    Sie sollten eine Kopie der GNU General Public License
!    zusammen mit diesem Programm erhalten haben. Wenn nicht,
!    siehe <http://www.gnu.org/licenses/>.
!
!******************************************************************************
*KEND.

C---  INTERPOLATES Y(X) VIA SPLINE

C--   INPUT:

C-       N: NUMBER OF X,Y-VALUES
C-       XA:   ARRAY OF X-VALUES
C-       YA:   ARRAY OF Y-VALUES
C-       YA2:  ARRAY SPLINE COEFFICIENTS
C-       X: Y(X) IS CALCULATED
C-       MODE: CONTROL FLAG:
C-             MODE.GE.0: USE VALUES OF LAST CALL TO START WITH
C-             MODE.LT.0: NEW INITIALIZATION

C--   OUTPUT:

C-       Y: Y(X) IS CALCULATED

      IMPLICIT NONE

      INTEGER NOLD,N,KLO,KHI,KLOLD,K,MODE,NORDER

      double precision Y,X,XA1OLD,XANOLD,H,A,B

      double precision XA(*),YA(*),Y2A(*),EPS,XX

      save klold,nold,xa1old,xanold

      DATA KLOLD/1/,NOLD/-99/
      DATA XA1OLD/-9999.D0/,XANOLD/-9999./

      EPS=ABS(XA(N)-XA(1))/1.0D10
      XX=X

      IF(XA(1).LT.XA(N)) THEN

        IF(XX.LT.XA(1).AND.XX.GT.XA(1)-EPS) THEN
          XX=XA(1)
        ELSE IF(XX.GT.XA(N).AND.XX.LT.XA(N)+EPS) THEN
          XX=XA(N)
        ENDIF

        IF(XX.LT.XA(1).OR.XX.GT.XA(N)) THEN
          WRITE(6,*)'XA(1), XA(N):',XA(1), XA(N)
          WRITE(6,*)'X:',x
          WRITE(6 ,*)'***ERROR IN UTIL_SPLINE_INTER: X OUT OF RANGE ***'
          STOP
        ENDIF

      ELSE

        IF(XX.LT.XA(N).AND.XX.GT.XA(N)-EPS) THEN
          XX=XA(N)
        ELSE IF(XX.GT.XA(1).AND.XX.LT.XA(N)+EPS) THEN
          XX=XA(1)
        ENDIF

        IF(XX.LT.XA(N).OR.XX.GT.XA(1)) THEN
          WRITE(6,*)'XA(1), XA(N):',XA(1), XA(N)
          WRITE(6,*)'X:',X
          WRITE(6 ,*)'***ERROR IN UTIL_SPLINE_INTER: X OUT OF RANGE ***'
          STOP
        ENDIF

      ENDIF

      norder=1
      if (xa(n).lt.xa(1)) then
        norder=-1
      endif

      if (norder.eq.1) then

        IF (MODE.LT.0.OR.KLOLD.GE.N) THEN
          KLO=1
        ELSE IF(NOLD.EQ.N
     &      .AND. XA(1).EQ.XA1OLD
     &      .AND. XA(N).EQ.XANOLD
     &      .AND. XX.GT.XA(KLOLD)
     &      ) THEN
          KLO=KLOLD
        ELSE
          KLO=1
        ENDIF

        IF (XX.LT.XA(KLO+1)) THEN
          KHI=KLO+1
          GOTO 2
        ENDIF

        KHI=N
1       IF (KHI-KLO.GT.1) THEN
          K=(KHI+KLO)/2
          IF(XA(K).GT.XX)THEN
            KHI=K
          ELSE
            KLO=K
          ENDIF
          GOTO 1
        ENDIF

2       H=XA(KHI)-XA(KLO)

        IF (H.le.0.0D0) THEN
          WRITE(6 ,*)'*** ERROR IN UTIL_SPLINE_INTER: BAD INPUT ***'
          STOP
        ENDIF

        A=(XA(KHI)-XX)/H
        B=(XX-XA(KLO))/H
        Y=A*YA(KLO)+B*YA(KHI)+
     &    (A*(A+1.D0)*(A-1.D0)*Y2A(KLO)+B*(B+1.D0)*
     &    (B-1.D0)*Y2A(KHI))*(H**2)/6.D0

      KLOLD=KLO
      NOLD=N
      XA1OLD=XA(1)
      XANOLD=XA(N)

      else !(norder.eq.1) then

        IF (MODE.LT.0.or.nold.ne.n) THEN
          KLO=1
        ELSE IF(
     &      XA(1).EQ.XA1OLD
     &      .AND. XA(N).EQ.XANOLD
     &      .AND. XX.lt.XA(KLOLD)
     &      ) THEN
          KLO=KLOLD
        ELSE
          KLO=1
        ENDIF

        IF (XX.gt.XA(KLO+1)) THEN
          KHI=KLO+1
          GOTO 21
        ENDIF

        KHI=N
11      IF (KHI-KLO.GT.1) THEN
          K=(KHI+KLO)/2
          IF(XA(K).LT.XX)THEN
            KHI=K
          ELSE
            KLO=K
          ENDIF
          GOTO 11
        ENDIF

21      H=XA(KHI)-XA(KLO)

        IF (H.ge.0.0D0) THEN
          WRITE(6 ,*)'*** ERROR IN UTIL_SPLINE_INTER: BAD INPUT ***'
          STOP
        ENDIF

        A=(XA(KHI)-XX)/H
        B=(XX-XA(KLO))/H
        Y=A*YA(KLO)+B*YA(KHI)+
     &    (A*(A+1.D0)*(A-1.D0)*Y2A(KLO)+B*(B+1.D0)*
     &    (B-1.D0)*Y2A(KHI))*(H**2)/6.D0

        KLOLD=KLO
        NOLD=N
        XA1OLD=XA(1)
        XANOLD=XA(N)

      endif !(norder.eq.1) then

      RETURN
      END
