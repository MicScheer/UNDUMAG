*CMZ :  2.02/01 28/05/2021  09.18.09  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  09.09.48  by  Michael Scheer
*CMZ :  1.10/01 18/11/2016  15.03.37  by  Michael Scheer
*CMZ :  1.02/01 09/09/2016  14.12.42  by  Michael Scheer
*CMZ :  0.00/01 19/11/2015  13.56.50  by  Michael Scheer
*CMZ :  2.70/12 01/03/2013  16.28.24  by  Michael Scheer
*CMZ :  2.67/05 16/05/2012  12.45.37  by  Michael Scheer
*CMZ : 00.00/07 12/10/2009  12.17.45  by  Michael Scheer
*CMZ : 00.00/02 14/04/2003  12.46.09  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.48  by  Michael Scheer
*-- Author : Michael Scheer
      SUBROUTINE UTIL_SPLINE_COEF(X,Y,N,YP1,YPN,Y2,AA,BB,CC,C)
*KEEP,GPLHINT.
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

C--- CALCULATES SPLINE COEFFICIENTS

C--   INPUT:

C-       N: NUMBER OF X,Y-VALUES
C-       X: ARRAY OF X-VALUES
C-       Y: ARRAY OF Y-VALUES
C-       YP1:  SECOND DERIVATIVE AT FIRST X-VALUE
C-       YPN:  SECOND DERIVATIVE AT LAST X-VALUE

C--   OUPUT:

C-       Y2:   SPLINE-COEFFICIENTS

C--   WORKINGSPACE: AA(N),BB(N),CC(N),C(N)


      IMPLICIT NONE

      INTEGER N,J
      double precision  X(N),Y(N),Y2(N),AA(N),BB(N),CC(N),C(N)

      double precision YP1,YPN

      double precision xx(3),yy(3),a(3),yp(3),xopt,yopt
      INTEGER ifail

      IF (N.LT.3) then
        if (abs(yp1).eq.9999.0d0) then
          y2(1)=0.0d0
        else
          y2(1)=yp1
        endif
        if (abs(ypn).eq.9999.0d0) then
          y2(n)=0.0d0
        else
          y2(n)=ypn
        endif
        RETURN
      endif

      if (abs(yp1).eq.9999.0d0) then
        xx=x(1:3)
        yy=y(1:3)
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL)
        if (ifail.eq.0) then
          y2(1)=2.0d0*a(3)
        else
          y2(1)=0.0d0
        endif
      else
        Y2(1)=YP1
      endif

      if (abs(ypn).eq.9999.0d0) then
        xx=x(n-2:n)
        yy=y(n-2:n)
        call UTIL_PARABEL(xx,yy,A,YP,XOPT,yopt,IFAIL)
        if (ifail.eq.0) then
          y2(n)=2.0d0*a(3)
        else
          y2(N)=0.0d0
        endif
      else
        Y2(N)=YPN
      endif

      C(1)=Y2(1)
      C(N)=y2(n)

      BB(1)=1.D0
      CC(1)=0.D0
      CC(N)=1.D0

      DO J=2,N-1
        if(x(j+1).eq.x(j)) then
          write(6,*)
          write(6,*)
     &      '*** Error in util_spline_coef: Intervall of zero length'
          write(6,*)'j, x(j), x(j+1):',j,x(j),x(j+1)
          write(6,*)
          ifail=1/(ifail-ifail)
          stop
        endif
          AA(J)=(X(J  )-X(J-1))/6.D0
          BB(J)=(X(J+1)-X(J-1))/3.D0
          CC(J)=(X(J+1)-X(J  ))/6.D0
          C(J)=(Y(J+1)-Y(J  ))/(X(J+1)-X(J  ))
     &          -(Y(J  )-Y(J-1))/(X(J  )-X(J-1))
      ENDDO !J

      DO J=2,N-1

          BB(J)=BB(J)-AA(J)*CC(J-1)
           C(J)= C(J)-AA(J)* C(J-1)
C          AA(J)=AA(J)-AA(J)*BB(J-1)

          CC(J)=CC(J)/BB(J)
           C(J)= C(J)/BB(J)
          BB(J)=1.D0

      ENDDO !J

      DO J=N-1,2,-1
        Y2(J)=C(J)-CC(J)*Y2(J+1)
        if (Abs(y2(j)).lt.1.0d-15) y2(j)=0.0d0
      ENDDO

      RETURN
      END
