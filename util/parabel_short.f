*CMZ :  2.02/01 28/05/2021  09.13.10  by  Michael Scheer
*CMZ :  2.68/02 02/07/2012  11.19.55  by  Michael Scheer
*CMZ :  2.66/09 22/03/2010  15.23.05  by  Michael Scheer
*CMZ : 00.00/02 26/03/97  10.23.11  by  Michael Scheer
*CMZ : 00.00/00 10/01/95  15.27.40  by  Michael Scheer
*-- Author : Michael Scheer
      SUBROUTINE parabel_short(x,y,a)
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

C--- CALCULATES A(1),A(2),A(3)

      IMPLICIT NONE

      double precision A(3),X(3),Y(3),DXM,DXP,x0,a1,a2,dxm2,dxp2
      double precision DET,a22,fm,fp,f0

      x0=x(2)
      f0=y(2)

      fm=y(1)-f0
      fp=y(3)-f0

      dxm=x(1)-x0
      dxp=x(3)-x0

c fm=a1*dxm+a2*dxm**2
c fp=a1*dxp+a2*dxp**2

c (dxm dxm2) (a1) = (y(1))
c (dxp dxp2) (a2) = (y(3))

      dxm2=dxm*dxm
      dxp2=dxp*dxp

      det=dxm*dxp2-dxp*dxm2

      a1=(fm*dxp2-fp*dxm2)/det
      a2=(fp*dxm-fm*dxp)/det
      a22=2.0d0*a2

      a(1)=f0 + (a2*x0 -a1)*x0
      a(2)=a1 - a22*x0
      a(3)=a2

      RETURN
      END
