*CMZ :  2.04/05 15/03/2023  13.03.23  by  Michael Scheer
*CMZ :  0.00/01 19/11/2015  13.51.04  by  Michael Scheer
*CMZ :  3.02/05 27/03/2015  15.15.15  by  Michael Scheer
*CMZ :  3.01/03 19/03/2014  12.19.21  by  Michael Scheer
*CMZ :  2.68/05 03/09/2012  09.26.37  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2012
      subroutine util_g1_static(y,g1)
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

c calculates G1(y) with an estimated precision of about 1.5e-3 for y<=30,
c and 1.5e-2 for y>30.


      implicit none

      integer ical,npoi,ipoi,npoilow,npoihigh,ndatp
      parameter(ndatp=29)

      double precision y,g1,g1_30,c_30,g1_5em5,c_5em5,y_30,
     &  y_5em5,ydum,g1dum,r1

      double precision
     &  ywlow(ndatp),ywhigh(ndatp),coeflow(ndatp),coefhigh(ndatp),
     &  g1wlow(ndatp),g1whigh(ndatp),g1walow(ndatp),
     &  g1wahigh(ndatp),r1low(ndatp),r1high(ndatp),
     &  w1(ndatp),w2(ndatp),w3(ndatp),w4(ndatp),
     &  ystat(ndatp),g1stat(ndatp)

      save

      data ical/0/
      data y_30/30.0d0/
      data y_5em5/5.0d-5/
      data g1_30/6.580794488121591d-013/ !WAVE
      data g1_5em5/7.909860755922665E-002/ !WAVE

* Numerisch mit WAVE berechnet 11.5.2012 (ISPECDIP=2)
        data ystat/
     &    5.0D-005, 7.0D-005, 2.0D-004, 5.0D-004, 1.0D-003,
     &    2.0D-003, 5.0D-003, 1.0D-002, 2.0D-002, 5.0D-002,
     &    0.10D0, 0.20D0, 0.50D0, 1.0D0, 2.0D0,
     &    3.0D0, 4.0D0, 5.0D0, 6.0D0, 7.0D0,
     &    8.0D0, 9.0D0, 10.00D0, 20.00D0, 30.00D0,
     &    40.0D0, 50.0D0, 60.0D0, 70.0D0
     &    /

        data g1stat/
     &    7.909860755922665D-002, 8.846122555950733D-002,
     &    0.125342415210665d0, 0.169701277907238d0, 0.2131391d0,
     &    0.2671962d0, 0.3584969d0, 0.4449725d0, 0.5472394d0,
     &    0.7015719d0, 0.8181855d0, 0.9033860d0, 0.8708191d0,
     &    0.65142282d0, 0.30163590d0, 0.128565710002655d0,
     &    5.282739666852105D-002,
     &    2.12481297D-002, 8.426079715722744D-003,
     &    3.307610970763407D-003, 1.288451614441198D-003,
     &    4.988932935072772D-004, 1.92238264D-004,
     &    1.19686345D-008, 6.58079455D-013, 3.42988745D-017,
     &    1.73478519D-021, 8.60693915D-026, 4.21333348D-030
     &    /

      if (ical.eq.0) then

        c_5em5=g1_5em5/y_5em5**(1.0d0/3.0d0)
        c_30=g1_30/(sqrt(y_30)*exp(-y_30))

        npoilow=0
        npoihigh=0

        do npoi=1,ndatp
          ydum=ystat(npoi)
          if (ydum.ge.y_5em5.and.ydum.le.4.0d0) then !zwei Abfragen wegen 4.0
            npoilow=npoilow+1
          endif
          if (ydum.ge.4.0d0.and.ydum.le.y_30) then
            npoihigh=npoihigh+1
          endif
        enddo

        npoi=ndatp

        npoilow=0
        npoihigh=0
        do ipoi=1,npoi
          ydum=ystat(ipoi)
          g1dum=g1stat(ipoi)
          if (ydum.ge.y_5em5.and.ydum.le.4.0d0) then
            npoilow=npoilow+1
            ywlow(npoilow)=ydum
            g1wlow(npoilow)=g1dum
            g1walow(npoilow)=
     &        391.8d0 * ydum**(1.0d0/3.0d0) * exp(-ydum*0.8307d0)
     &        -192.0d0 * sqrt(ydum) * exp(-ydum*0.7880d0)
          endif
          if (ydum.ge.4.0d0.and.ydum.le.y_30) then
            npoihigh=npoihigh+1
            ywhigh(npoihigh)=ydum
            g1whigh(npoihigh)=g1dum
            g1wahigh(npoihigh)=164.0d0*sqrt(ydum)*EXP(-ydum)
          endif
        enddo

        r1low(1:npoilow)=g1wlow(1:npoilow)/g1walow(1:npoilow)
        r1high(1:npoihigh)=g1whigh(1:npoihigh)/g1wahigh(1:npoihigh)

        call util_spline_coef(ywlow,r1low,npoilow,0.0d0,0.0d0,coeflow,
     &    w1,w2,w3,w4)
        call util_spline_coef(ywhigh,r1high,npoihigh,0.0d0,0.0d0,
     &    coefhigh,w1,w2,w3,w4)

        ical=1
      endif

      if (y.le.5.0d-5) then
        g1=c_5em5*y**(1.0d0/3.0d0)
      else if (y.ge.30.0d0) then
        g1=c_30*sqrt(y)*exp(-y)
      else

        if (y.ge.y_5em5.and.y.lt.4.0d0) then
          call util_spline_inter(ywlow,r1low,coeflow,npoilow,y,r1,-1)
          g1=r1*(
     &      391.8d0 * y**(1.0d0/3.0d0) * exp(-y*0.8307d0)
     &      -192.0d0 * sqrt(y) * exp(-y*0.7880d0))
        else if (y.ge.4.0d0.and.y.le.y_30) then
          call util_spline_inter(
     &      ywhigh,r1high,coefhigh,npoihigh,y,r1,-1)
          g1=r1*(164.0d0*sqrt(y)* EXP(-y))
        endif

      endif

      return
9999  stop '*** File wave-g1.dat not found ***'
      end
