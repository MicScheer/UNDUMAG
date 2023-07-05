*CMZ :  1.25/04 05/04/2018  16.11.27  by  Michael Scheer
*CMZ :  1.25/03 29/03/2018  13.22.42  by  Michael Scheer
*-- Author :    Michael Scheer   26/03/2018
      subroutine undumag_bwireinf(wire7,yin,zin,byint,bzint,istat)

      implicit none

      double precision byint,bzint,yin,zin,wire7(7),rmu04pi,curr,curnor,
     &  x1,x2,y1,y2,z1,z2

      integer istat

c     This routine calculates the infinit integrals byint and bzint for
c     for a filament wire

      data rmu04pi/1.0d-7/

      istat=0

      byint=0.0d0
      bzint=0.0d0

      curr=wire7(1)
      curnor=curr*rmu04pi

      if (curr.eq.0.0d0) return

      !Shift the problem in x and yin,zin

      x1=-(wire7(5)-wire7(2))/2.0d0
      x2= (wire7(5)-wire7(2))/2.0d0
      y1=wire7(3)-yin
      y2=wire7(6)-yin
      z1=wire7(4)-zin
      z2=wire7(7)-zin

      if ((x1-x2)**2+(y1-y2)**2+(z1-z2)**2.lt.1.0d-12) return

      ! See bfilament.red for reduce

      if (abs(y1-y2).lt.1.0d-7) y2=y1+1.0d-7
      if (abs(z1-z2).lt.1.0d-7) z2=z1+1.0d-7

      byint=
     &  (-(2.0*(y1-y2)*atan(((y1-y2)*y1+(z1-z2)*z1)/(y1*z2-y2*z1))+
     & (z1-z2)*log(((z1-z2)**2+y2**2+(y1-2.0*y2)*y1+x2**2+(x1-2.0*x2)
     & *x1)*(y1**2+z1**2))-(2.0*(y1-y2)*atan((y1*y2-y2**2+z1*z2-z2**2
     & )/(y1*z2-y2*z1))+(z1-z2)*log((x1**2-2.0*x1*x2+x2**2+y1**2-2.0*
     & y1*y2+y2**2+z1**2-2.0*z1*z2+z2**2)*(y2**2+z2**2))))*(x1-x2))/(
     & (z1-z2)**2+y2**2+(y1-2.0*y2)*y1)
     &  *curnor

      bzint=
     &  (((y1-y2)*log(((z1-z2)**2+y2**2+(y1-2.0*y2)*y1+x2**2+(x1-
     & 2.0*x2)*x1)*(y1**2+z1**2))-2.0*(z1-z2)*atan(((y1-y2)*y1+(z1-z2
     & )*z1)/(y1*z2-y2*z1))-((y1-y2)*log((x1**2-2.0*x1*x2+x2**2+y1**2
     & -2.0*y1*y2+y2**2+z1**2-2.0*z1*z2+z2**2)*(y2**2+z2**2))-2.0*(z1
     & -z2)*atan((y1*y2-y2**2+z1*z2-z2**2)/(y1*z2-y2*z1))))*(x1-x2))/
     & ((z1-z2)**2+y2**2+(y1-2.0*y2)*y1)
     &  *curnor

      return
      end
