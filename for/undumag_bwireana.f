*CMZ :  2.02/00 26/02/2021  15.54.27  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  10.15.11  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2018
      subroutine undumag_bwireana(wire7,xin,yin,zin,bxout,byout,bzout)

c Units: A, T and m

      implicit none

      double precision xin,yin,zin,bxout,byout,bzout,
     &  rx,ry,rz,dxl,dyl,dzl,xl,yl,zl,dbx,dby,dbz,xc,yc,zc,r,
     &  bx,by,bz,wire7(7),r3,curr,tiny,rmu04pi,
     &  x0,y0,z0,cen(3),wrot(6),wwrot(6),costhe,sinthe,sinphi,cosphi,
     &  dl(3),dln(3),dlabs,xh,byl,byh,xr,yr,zr,xrr,yrr,zrr,rl,rh,
     &  bxr,byr,bzr,bxrr,byrr,bzrr,bzl,bzh,curnor,dlxy

      data tiny/1.0d-6/
      data rmu04pi/1.0d-7/

      integer iseg,nseg

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      curr=wire7(1)
      curnor=curr*rmu04pi

      dl(1)=wire7(5)-wire7(2)
      dl(2)=wire7(6)-wire7(3)
      dl(3)=wire7(7)-wire7(4)

      dlabs=sqrt(dl(1)**2+dl(2)**2+dl(3)**2)
      if (dlabs*curr.eq.0.0d0) return
      dln=dl/dlabs

c translate and rotate wire such, that it is centered a (0,0,0) and on the
c x-axis

      dlxy=sqrt(dln(1)**2+dln(2)**2)
      if (dlxy.gt.0.0d0) then
        costhe=dln(1)/dlxy
        sinthe=dln(2)/dlxy
      else
        costhe=1.0d0
        sinthe=0.0d0
      endif

      wrot(1)=costhe*wire7(2)+sinthe*wire7(3)
      wrot(2)=-sinthe*wire7(2)+costhe*wire7(3)
      wrot(3)=wire7(4)

      wrot(4)=costhe*wire7(5)+sinthe*wire7(6)
      wrot(5)=-sinthe*wire7(5)+costhe*wire7(6)
      wrot(6)=wire7(7)

      xr=costhe*xin+sinthe*yin
      yr=-sinthe*xin+costhe*yin
      zr=zin

      dl=wrot(4:6)-wrot(1:3)
      dln=dl/dlabs
      cosphi=dln(1)
      sinphi=dln(3)

      wwrot(1)=cosphi*wrot(1)+sinphi*wrot(3)
      wwrot(2)=wrot(2)
      wwrot(3)=-sinphi*wrot(1)+cosphi*wrot(3)

      wwrot(4)=cosphi*wrot(4)+sinphi*wrot(6)
      wwrot(5)=wrot(5)
      wwrot(6)=-sinphi*wrot(4)+cosphi*wrot(6)

      xrr=cosphi*xr+sinphi*zr
      yrr=yr
      zrr=-sinphi*xr+cosphi*zr

      cen(1)=(wwrot(1)+wwrot(4))/2.0d0
      cen(2)=(wwrot(2)+wwrot(5))/2.0d0
      cen(3)=(wwrot(3)+wwrot(6))/2.0d0

      x0=xrr-cen(1)
      y0=yrr-cen(2)
      z0=zrr-cen(3)

      xl=-(wwrot(4)-wwrot(1))/2.0d0
      xh=-xl
      rl=sqrt((xl-x0)**2+y0**2+z0**2)
      rh=sqrt((xh-x0)**2+y0**2+z0**2)

      byl=((rl**2+rl*(xl-x0))*(-z0))/(rl**2*(y0**2+z0**2))
      byh=((rh**2+rh*(xh-x0))*(-z0))/(rh**2*(y0**2+z0**2))

      bzl=-((rl**2+rl*(xl-x0))*(-y0))/(rl**2*(y0**2+z0**2))
      bzh=-((rh**2+rh*(xh-x0))*(-y0))/(rh**2*(y0**2+z0**2))

      bxrr=0.0d0
      byrr=byh-byl
      bzrr=bzh-bzl

      bxr=cosphi*bxrr-sinphi*bzrr
      byr=byrr
      bzr=sinphi*bxrr+cosphi*bzrr

      bxout=(costhe*bxr-sinthe*byr)*curnor
      byout=(sinthe*bxr+costhe*byr)*curnor
      bzout=bzr*curnor

      if (abs(bxout)+abs(byout)+abs(bzout).gt.10.0d0) then
        print*,"Undumag_bwireana: Field above 10 Tesla"
        print*,"wire7"
        print*,"xin,yin,zin:",xin,yin,zin
      endif

      return
      end
