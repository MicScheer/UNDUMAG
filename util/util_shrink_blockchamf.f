*CMZ :  2.04/18 14/09/2023  07.36.02  by  Michael Scheer
*CMZ :  2.03/00 03/08/2022  14.56.09  by  Michael Scheer
*CMZ :  2.02/01 01/11/2021  07.05.51  by  Michael Scheer
*CMZ :  2.02/00 23/03/2021  20.11.13  by  Michael Scheer
*-- Author :    Michael Scheer   10/09/2020
      subroutine util_shrink_blockchamf(xlin,ylin,zlin,chamf,modech,coat,
     &  n,x,y,z)

c +PATCH,//UNDUMAG/UTIL
c +DECK,util_shrink_blockshamf.

      ! shrink block chamfer

      ! if chamf > 0: top chamfer
      ! if chamf < 0: bottom chamfer

      ! modech <0: Upstream chamfer only
      ! modech =0: Upstream and downstream chamfer
      ! modech >0: Downstream chamfer only

      implicit none

      double precision xl,yl,zl,chamf,coat,x(12),y(12),z(12),cen(3),
     &  xlin,ylin,zlin,xus,xds,ybot,ytop,xusch,xdsch,ytopch,zz,chamfa

      integer modech,i,n,istat

      chamfa=abs(chamf)

      if (chamfa.eq.0.0d0.or.coat.gt.chamfa) then

        xl=xlin-2.0d0*coat
        yl=ylin-2.0d0*coat
        zl=zlin-2.0d0*coat

        !bottom

        x(1)=-xl/2.0d0
        y(1)=-yl/2.0d0
        z(1)=-zl/2.0d0

        x(2)=xl/2.0d0
        y(2)=-yl/2.0d0
        z(2)=-zl/2.0d0

        x(3)=xl/2.0d0
        y(3)=-yl/2.0d0
        z(3)=zl/2.0d0

        x(4)=-xl/2.0d0
        y(4)=-yl/2.0d0
        z(4)=zl/2.0d0

        !top

        x(5)=-xl/2.0d0
        y(5)=yl/2.0d0
        z(5)=-zl/2.0d0

        x(6)=xl/2.0d0
        y(6)=yl/2.0d0
        z(6)=-zl/2.0d0

        x(7)=xl/2.0d0
        y(7)=yl/2.0d0
        z(7)=zl/2.0d0

        x(8)=-xl/2.0d0
        y(8)=yl/2.0d0
        z(8)=zl/2.0d0

        n=8

      endif

      xl=xlin
      yl=ylin
      zl=zlin

      zz=zl/2.0d0-coat

      xus=-xl/2.0d0+coat
      xusch=-xl/2.0d0+chamfa+coat*(sqrt(2.0d0)-1.0d0)
      xds=-xus
      xdsch=-xusch

      ybot=-yl/2.0d0+coat
      ytop=-ybot
      ytopch=yl/2.0d0-chamfa-coat*(sqrt(2.0d0)-1.0d0)

      if (modech.eq.0) then

        n=12

        ! top chamfaer

        x(1)=xus
        y(1)=ybot
        z(1)=zz

        x(2)=xds
        y(2)=ybot
        z(2)=zz

        x(3)=xds
        y(3)=ytopch
        z(3)=zz

        x(4)=xdsch
        y(4)=ytop
        z(4)=zz

        x(5)=xusch
        y(5)=ytop
        z(5)=zz

        x(6)=xus
        y(6)=ytopch
        z(6)=zz

        x(7:12)=x(1:6)
        y(7:12)=y(1:6)
        z(7:12)=-z(1:6)

        if (chamfa.lt.0.0d0) then
          y=-y
        endif

      else

        n=10

        ! top chamfaer

        x(1)=xus
        y(1)=ybot
        z(1)=zz

        x(2)=xds
        y(2)=ybot
        z(2)=zz

        x(3)=xds
        y(3)=ytopch
        z(3)=zz

        x(4)=xdsch
        y(4)=ytop
        z(4)=zz

        x(5)=xus
        y(5)=ytop
        z(5)=zz

        x(6:10)=x(1:5)
        y(6:10)=y(1:5)
        z(6:10)=-z(1:5)

        if (modech.lt.0) then
          x=-x
        endif

      endif !modech

      if (chamf.lt.0.0d0) then
        y=-y
      endif

      return
      end
