*CMZ :  2.03/00 03/08/2022  14.56.09  by  Michael Scheer
*CMZ :  2.02/01 01/11/2021  07.05.51  by  Michael Scheer
*CMZ :  2.02/00 23/03/2021  20.11.13  by  Michael Scheer
*-- Author :    Michael Scheer   10/09/2020
      subroutine util_shrink_blockchamf(xlin,ylin,zlin,chamf,modech,coat,n,x,y,z)

c +PATCH,//UNDUMAG/UTIL
c +DECK,util_shrink_blockshamf.

      ! shrink block chamfer

      ! if chamf > 0: top chamfer
      ! if chamf < 0: bottom chamfer

      ! modech <0: Upstream chamfer only
      ! modech =0: Upstream and downstream chamfer
      ! modech >0: Downstream chamfer only

      implicit none

      double precision xl,yl,zl,chamf,coat,x(12),y(12),z(12),cen(3),chus,chds,
     &  sigchamf,xlin,ylin,zlin,s2c

      integer modech,i,n,istat

      if (chamf.ge.0.0d0) then
        sigchamf=1.0d0
      else
        sigchamf=-1.0d0
        chamf=-chamf
      endif

      if (chamf.eq.0.0d0.or.coat.gt.chamf) then

        xl=xlin-2.0d0*coat
        yl=ylin-2.0d0*coat
        zl=zlin-2.0d0*coat

        !bottom

        x(1)=-xl/2.0d0
        y(1)=-yl*sigchamf/2.0d0
        z(1)=-zl/2.0d0

        x(2)=xl/2.0d0
        y(2)=-yl*sigchamf/2.0d0
        z(2)=-zl/2.0d0

        x(3)=xl/2.0d0
        y(3)=-yl*sigchamf/2.0d0
        z(3)=zl/2.0d0

        x(4)=-xl/2.0d0
        y(4)=-yl*sigchamf/2.0d0
        z(4)=zl/2.0d0

        !top
        x(5)=-xl/2.0d0
        y(5)=yl*sigchamf/2.0d0
        z(5)=-zl/2.0d0

        x(6)=xl/2.0d0
        y(6)=yl*sigchamf/2.0d0
        z(6)=-zl/2.0d0

        x(7)=xl/2.0d0
        y(7)=yl*sigchamf/2.0d0
        z(7)=zl/2.0d0

        x(8)=-xl/2.0d0
        y(8)=yl*sigchamf/2.0d0
        z(8)=zl/2.0d0

        n=8
        return

      endif

      if (modech.lt.0) then
        chus=chamf
        chds=0.0d0
      else if (modech.gt.0) then
        chus=0.0d0
        chds=chamf
      else
        chus=chamf
        chds=chamf
      endif

      xl=xlin
      yl=ylin
      zl=zlin

      x(1)=-xl/2.0d0+coat
      y(1)=-yl*sigchamf/2.0d0+coat
      z(1)=-zl/2.0d0+coat

      x(2)=xl/2.0d0-coat
      y(2)=-yl*sigchamf/2.0d0+coat
      z(2)=z(1)

      x(6)=x(2)
      y(6)=y(2)
      z(6)=-z(2)

      x(7)=x(1)
      y(7)=y(1)
      z(7)=-z(1)

      if (chus.ne.0.0d0.or.chds.ne.0.0d0) then

        x(3)=x(2)
        y(3)=yl*sigchamf/2.-coat
        z(3)=z(1)

        x(4)=-xl/2.0d0+chamf+coat*(sqrt(2.0d0)-1.0d0)
        y(4)=y(3)
        z(4)=z(1)

        x(5)=x(1)
        y(5)=yl*sigchamf/2.0d0-chamf-coat*(sqrt(2.0d0)-1.0d0)
        z(5)=z(1)

        x(8)=x(5)
        y(8)=y(5)
        z(8)=-z(5)

        x(9)=x(4)
        y(9)=y(4)
        z(9)=-z(4)

        x(10)=x(3)
        y(10)=y(3)
        z(10)=-z(3)

        n=10

        if (chus.eq.0.0d0) x=-x

        if (chds.eq.0.0d0) then
        endif

        return

      endif

      n=12

      x(3)=-x(4)
      y(3)=y(4)

      x(10)=x(3)
      y(10)=y(3)
      z(10)=-z(3)

      x(11)=-x(5)
      y(11)=y(5)
      z(11)=z(5)

      x(12)=x(11)
      y(12)=y(11)
      z(12)=-z(11)
      return
      end
