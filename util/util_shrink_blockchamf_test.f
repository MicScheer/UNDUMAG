*CMZ :  2.03/00 01/08/2022  14.42.23  by  Michael Scheer
*CMZ :  2.02/01 01/11/2021  07.05.51  by  Michael Scheer
*CMZ :  2.02/00 23/03/2021  20.11.13  by  Michael Scheer
*-- Author :    Michael Scheer   10/09/2020
      subroutine util_shrink_blockchamf_test(xl,yl,zl,chamf,modech,coat,n,x,y,z)

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
     &  sigchamf

      integer modech,i,n,istat


      if (chamf.ge.0.0d0) then
        sigchamf = 1.0d0
      else
        sigchamf = -1.0d0
        chamf = - chamf
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

      !bottom
      x(1)=-xl/2.0d0
      y(1)=-yl/2.0d0+coat
      z(1)=-zl/2.0d0

      x(2)=xl/2.0d0
      y(2)=-yl/2.0d0+coat
      z(2)=-zl/2.0d0

      x(3)=xl/2.0d0
      y(3)=-yl/2.0d0+coat
      z(3)=zl/2.0d0

      x(4)=-xl/2.0d0
      y(4)=-yl/2.0d0+coat
      z(4)=zl/2.0d0

      !middle
      x(5)=-xl/2.0d0
      y(5)=yl/2.0d0-chamf
      z(5)=-z l/2.0d0

      x(6)=xl/2.0d0
      y(6)=yl/2.0d0-chamf
      z(6)=-zl/2.0d0

      x(7)=xl/2.0d0
      y(7)=yl/2.0d0-chamf
      z(7)=zl/2.0d0

      x(8)=-xl/2.0d0
      y(8)=yl/2.0d0-chamf
      z(8)=zl/2.0d0

      x(9)=-xl/2.0d0+chus
      y(9)=yl/2.0d0
      z(9)=-zl/2.0d0

      x(10)=xl/2.0d0-chds
      y(10)=yl/2.0d0
      z(10)=-zl/2.0d0

      x(11)=xl/2.0d0-chds
      y(11)=yl/2.0d0
      z(11)=zl/2.0d0

      x(12)=-xl/2.0d0+chus
      y(12)=yl/2.0d0
      z(12)=zl/2.0d0

      if (sigchamf.lt.0.0d0) then
        do i = 1,12
          y(i)=-y(i)
        enddo
      endif

c      if (coat.eq.0.0d0) return

      if (abs(chamf).eq.0.0d0) then
        do i=1,12
          if (x(i).gt.0.0d0) then
            x(i)=x(i)-coat
          else
            x(i)=x(i)+coat
          endif
          if (y(i).gt.0.0d0) then
            y(i)=y(i)-coat
          else
            y(i)=y(i)+coat
          endif
          if (z(i).gt.0.0d0) then
            z(i)=z(i)-coat
          else
            z(i)=z(i)+coat
          endif
        enddo
        return
      endif

      cen=0.0d0
      call util_shrink_xyz(12,x,y,z,cen,coat,n,x,y,z,istat)

      return
      end
