*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  11.30.38  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pline3d_closed(n,x,y,z)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer n
      real x(*),y(*),z(*),xx(2),yy(2),zz(2)

      call mshplt_pline3d(n,x,y,z)

      xx(1)=x(n)
      yy(1)=y(n)
      zz(1)=z(n)

      xx(2)=x(1)
      yy(2)=y(1)
      zz(2)=z(1)

      call mshplt_pline3d(2,xx,yy,zz)

      return
      end
