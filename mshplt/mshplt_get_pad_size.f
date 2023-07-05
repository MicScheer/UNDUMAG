*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  08.43.21  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_pad_size(xsiz,ysiz)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xsiz,ysiz

      xsiz=xsiz_ps
      ysiz=ysiz_ps

      return
      end
