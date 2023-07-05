*CMZ :  0.01/03 23/09/2014  08.55.40  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_clip(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*)
      integer n,i

      if (n.lt.0) then
        n=nclips_ps
        return
      endif

      n=nclips_ps
      do i=1,nclips_ps
        x(n)=clips_ps(1,n)
        y(n)=clips_ps(2,n)
      enddo

      return
      end
