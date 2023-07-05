*CMZ :  0.01/03 23/09/2014  12.00.19  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_clip(n,x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

c Controls clipping:
c      n=0: return -nclips_ps
c      n<0: Activate clipping for already defined clips_ps
c      n>2: Set clipping for (x,y) path
c     else: return

c see also: mshplt_get_clip

      real x(*),y(*),x1,y1,x2,y2
      integer n,i
      character(2048) cline

      if (n.gt.0.and.n.le.2) then
        nclips_ps=0
        return
      endif

      write(cline,*)'initclip' !reset clipping
      call mshplt_fill_buff(cline)

      if (n.eq.0) then
        n=-nclips_ps
        return
      endif

c Just to switch clipping on for already defined x,y
      if (n.lt.0) then

        nclips_ps=-n

        if (n.gt.1000) then
          Print*,'*** Error in mshplt_clip: Dimension exceeded ***'
          return
        endif

        x1=xleft_ps+scalex_ps*(clips_ps(1,1)-wxmin_ps)
        y1=ybottom_ps+scaley_ps*(clips_ps(2,1)-wymin_ps)

        write(cline,*)'newpath ', x1,y1,' moveto'
        call mshplt_fill_buff(cline)

        do i=2,nclips_ps
          x2=xleft_ps+scalex_ps*(clips_ps(1,i)-wxmin_ps)
          y2=ybottom_ps+scaley_ps*(clips_ps(2,i)-wymin_ps)
          write(cline,*)x1,y1,x2,y2,' lineto'
          call mshplt_fill_buff(cline)
          x1=x2
          y1=y2
        enddo

        write(cline,*)'closepath clip newpath'
        call mshplt_fill_buff(cline)

      else ! if (n.lt.0) then

        nclips_ps=n

        if (n.gt.1000) then
          Print*,'*** Error in mshplt_clip: Dimension exceeded ***'
          return
        endif

        nclips_ps=n
        clips_ps(1,1)=x(1)
        clips_ps(2,1)=y(1)

        x1=xleft_ps+scalex_ps*(x(1)-wxmin_ps)
        y1=ybottom_ps+scaley_ps*(y(1)-wymin_ps)

        write(cline,*)'newpath ', x1,y1,' moveto'
        call mshplt_fill_buff(cline)

        do i=2,nclips_ps
          clips_ps(1,i)=x(i)
          clips_ps(2,i)=y(i)
          x2=xleft_ps+scalex_ps*(x(i)-wxmin_ps)
          y2=ybottom_ps+scaley_ps*(y(i)-wymin_ps)
          write(cline,*)x1,y1,x2,y2,' lineto'
          call mshplt_fill_buff(cline)
          x1=x2
          y1=y2
        enddo

        write(cline,*)'closepath clip newpath'
        call mshplt_fill_buff(cline)

      endif ! if (n.lt.0) then

      return
      end
