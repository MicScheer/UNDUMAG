*CMZ :  1.03/02 25/04/2016  15.48.20  by  Michael Scheer
*CMZ :  1.03/01 10/10/2014  13.21.20  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  12.06.56  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  11.47.48  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.17.24  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text_ndc(x,y,text)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x1,y1,x,y
      integer ic,ir,ig,ib
      character(*) text
      character(2048) cline

      itouched_ps=1

      if (icolor_ps.ne.kTextColor_ps) then
        call mshplt_get_text_color(ic,ir,ig,ib)
        call mshplt_set_text_color(ic,ir,ig,ib)
      endif

      x1=xleft_ps+x*xsiz_ps
      y1=ybottom_ps+y*ysiz_ps

      write(cline,*) x1,y1,
     &  ' moveto ', tang_ps,' rotate (',text(1:len_trim(text)),') show ',
     &  -tang_ps,' rotate'
      call mshplt_fill_buff(cline)

      return
      end
