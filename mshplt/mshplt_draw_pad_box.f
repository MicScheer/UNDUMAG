*CMZ :  0.01/03 22/09/2014  20.54.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.19.17  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_draw_pad_box

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      call mshplt_line_raw(0.,0.,xsiz_ps,0.)
      call mshplt_line_raw(xsiz_ps,0.,xsiz_ps,ysiz_ps)
      call mshplt_line_raw(xsiz_ps,ysiz_ps,0.,ysiz_ps)
      call mshplt_line_raw(0.,ysiz_ps,0.,0.)

      return
      end
