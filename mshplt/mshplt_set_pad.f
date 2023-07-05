*CMZ :  0.01/02 18/09/2014  12.57.25  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  09.42.51  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  12.37.32  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  08.45.57  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_pad(xleft,xright,ybottom,ytop)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xleft,xright,ybottom,ytop

      write(cline_ps,*)
     &  '% begin of mshplt_set_pad xleft, xright,ybottom, ytop:'
      call mshplt_fill_buff(cline_ps(2:len_trim(cline_ps)))
      write(cline_ps,*)
     &  '%',xleft, xright,ybottom, ytop
      call mshplt_fill_buff(cline_ps(2:len_trim(cline_ps)))

      call mshplt_fill_buff('initclip') !reset clipping

      xleft_ps=xleft
      xright_ps=xright
      ybottom_ps=ybottom
      ytop_ps=ytop

      xsiz_ps=xright-xleft
      ysiz_ps=ytop-ybottom

      write(cline_ps,*)
     &  '% end of mshplt_set_pad xleft, xright,ybottom, ytop:'
      call mshplt_fill_buff(cline_ps(2:len_trim(cline_ps)))
      write(cline_ps,*)
     &  '%',xleft, xright,ybottom, ytop
      call mshplt_fill_buff(cline_ps(2:len_trim(cline_ps)))

      return
      end
