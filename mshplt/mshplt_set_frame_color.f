*CMZ :  1.02/00 03/10/2014  10.54.54  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  16.58.57  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.09.19  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_frame_color(icolor,ired,igreen,iblue)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer icolor,ired,igreen,iblue

      kFrameColor_ps=icolor

      if (icolor.gt.0) then
        if (icolor.eq.1) then
          kFrameRed_ps=0
          kFrameGreen_ps=0
          kFrameBlue_ps=0
        else if (icolor.eq.2) then
          kFrameRed_ps=1
          kFrameGreen_ps=0
          kFrameBlue_ps=0
        else if (icolor.eq.3) then
          kFrameRed_ps=0
          kFrameGreen_ps=1
          kFrameBlue_ps=0
        else if (icolor.eq.4) then
          kFrameRed_ps=0
          kFrameGreen_ps=0
          kFrameBlue_ps=1
        else if (icolor.eq.5) then
          kFrameRed_ps=1
          kFrameGreen_ps=1
          kFrameBlue_ps=0
        else if (icolor.eq.6) then
          kFrameRed_ps=1
          kFrameGreen_ps=0
          kFrameBlue_ps=1
        else if (icolor.eq.7) then
          kFrameRed_ps=0
          kFrameGreen_ps=1
          kFrameBlue_ps=1
        else if (icolor.eq.8) then
          kFrameRed_ps=35
          kFrameGreen_ps=85
          kFrameBlue_ps=33
        endif
      else
        kFrameColor_ps=-1
        kFrameRed_ps=ired
        kFrameGreen_ps=igreen
        kFrameBlue_ps=iblue
      endif !icolor.gt.0

      cnFrame_ps=sqrt(float(kFrameRed_ps**2+kFrameGreen_ps**2+kFrameBlue_ps**2))
      if (cnFrame_ps.le.0) cnFrame_ps=1.

      write(cline_ps,*)
     &  kFrameRed_ps/cnFrame_ps,
     &  kFrameGreen_ps/cnFrame_ps,
     &  kFrameBlue_ps/cnFrame_ps,
     &  ' setrgbcolor'
      call mshplt_fill_buff(cline_ps)

      call mshplt_flush_buff

      return
      end
