*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.09.19  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_line_color(icolor,ired,igreen,iblue)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer icolor,ired,igreen,iblue

      kLineColor_ps=icolor

      if (icolor.gt.0) then
        if (icolor.eq.1) then
          kLineRed_ps=0
          kLineGreen_ps=0
          kLineBlue_ps=0
        else if (icolor.eq.2) then
          kLineRed_ps=1
          kLineGreen_ps=0
          kLineBlue_ps=0
        else if (icolor.eq.3) then
          kLineRed_ps=0
          kLineGreen_ps=1
          kLineBlue_ps=0
        else if (icolor.eq.4) then
          kLineRed_ps=0
          kLineGreen_ps=0
          kLineBlue_ps=1
        else if (icolor.eq.5) then
          kLineRed_ps=1
          kLineGreen_ps=1
          kLineBlue_ps=0
        else if (icolor.eq.6) then
          kLineRed_ps=1
          kLineGreen_ps=0
          kLineBlue_ps=1
        else if (icolor.eq.7) then
          kLineRed_ps=0
          kLineGreen_ps=1
          kLineBlue_ps=1
        else if (icolor.eq.8) then
          kLineRed_ps=35
          kLineGreen_ps=85
          kLineBlue_ps=33
        endif
      else
        kLineColor_ps=-1
        kLineRed_ps=ired
        kLineGreen_ps=igreen
        kLineBlue_ps=iblue
      endif !icolor.gt.0

      cnLine_ps=sqrt(float(kLineRed_ps**2+kLineGreen_ps**2+kLineBlue_ps**2))
      if (cnLine_ps.le.0.) cnLine_ps=1.

      write(cline_ps,*)
     &  kLineRed_ps/cnLine_ps,
     &  kLineGreen_ps/cnLine_ps,
     &  kLineBlue_ps/cnLine_ps,
     &  ' setrgbcolor'
      call mshplt_fill_buff(cline_ps)

      return
      end
