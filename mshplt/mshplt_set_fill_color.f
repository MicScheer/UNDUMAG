*CMZ :  1.04/00 12/02/2025  13.20.22  by  Michael Scheer
*CMZ :  1.03/03 03/02/2025  10.22.58  by  Michael Scheer
*CMZ :  1.02/00 29/09/2014  10.07.08  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.09.19  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_fill_color(icolor,ired,igreen,iblue)

      use cmapmod

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.
c+seq,cmap.

      integer icolor,ired,igreen,iblue,icol
      real red,green,blue

      kFillColor_ps=icolor

      if (ksplinecmap.ne.0) then
        red=ired/100000.
        green=igreen/100000.
        blue=iblue/100000.
        kFillRed_ps=ired
        kFillGreen_ps=igreen
        kFillBlue_ps=iblue
        write(cline_ps,*)red,green,blue,' setrgbcolor'
        call mshplt_fill_buff(cline_ps)
        return
      endif

      if (icolor.gt.0) then
        icol=mod(icolor,256+1)
        write(cline_ps,*) cmap(1:3,icol),' setrgbcolor'
        kFillColor_ps=icolor
        call mshplt_fill_buff(cline_ps)
        return
      else if (icolor.eq.-1) then
        kFillRed_ps=0
        kFillGreen_ps=0
        kFillBlue_ps=0
      else if (icolor.eq.-2) then
        kFillRed_ps=1
        kFillGreen_ps=0
        kFillBlue_ps=0
      else if (icolor.eq.-3) then
        kFillRed_ps=0
        kFillGreen_ps=1
        kFillBlue_ps=0
      else if (icolor.eq.-4) then
        kFillRed_ps=0
        kFillGreen_ps=0
        kFillBlue_ps=1
      else if (icolor.eq.-5) then
        kFillRed_ps=1
        kFillGreen_ps=1
        kFillBlue_ps=0
      else if (icolor.eq.-6) then
        kFillRed_ps=1
        kFillGreen_ps=0
        kFillBlue_ps=1
      else if (icolor.eq.-7) then
        kFillRed_ps=0
        kFillGreen_ps=1
        kFillBlue_ps=1
      else if (icolor.eq.-8) then
        kFillRed_ps=35
        kFillGreen_ps=85
        kFillBlue_ps=33
      else
        kFillColor_ps=-9
        kFillRed_ps=ired
        kFillGreen_ps=igreen
        kFillBlue_ps=iblue
      endif !icolor.gt.0

      cnFill_ps=sqrt(float(kFillRed_ps**2+kFillGreen_ps**2+kFillBlue_ps**2))
      if (cnFill_ps.le.0.) cnFill_ps=1.

      write(cline_ps,*)
     &  kFillRed_ps/cnFill_ps,
     &  kFillGreen_ps/cnFill_ps,
     &  kFillBlue_ps/cnFill_ps,
     &  ' setrgbcolor'

      call mshplt_fill_buff(cline_ps)

      return
      end
