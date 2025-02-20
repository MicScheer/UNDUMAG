*CMZ :  1.04/00 12/02/2025  13.10.40  by  Michael Scheer
*CMZ :  1.03/03 03/02/2025  10.22.58  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  16.58.57  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.11.12  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_marker_color(icolor,ired,igreen,iblue)

      use cmapmod

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.
c+seq,cmap.

      integer icolor,ired,igreen,iblue,icol
      real red,green,blue

      if (ksplinecmap.ne.0) then
        red=ired/100000.
        green=igreen/100000.
        blue=iblue/100000.
        write(cline_ps,*)red,green,blue,' setrgbcolor'
        call mshplt_fill_buff(cline_ps)
        return
      endif

      if (icolor.gt.0) then
        icol=mod(icolor,256+1)
        kMarkerColor_ps=icolor
        write(cline_ps,*) cmap(1:3,icol),' setrgbcolor'
        call mshplt_fill_buff(cline_ps)
        return
      else if (icolor.eq.-1) then
          kMarkerRed_ps=0
          kMarkerGreen_ps=0
          kMarkerBlue_ps=0
        else if (icolor.eq.-2) then
          kMarkerRed_ps=1
          kMarkerGreen_ps=0
          kMarkerBlue_ps=0
        else if (icolor.eq.-3) then
          kMarkerRed_ps=0
          kMarkerGreen_ps=1
          kMarkerBlue_ps=0
        else if (icolor.eq.-4) then
          kMarkerRed_ps=0
          kMarkerGreen_ps=0
          kMarkerBlue_ps=1
        else if (icolor.eq.-5) then
          kMarkerRed_ps=1
          kMarkerGreen_ps=1
          kMarkerBlue_ps=0
        else if (icolor.eq.-6) then
          kMarkerRed_ps=1
          kMarkerGreen_ps=0
          kMarkerBlue_ps=1
        else if (icolor.eq.-7) then
          kMarkerRed_ps=0
          kMarkerGreen_ps=1
          kMarkerBlue_ps=1
        else if (icolor.eq.-8) then
          kMarkerRed_ps=35
          kMarkerGreen_ps=85
          kMarkerBlue_ps=33
      else
        kMarkerColor_ps=-9
        kMarkerRed_ps=ired
        kMarkerGreen_ps=igreen
        kMarkerBlue_ps=iblue
      endif !icolor.gt.0

      cnMarker_ps=sqrt(float(kMarkerRed_ps**2+kMarkerGreen_ps**2+kMarkerBlue_ps**2))
      if (cnMarker_ps.le.0) cnMarker_ps=1.

      write(cline_ps,*)
     &  kMarkerRed_ps/cnMarker_ps,
     &  kMarkerGreen_ps/cnMarker_ps,
     &  kMarkerBlue_ps/cnMarker_ps,
     &  ' setrgbcolor'
      call mshplt_fill_buff(cline_ps)

      return
      end
