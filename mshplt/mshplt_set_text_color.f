*CMZ :  0.01/02 05/09/2014  16.58.57  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.11.41  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_text_color(icolor,ired,igreen,iblue)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer icolor,ired,igreen,iblue

      kTextColor_ps=icolor

      if (icolor.gt.0) then
        if (icolor.eq.1) then
          kTextRed_ps=0
          kTextGreen_ps=0
          kTextBlue_ps=0
        else if (icolor.eq.2) then
          kTextRed_ps=1
          kTextGreen_ps=0
          kTextBlue_ps=0
        else if (icolor.eq.3) then
          kTextRed_ps=0
          kTextGreen_ps=1
          kTextBlue_ps=0
        else if (icolor.eq.4) then
          kTextRed_ps=0
          kTextGreen_ps=0
          kTextBlue_ps=1
        else if (icolor.eq.5) then
          kTextRed_ps=1
          kTextGreen_ps=1
          kTextBlue_ps=0
        else if (icolor.eq.6) then
          kTextRed_ps=1
          kTextGreen_ps=0
          kTextBlue_ps=1
        else if (icolor.eq.7) then
          kTextRed_ps=0
          kTextGreen_ps=1
          kTextBlue_ps=1
        else if (icolor.eq.8) then
          kTextRed_ps=35
          kTextGreen_ps=85
          kTextBlue_ps=33
        endif
      else
        kTextColor_ps=-1
        kTextRed_ps=ired
        kTextGreen_ps=igreen
        kTextBlue_ps=iblue
      endif !icolor.gt.0

      cnText_ps=sqrt(float(kTextRed_ps**2+kTextGreen_ps**2+kTextBlue_ps**2))
      if (cnText_ps.le.0.) cnText_ps=1.

      write(cline_ps,*)
     &  kTextRed_ps/cnText_ps,
     &  kTextGreen_ps/cnText_ps,
     &  kTextBlue_ps/cnText_ps,
     &  ' setrgbcolor'
      call mshplt_fill_buff(cline_ps)

      return
      end
