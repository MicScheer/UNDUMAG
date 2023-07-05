*CMZ :  1.02/01 03/10/2014  14.42.26  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.10.09  by  Michael Scheer
*CMZ :  0.00/04 08/08/2014  16.20.54  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_color(icolor,ired,igreen,iblue)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer icolor,ired,igreen,iblue

      if (icolor.gt.0) then
        if (icolor.eq.1) then
          ired_ps=0
          igreen_ps=0
          iblue_ps=0
        else if (icolor.eq.2) then
          ired_ps=1
          igreen_ps=0
          iblue_ps=0
        else if (icolor.eq.3) then
          ired_ps=0
          igreen_ps=1
          iblue_ps=0
        else if (icolor.eq.4) then
          ired_ps=0
          igreen_ps=0
          iblue_ps=1
        else if (icolor.eq.5) then
          ired_ps=1
          igreen_ps=1
          iblue_ps=0
        else if (icolor.eq.6) then
          ired_ps=1
          igreen_ps=0
          iblue_ps=1
        else if (icolor.eq.7) then
          ired_ps=0
          igreen_ps=1
          iblue_ps=1
        else if (icolor.eq.8) then
          ired_ps=35
          igreen_ps=85
          iblue_ps=33
        endif
      else
        icolor_ps=-1
        ired_ps=ired
        igreen_ps=igreen
        iblue_ps=iblue
      endif

      coln_ps=sqrt(float(ired_ps**2+igreen_ps**2+iblue_ps**2))
      if (icolor_ps.lt.0.or.coln_ps.le.0.0) coln_ps=1.

      write(cline_ps,*)
     &  ired_ps/coln_ps,
     &  igreen_ps/coln_ps,
     &  iblue_ps/coln_ps,
     &  ' setrgbcolor'
      call mshplt_fill_buff(cline_ps(1:len_trim(cline_ps)))

      return
      end
