*CMZ :  1.03/02 25/04/2016  12.25.01  by  Michael Scheer
*CMZ :  1.03/01 10/10/2014  13.26.55  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  10.54.42  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.34.08  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.34.50  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_draw_title

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y
      integer lentit,icolor,ired,igreen,iblue
      character(2048) gtit

      write(lun_ps,'(a)')"% begin of mshplt_draw_title"

      call mshplt_get_text_color(icolor,ired,igreen,iblue)
      call mshplt_set_text_color(icolor,ired,igreen,iblue)

      call mshplt_flush_buff

      if (idrawgtit_ps.ne.1) then
        write(lun_ps,'(a)')"% end of mshplt_draw_title"
        return
      endif

      gtit=gtit_ps
      lentit=len_trim(gtit)

      x=xleftorig_ps+xsizorig_ps/2.
     &  -gsiz_ps*lentit/5.*scaletxt_ps+offgtitx_ps*scaletxt_ps
      y=ybottomorig_ps+ysizorig_ps-offgtity_ps*scaletxt_ps

      if (ihigzmode_ps.eq.0) then
        write(lun_ps,*)'/Helvetica findfont ',gsiz_ps*scaletxt_ps,
     &    ' scalefont setfont'
      else
        write(lun_ps,*)'/Helvetica findfont ',gsiz_ps*scaletxt_ps*1.5,
     &    ' scalefont setfont'
      endif

      write(lun_ps,*)x,y,' moveto'
      write(lun_ps,*)'(',gtit(1:lentit),') show'
      write(lun_ps,*)xleftorig_ps,ybottomorig_ps,' moveto %goto origin'

      if (ihigzmode_ps.eq.0) then
        write(lun_ps,*)'/Helvetica findfont ',chhe_ps*scaletxt_ps,
     &    ' scalefont setfont'
      else
        write(lun_ps,*)'/Helvetica findfont ',chhe_ps*scaletxt_ps*1.5,
     &    ' scalefont setfont'
      endif

      write(lun_ps,'(a)')"% end of mshplt_draw_title"
      return
      end
