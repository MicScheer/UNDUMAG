*CMZ :  0.01/03 22/09/2014  21.17.47  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 24/08/2014  13.06.42  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_draw_axis_titles(xtit,ytit)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y,chhe,tsiz,tang
      character(*) xtit,ytit

      chhe=chhe_ps
      tsiz=tsiz_ps

      call mshplt_set_character_height(tsiz)

      if (ihigzmode_ps.ne.0) then
        x=wxmax_ps
     &    -len_trim(xtit)*tsiz_ps/scalex_ps*scaletxt_ps
        y=wymin_ps-ytitoff_ps/scaley_ps*scaletxt_ps
      else
        x=(wxmax_ps+wxmin_ps)/2.
     &    -len_trim(xtit)*tsiz_ps/2./scalex_ps*scaletxt_ps
        y=wymin_ps-ytitoff_ps/scaley_ps*scaletxt_ps
      endif

      call mshplt_text_raw(x,y,xtit)

      if (ihigzmode_ps.ne.0) then
        y=wymax_ps
     &    -len_trim(ytit)*tsiz_ps/scaley_ps*scaletxt_ps
        x=wxmin_ps-xtitoff_ps/scalex_ps*scaletxt_ps
      else
        y=(wymax_ps+wymin_ps)/2.
     &    -len_trim(ytit)*tsiz_ps/2./scaley_ps*scaletxt_ps
        x=wxmin_ps-xtitoff_ps/scalex_ps*scaletxt_ps
      endif

      tang=tang_ps
      tang_ps=90.

      call mshplt_text_raw(x,y,ytit)

      call mshplt_set_character_height(chhe)

      tang_ps=tang

      return
      end
