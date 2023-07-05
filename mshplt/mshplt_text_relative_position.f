*CMZ :  1.01/02 25/09/2014  14.49.04  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  11.31.17  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  17.02.18  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text_relative_position(dx,dy,chtext,chhe)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real dx,dy,chhe,chheo
      character(*) chtext

      chheo=chhe_ps

      call mshplt_flush_buff
      call mshplt_fill_buff('gsave currentpoint pop 0 translate')
      call mshplt_set_character_height(chhe)
      write(cline_ps,*)dx,dy,' rmoveto ',tang_ps,' rotate 0 0 ('
     &  //chtext(1:len_trim(chtext))//
     &  ') show ',-tang_ps,' rotate grestore'
      call mshplt_fill_buff(cline_ps)
      call mshplt_set_character_height(chheo)

      return
      end
