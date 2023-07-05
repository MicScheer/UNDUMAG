*CMZ :  0.01/02 22/09/2014  17.05.14  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text_upper(chtext)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) chtext
      real chhe

      chhe=chhe_index_ps
      call mshplt_text_relative_position(chhe/4.,chhe,chtext,chhe)

      return
      end
