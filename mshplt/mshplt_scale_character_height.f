*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  18.31.12  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_scale_character_height(fac)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real fac,chhe

      chhe=chhe_ps
      call mshplt_set_character_height(fac*chhe)

      return
      end
