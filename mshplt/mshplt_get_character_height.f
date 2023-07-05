*CMZ :  0.01/02 22/09/2014  17.03.34  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.23.52  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.49.49  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.55.54  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_get_character_height(siz)

      implicit none

      real siz

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      siz=chhe_ps

      return
      end
