*CMZ :  0.01/03 23/09/2014  12.13.27  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  17.12.02  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_text_upper_lower(chup,chlow)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) chup,chlow

      call mshplt_text_upper(chup)
      call mshplt_text_lower(chlow)

      return
      end
