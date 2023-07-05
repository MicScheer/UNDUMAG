*CMZ :  1.03/01 09/10/2014  15.43.42  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.06.28  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_date_position(offx,offy)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real offx,offy

      offdatex_ps=offx
      offdatey_ps=offy

      isoffdate_ps=1

      return
      end
