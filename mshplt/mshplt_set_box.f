*CMZ :  1.02/00 29/09/2014  13.44.41  by  Michael Scheer
*CMZ :  1.01/02 27/09/2014  16.19.04  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_box(ibox)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer ibox

      kbox_ps=ibox

      isbox_ps=1

      return
      end
