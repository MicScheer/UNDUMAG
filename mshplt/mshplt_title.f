*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  10.04.36  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.34.08  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.34.50  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_title(title)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) title

      gtit_ps=title
      idrawgtit_ps=1

      return
      end
