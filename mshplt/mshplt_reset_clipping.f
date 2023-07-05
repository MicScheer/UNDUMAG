*CMZ :  1.03/01 10/10/2014  13.08.28  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  12.00.19  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 06/08/2014  09.32.19  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.48.35  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_reset_clipping

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      nclips_ps=0

      call mshplt_fill_buff('initclip') !reset clipping

      return
      end
