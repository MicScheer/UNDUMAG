*CMZ :  1.01/02 25/09/2014  12.33.10  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 24/08/2014  12.42.28  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.23.52  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.49.49  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.55.54  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_text_angle(tang)

      implicit none

      real tang

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      tang_ps=tang

c      write(cline_ps,*)tang_ps,' rotate'
c      call mshplt_fill_buff(cline_ps)

      return
      end
