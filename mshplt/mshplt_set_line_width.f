*CMZ :          26/05/2017  10.39.29  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  15.09.19  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.52.16  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.42.32  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_line_width(width)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real width
      rlinewidth_ps=width !cm
      write(cline_ps,*)rlinewidth_ps,' setlinewidth'
      call mshplt_fill_buff(cline_ps)

      return
      end
