*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 21/08/2014  16.54.44  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.55.55  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_axis_label_offset(x,y)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x,y

      xlaboff_ps=x
      ylaboff_ps=y

      return
      end
