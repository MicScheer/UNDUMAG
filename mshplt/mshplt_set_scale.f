*CMZ :  1.03/01 10/10/2014  08.54.46  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  11.57.12  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.02.07  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  15.28.51  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  11.35.10  by  Michael Scheer
*CMZ : 00.00/02 30/06/2014  10.16.31  by  Michael Scheer
*-- Author :    Michael Scheer   27/06/2014
      subroutine mshplt_set_scale(scale)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real scale

      scale_ps=scale

      if (isscale_ps.ne.1) then
        isscale_ps=1
      else
        write(cline_ps,*)scale_ps,scale_ps,' scale'
        call mshplt_fill_buff(cline_ps)
      endif

      return
      end
