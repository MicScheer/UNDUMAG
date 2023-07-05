*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  15.53.15  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  16.13.05  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_line_style(istyle)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer istyle

      if (istyle.eq.ilinestyle_ps) return

      if (istyle.ge.0.and.istyle.le.4) then

        ilinestyle_ps=istyle

        if (ilinestyle_ps.eq.1) then
          write(cline_ps,*)'[] 0 setdash'
        else if (ilinestyle_ps.eq.2) then
          write(cline_ps,*)'[',6*rlinewidth_ps, 3*rlinewidth_ps,'] 0 setdash'
        else if (ilinestyle_ps.eq.3) then
          write(cline_ps,*)'[',2*rlinewidth_ps, 2*rlinewidth_ps,'] 0 setdash'
        else if (ilinestyle_ps.eq.4) then
          write(cline_ps,*)'[',
     &      8*rlinewidth_ps,
     &      4*rlinewidth_ps,
     &      2*rlinewidth_ps,
     &      4*rlinewidth_ps,
     &      '] 0 setdash'
        endif

        call mshplt_fill_buff(cline_ps)

      endif

      return
      end
