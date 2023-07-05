*CMZ :  0.01/02 22/09/2014  13.46.15  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.23.52  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.49.49  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.55.54  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_index_height(siz)

      implicit none

      real siz

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      chhe_index_ps=siz

      if (ihigzmode_ps.eq.0) then
        write(cline_ps,*)'/Helvetica findfont',chhe_index_ps*scaletxt_ps,
     &    ' scalefont setfont'
      else
        write(cline_ps,*)'/Helvetica findfont',chhe_index_ps*scaletxt_ps*1.3,
     &    ' scalefont setfont'
      endif

      call mshplt_fill_buff(cline_ps)

      return
      end
