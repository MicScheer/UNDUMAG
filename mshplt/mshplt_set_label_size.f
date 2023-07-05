*CMZ :  0.01/02 11/09/2014  14.09.59  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.23.52  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  11.49.49  by  Michael Scheer
*CMZ :  0.00/02 10/07/2014  15.55.54  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_label_size(siz)

      implicit none

      real siz

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      aLabHeight_ps=siz

      call mshplt_fill_buff('% begin of mshplt_set_label_size')

      if (ihigzmode_ps.eq.0) then
        write(cline_ps,*)'/Helvetica findfont',aLabHeight_ps*scaletxt_ps,
     &    ' scalefont setfont'
      else
        write(cline_ps,*)'/Helvetica findfont',aLabHeight_ps*scaletxt_ps*1.3,
     &    ' scalefont setfont'
      endif

      call mshplt_fill_buff(cline_ps)

      call mshplt_fill_buff('% end of mshplt_set_label_size')

      return
      end
