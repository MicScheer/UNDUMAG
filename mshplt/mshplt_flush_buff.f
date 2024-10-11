*CMZ :          10/10/2024  10.57.55  by  Michael Scheer
*CMZ :  0.01/02 11/09/2014  12.59.54  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  13.35.55  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_flush_buff

      implicit none

*KEEP,MSHPLTINCL.
      include 'mshplt.cmn'
*KEND.

      integer i

c      write(lun_ps,'(a)')'% begin of mshplt_flush_buff'

      do i=1,ibuffpos_ps
        write(lun_ps,'(a)') chbuff_ps(i)(1:len_trim(chbuff_ps(i)))
      enddo
      flush(lun_ps)
      ibuffpos_ps=0

c      write(lun_ps,'(a)')'% end of mshplt_flush_buff'

      return
      end
