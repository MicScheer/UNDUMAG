*CMZ :  1.01/02 27/09/2014  15.10.10  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  15.03.41  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.47.46  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_newpage

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer ierr
      character(2048) file
      data file/'filedummy'/

      if (itouched_ps.eq.0) return

      call mshplt_flush_buff

      inewpage_ps=1
      itouched_ps=0
      call mshplt_file_open(-1,file(1:len_trim(file)),'',ierr)

      return
      end
