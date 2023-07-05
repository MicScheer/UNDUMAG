*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.01/00 23/08/2014  09.39.32  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_pause

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(64) cans

      write(6,*)
      write(6,*)'Hit any key to continue or type "stop" to terminate the programm!'
      read(5,'(a)')cans

      if(cans.eq.'stop'.or.cans.eq.'STOP') then
        call mshplt_end
        stop
      endif

      return
      end
