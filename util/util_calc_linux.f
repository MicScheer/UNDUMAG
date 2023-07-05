*CMZ :  1.10/00 01/11/2016  17.43.23  by  Michael Scheer
*CMZ :  0.00/05 13/05/2016  16.05.45  by  Michael Scheer
*CMZ :  0.00/04 10/05/2016  14.52.12  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2016
      subroutine util_calc_linux(clinein,calc)

* +PATCH,//UTIL/MAIN
* +DECK,util_calc_linux.

      implicit none

      double precision calc
      integer lun,lenc,i

      character(*) clinein
      character(4096) command
      character(2048) cwork,cline

      cwork=clinein
      call util_remove_blanks(cwork,cline,lenc)

      do i=1,lenc-1
        if (
     &      cline(i:i+1).eq.'--'
     &      .or.
     &      cline(i:i+1).eq.'++') then
          cline(i:i+1)='+ '
        else if (
     &      cline(i:i+1).eq.'+-'
     &      .or.
     &      cline(i:i+1).eq.'-+') then
          cline(i:i+1)='- '
        endif
      enddo

      if (cline(1:1).eq.'-') then
        cline='0'//cline(1:lenc)
        lenc=lenc+1
      endif

      command="calc -d -C '" // cline(1:lenc) // "' > calc.out"

      call system(trim(command))

      open(newunit=lun,file='calc.out',status='old')
      read(lun,*,err=9,end=9) calc
      close(lun)

      return

9     print*
      print*,"*** Error in util_calc_linux:"
      print*,trim(command)
      print*
      print*,"calc.out:"
      call system("cat calc.out")
      stop

      end
