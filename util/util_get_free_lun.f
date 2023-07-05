*CMZ : 00.00/16 10/10/2014  16.25.08  by  Michael Scheer
*CMZ : 00.00/07 21/07/2009  14.58.29  by  Michael Scheer
*CMZ : 00.00/06 12/07/2007  15.45.32  by  Michael Scheer
*-- Author :    Michael Scheer   12/07/2007
      subroutine util_get_free_lun(lun)

      integer lun,luni

      logical isopen

      luni=abs(lun)

1     continue
      inquire(unit=luni,opened=isopen)
      if (isopen) then
        luni=luni+1
        goto 1
      endif

      lun=luni

      return
      end
