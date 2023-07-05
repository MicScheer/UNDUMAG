*CMZ :  1.11/01 09/01/2017  13.28.08  by  Michael Scheer
*-- Author :    Michael Scheer   06/01/2017
      subroutine util_read_line(lun,cline,last)
      implicit none

      integer lun,last,ieof,i,nfirst,nlast
      character(*)cline

      cline=''
      call util_skip_comment_end(lun,ieof)

      if (ieof.ne.0) then
        last=-1
        return
      else

        read(lun,'(a)')cline
        call util_string_trim(cline,nfirst,nlast)

        do i=nfirst,nlast
          if (cline(i:i).eq.'!') then
            call util_string_trim(cline(1:i-1),nfirst,nlast)
            exit
          endif
        enddo

        if (nlast.ge.nfirst) then
          cline=cline(nfirst:nlast)
          last=nlast-nfirst+1
        else
          cline=''
          last=0
        endif

      endif

      return
      end
