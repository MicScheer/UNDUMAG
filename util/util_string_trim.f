*CMZ :  1.25/00 08/03/2018  14.13.11  by  Michael Scheer
*CMZ :  1.11/05 13/02/2017  14.53.41  by  Michael Scheer
*CMZ :  1.10/00 01/11/2016  16.50.54  by  Michael Scheer
*CMZ : 00.00/19 21/01/2016  13.55.01  by  Michael Scheer
*CMZ : 00.00/06 07/01/2008  14.32.30  by  Michael Scheer
*CMZ :  1.19/07 22/08/2002  15.44.21  by  Michael Scheer
*-- Author :    Michael Scheer   09/11/2001
      subroutine util_string_trim(cline,nfirst,nlast)

      implicit none

      integer nfirst,i,ic,nlast
      character(*) cline
      character c1

      equivalence (ic,c1)
      ic=0

      nfirst=0
      nlast=0

      do i=1,len(cline)
        c1=cline(i:i)
        if (ic.gt.32.and.ic.lt.127) then
          nfirst=i
          exit
        endif
      enddo

      if (nfirst.gt.0) then
        do i=len(cline),nfirst,-1
          c1=cline(i:i)
          if (ic.gt.32.and.ic.lt.127) then
            nlast=i
            exit
          endif
        enddo
      endif

      if (nlast.eq.0) nfirst=1 ! then nlast-nfirst+1 is length of string

      return
      end
