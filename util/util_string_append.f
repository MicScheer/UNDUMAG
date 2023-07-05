*CMZ :  1.11/04 18/01/2017  13.16.39  by  Michael Scheer
*CMZ : 00.00/19 21/01/2016  13.55.01  by  Michael Scheer
*CMZ : 00.00/06 07/01/2008  14.32.30  by  Michael Scheer
*CMZ :  1.19/07 22/08/2002  15.44.21  by  Michael Scheer
*-- Author :    Michael Scheer   09/11/2001
      subroutine util_string_append(cline,cword,nfirst,nlast)

      implicit none

      integer nfirst,nlast,nl1,nl2,nw1,nw2,lword,lenc
      character(*) cline,cword

      lenc=len(cline)

      call util_string_trim(cline,nl1,nl2)

      if (nl2.eq.lenc) then
        nfirst=nl1
        nlast=nl2
        return
      endif

      call util_string_trim(cword,nw1,nw2)
      lword=nw2-nw1+1

      if (lword+nl2.le.len(cline)) then
        cline=cline(1:nl2)//cword(nw1:nw2)
        nfirst=nl1
        nlast=nl2+lword
      else
        cline=cline(1:nl2)//cword(nw1:nw1+lenc-nl2-1)
        nfirst=nl1
        nlast=lenc
      endif

      return
      end
