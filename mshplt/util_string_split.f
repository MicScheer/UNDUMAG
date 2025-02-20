*CMZ :  1.00/01 24/09/2014  12.06.33  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  12.14.42  by  Michael Scheer
*CMZ : 00.00/06 07/03/2007  17.00.51  by  Michael Scheer
*CMZ : 00.00/05 07/03/2007  12.58.44  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2007
      subroutine util_string_split(cline,ndim,nwords,ipos,istat)

c Input:
c      cline
c      ndim: max. number of words

c Output:
c      nwords: number of words
c      ipos(1:2,iword): start and end position of word in cline

      implicit none

      integer ilen,istat,jx,nwords,ndim,ipos(2,*),iblank,in
      character(*) cline

      byte ic
      character c1
      equivalence(c1,ic)

      istat=-1

      ilen=len_trim(cline)

      in=0
      nwords=0

      do jx=1,ilen

        c1=cline(jx:jx)

        if (ic.le.32.or.ic.ge.127) then
c found invisible character
          iblank=1
        else
          iblank=0
        endif

        if (iblank.eq.1) then

          if (in.eq.1) then
c found end of word
            if (jx.lt.ilen) then
              ipos(2,nwords)=jx-1
            else
              ipos(2,nwords)=jx
            endif
            in=0
          endif

        else !iblank

          if (in.eq.0) then
c found beginning of word
            nwords=nwords+1
            if (nwords.gt.ndim) return
            ipos(1,nwords)=jx
            ipos(2,nwords)=ilen
            in=1
          endif

        endif !iblank

      enddo


      if (nwords.gt.0) then
        istat=0
      endif

      return
      end
