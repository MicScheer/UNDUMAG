*CMZ :  1.25/00 08/03/2018  14.13.11  by  Michael Scheer
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

        integer ilen,istat,jx,nwords,ndim,ipos(2,ndim),ic,iblank,in

        character(*) cline
        character c1

        equivalence(c1,ic)

        ic=0
        istat=-1

        ilen=len(cline)

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
              in=1
            endif

          endif !iblank

        enddo

        if (nwords.gt.0) istat=0

	return
	end
