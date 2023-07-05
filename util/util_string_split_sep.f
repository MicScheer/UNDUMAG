*CMZ :  2.02/02 07/07/2021  15.28.57  by  Michael Scheer
*CMZ : 00.00/06 07/03/2007  17.00.51  by  Michael Scheer
*CMZ : 00.00/05 07/03/2007  12.58.44  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2007
      subroutine util_string_split_sep(cline,ndim,nwords,ipos,chsep,istat)

c Input:
c      cline
c      ndim: max. number of words
c      chsep: Seperator

c Output:
c      nwords: number of words
c      ipos(1:2,iword): start and end position of word in cline

      implicit none

      integer ilen,istat,jx,nwords,ndim,ipos(2,ndim),ic,isep,in

      character(*) cline
      character c1,chsep

      equivalence(c1,ic)

      istat=-1

      ilen=len_trim(cline)

      in=0
      nwords=0
      ipos=0

      if (ilen.eq.0) return

      do jx=1,ilen

        c1=cline(jx:jx)

        if (c1.eq.chsep) then
          isep=1
        else
          isep=0
        endif

        if (isep.eq.1) then

          if (in.eq.1) then
c found end of word
c 4.6.2021             if (jx.lt.ilen) then
            ipos(2,nwords)=jx-1
c 4.6.2021             else
c 4.6.2021               ipos(2,nwords)=jx
c 4.6.2021             endif
            in=0
          endif

        else !isep

          if (in.eq.0) then
c found beginning of word
            nwords=nwords+1
            if (nwords.gt.ndim) return
            ipos(1,nwords)=jx
            ipos(2,nwords)=ilen
            in=1
          endif

        endif !isep

      enddo

      if (ipos(1,nwords).ne.0.and.ipos(2,nwords).eq.0) then
        ipos(2,nwords)=len_trim(cline)
      endif

      if (nwords.gt.0) istat=0

      return
      end
