*CMZ :  2.02/02 06/07/2022  16.13.57  by  Michael Scheer
*CMZ :  2.02/01 20/04/2021  09.46.32  by  Michael Scheer
*CMZ :  2.02/00 27/03/2021  08.34.08  by  Michael Scheer
*CMZ :  2.00/03 23/04/2018  14.22.46  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  14.13.11  by  Michael Scheer
*CMZ :  1.10/00 02/11/2016  16.14.44  by  Michael Scheer
*-- Author :    Michael Scheer   13/10/2016
      subroutine  util_mshcalc_basics(cline,xopy,res)

      implicit none

      integer ndimp
      parameter (ndimp=1000)

      double precision x,y,res,xopy(ndimp)
      integer ndim,nitems,ipos(2,ndimp),istat,i1

      integer lenc,jc,ic1,ic,kbo,kbc,iw,i,nfirst,nlast,
     &  ires
      integer :: nres=0,npow=0,ipow=0
      integer :: nmul=0,imul=0
      integer :: nadd=0,iadd=0

      character(*) cline
      character(4096) cbrack,cwork
      character(32) c32
      character(5) c5
      character(4) c4
      character(2) c2
      character c1

      equivalence(c1,ic1)

      ic1=0
      ndim=ndimp

1     if (len_trim(cline).eq.0) goto 9999

      npow=0
      nmul=0
      nadd=0
      kbo=0
      kbc=0

      lenc=len_trim(cline)
      do ic=1,lenc
        c1=cline(ic:ic)
        if (c1.eq.'(') then
          kbo=ic+1
        else if (c1.eq.')') then
          kbc=ic-1
          exit
        endif
      enddo !lenc

      if (kbo.gt.0) then
        cbrack=cline(kbo:kbc)
      else
        cbrack=cline
      endif

      cwork=cbrack
      cbrack=''
      iw=0
      ic=0
      lenc=len_trim(cwork)
      do jc=1,lenc
        ic=ic+1
        c1=cwork(ic:ic)
        if (c1.ne.' ') then
          iw=iw+1
          cbrack(iw:iw)=c1
        endif
      enddo
      lenc=len_trim(cbrack)
      cwork=cbrack
      ic=0
      iw=0
      do jc=1,lenc
        ic=ic+1
        c2=cwork(ic:ic+1)
        iw=iw+1
        if (c2.eq.'++') then
          cbrack(iw:iw)='+'
          ic=ic+1
        else if (c2.eq.'--') then
          cbrack(iw:iw+1)='-!'
          ic=ic+1
          iw=iw+1
        else if (c2.eq.'+-') then
          cbrack(iw:iw+1)='+!'
          ic=ic+1
          iw=iw+1
        else if (c2.eq.'-+') then
          cbrack(iw:iw)='-'
          ic=ic+1
        else
          cbrack(iw:iw)=c2(1:1)
        endif
      enddo

      cwork=cbrack
      iw=0
      ic=0
      do jc=1,lenc
        ic=ic+1
        c1=cwork(ic:ic)
        if (c1.eq.'+' .or. c1.eq.'-' .or. c1.eq.'/') then
          if (c1.eq.'+'.or.c1.eq.'-') nadd=nadd+1
          if (c1.eq.'/') nmul=nmul+1
          iw=iw+1
          cbrack(iw:iw)=' '
          iw=iw+1
          cbrack(iw:iw)=c1
          iw=iw+1
          cbrack(iw:iw)=' '
        else if (c1.eq.'*') then
          iw=iw+1
          cbrack(iw:iw)=' '
          iw=iw+1
          cbrack(iw:iw)=c1
          if (cwork(ic+1:ic+1).eq.'*') then
            npow=npow+1
            ic=ic+1
            iw=iw+1
            cbrack(iw:iw)=c1
          else
            nmul=nmul+1
          endif
          iw=iw+1
          cbrack(iw:iw)=' '
        else
          iw=iw+1
          cbrack(iw:iw)=c1
        endif
      enddo

      lenc=len_trim(cbrack)
      if (cbrack(1:3).eq.' + '.or.cbrack(1:3).eq.' - ') then
        cbrack=cbrack(2:2)//cbrack(4:4)//cbrack(5:lenc)
        nadd=nadd-1
      endif

      lenc=len_trim(cbrack)
      if (cbrack(1:3).eq.'- -'.or.cbrack(1:3).eq.'+ +') then
        cbrack(1:lenc-3)=cbrack(4:lenc)
        nadd=nadd-1
      else if (cbrack(1:3).eq.'+ -'.or.cbrack(1:3).eq.'- +') then
        cbrack='-'//cbrack(4:lenc)
        nadd=nadd-1
      endif

      cwork=cbrack
      iw=1
      ic=1
      do jc=1,lenc
        c5=cwork(ic:ic+4)
        if (
     &      c5.eq.'  - x' .or.
     &      c5.eq.'  - 0' .or.
     &      c5.eq.'  - 1' .or.
     &      c5.eq.'  - 2' .or.
     &      c5.eq.'  - 3' .or.
     &      c5.eq.'  - 4' .or.
     &      c5.eq.'  - 5' .or.
     &      c5.eq.'  - 6' .or.
     &      c5.eq.'  - 7' .or.
     &      c5.eq.'  - 8' .or.
     &      c5.eq.'  - 9' .or.
     &      c5.eq.'  - .'
     &      ) then
          cbrack(iw+1:iw+2)=c5(3:3)//c5(5:5)
          iw=iw+3
          ic=ic+5
          nadd=nadd-1
        else if (
     &      c5.eq.'  + x' .or.
     &      c5.eq.'  + 0' .or.
     &      c5.eq.'  + 1' .or.
     &      c5.eq.'  + 2' .or.
     &      c5.eq.'  + 3' .or.
     &      c5.eq.'  + 4' .or.
     &      c5.eq.'  + 5' .or.
     &      c5.eq.'  + 6' .or.
     &      c5.eq.'  + 7' .or.
     &      c5.eq.'  + 8' .or.
     &      c5.eq.'  + 9' .or.
     &      c5.eq.'  + .'
     &      ) then
          cbrack(iw+1:iw+2)=c5(3:3)//c5(5:5)
          iw=iw+3
          ic=ic+5
          nadd=nadd-1
        else
          cbrack(iw:iw)=c5(1:1)
          iw=iw+1
          ic=ic+1
        endif
      enddo

      cwork=cbrack
      iw=1
      ic=1
      do jc=1,lenc
        c4=cwork(ic:ic+3)
        if (
     &      c4.eq.'e - ' .or.
     &      c4.eq.'E - ' .or.
     &      c4.eq.'d - ' .or.
     &      c4.eq.'D - '
     &      ) then
          cbrack(iw:iw+1)='d-'
          iw=iw+2
          ic=ic+4
          nadd=nadd-1
        else if (
     &      c4.eq.'e + ' .or.
     &      c4.eq.'E + ' .or.
     &      c4.eq.'d + ' .or.
     &      c4.eq.'D + '
     &      ) then
          cbrack(iw:iw+1)='d+'
          iw=iw+2
          ic=ic+4
          nadd=nadd-1
        else
          cbrack(iw:iw)=c4(1:1)
          iw=iw+1
          ic=ic+1
        endif
      enddo

      ! Evaluate power terms
      do ipow=1,npow
        call  util_string_split(cbrack,ndim,nitems,ipos,istat)
        do i=1,nitems
          if(cbrack(ipos(1,i):ipos(2,i)).eq.'**') then
            if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'-x') then
              i1=ipos(1,i-1)+2
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=-xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'+x') then
              i1=ipos(1,i-1)+2
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)).eq.'x') then
              i1=ipos(1,i-1)+1
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i-1)+1:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else
              i1=ipos(1,i-1)
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i-1):ipos(2,i-1)),*) x
            endif
            c2=cbrack(ipos(1,i):ipos(2,i))
            if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'-x') then
              i1=ipos(1,i-1)+2
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=-xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'+x') then
              i1=ipos(1,i-1)+2
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)).eq.'x') then
              i1=ipos(1,i-1)+1
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i+1)+1:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else
              i1=ipos(1,i-1)
              if (cbrack(i1:i1).eq.'!') cbrack(i1:i1)='-'
              read(cbrack(ipos(1,i+1):ipos(2,i+1)),*) y
            endif
            nres=nres+1
            call util_oper(x,c2,y,xopy(nres))
            write(c32,*)nres
            call  util_string_trim(c32,nfirst,nlast)
            cbrack(ipos(1,i-1):ipos(2,i+1))='x'//c32(nfirst:nlast)
            exit
          endif
        enddo
      enddo

      lenc=len_trim(cbrack)
      do ic=1,lenc-1
        if (cbrack(ic:ic+1).eq.'-!') then
          cbrack(ic:ic+1)=' + '
        else if (cbrack(ic:ic+1).eq.'+!') then
          cbrack(ic:ic+1)=' - '
        endif
      enddo !lenc

      do ic=1,lenc-2
        if (cbrack(ic:ic+2).eq.'- !') then
          cbrack(ic:ic+2)=' + '
        else if (cbrack(ic:ic+2).eq.'+ !') then
          cbrack(ic:ic+2)=' - '
        endif
      enddo !lenc

      ! Evaluate multiplications and divisions terms
      do imul=1,nmul
        call  util_string_split(cbrack,ndim,nitems,ipos,istat)
        do i=1,nitems
          if(
     &        cbrack(ipos(1,i):ipos(2,i)).eq.'*'
     &        .or.
     &        cbrack(ipos(1,i):ipos(2,i)).eq.'/'
     &        ) then
            if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'-x') then
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=-xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'+x') then
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)).eq.'x') then
              read(cbrack(ipos(1,i-1)+1:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else
              read(cbrack(ipos(1,i-1):ipos(2,i-1)),*) x
            endif
            c2=cbrack(ipos(1,i):ipos(2,i))
            if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'-x') then
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=-xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'+x') then
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)).eq.'x') then
              read(cbrack(ipos(1,i+1)+1:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else
              read(cbrack(ipos(1,i+1):ipos(2,i+1)),*) y
            endif
            nres=nres+1
            call util_oper(x,c2,y,xopy(nres))
            write(c32,*)nres
            call  util_string_trim(c32,nfirst,nlast)
            cbrack(ipos(1,i-1):ipos(2,i+1))='x'//c32(nfirst:nlast)
            exit
          endif
        enddo
      enddo

      ! Evaluate addition and substraction terms
      do iadd=1,nadd
        call  util_string_split(cbrack,ndim,nitems,ipos,istat)
        do i=1,nitems
          if(
     &        cbrack(ipos(1,i):ipos(2,i)).eq.'+'
     &        .or.
     &        cbrack(ipos(1,i):ipos(2,i)).eq.'-'
     &        ) then
            if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'-x') then
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=-xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)+1).eq.'+x') then
              read(cbrack(ipos(1,i-1)+2:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else if (cbrack(ipos(1,i-1):ipos(1,i-1)).eq.'x') then
              read(cbrack(ipos(1,i-1)+1:ipos(2,i-1)),*) ires
              x=xopy(ires)
            else
              read(cbrack(ipos(1,i-1):ipos(2,i-1)),*) x
            endif
            c2=cbrack(ipos(1,i):ipos(2,i))
            if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'-x') then
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=-xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)+1).eq.'+x') then
              read(cbrack(ipos(1,i+1)+2:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else if (cbrack(ipos(1,i+1):ipos(1,i+1)).eq.'x') then
              read(cbrack(ipos(1,i+1)+1:ipos(2,i+1)),*) ires
              y=xopy(ires)
            else
              read(cbrack(ipos(1,i+1):ipos(2,i+1)),*) y
            endif
            nres=nres+1
            call util_oper(x,c2,y,xopy(nres))
            write(c32,*)nres
            call  util_string_trim(c32,nfirst,nlast)
            cbrack(ipos(1,i-1):ipos(2,i+1))='x'//c32(nfirst:nlast)
            exit
          endif
        enddo
      enddo

      if (kbo.eq.0.and.(npow.eq.0.and.nmul.eq.0.and.nadd.eq.0)) then
        if (cbrack(1:2).eq.'-x') then
          read(cbrack(3:lenc),*) ires
          xopy(nres)=-xopy(ires)
        else if (cbrack(1:2).eq.'+x') then
          read(cbrack(3:lenc),*) ires
          nres=nres+1
          xopy(nres)=xopy(ires)
        else if (cbrack(1:1).eq.'x') then
          read(cbrack(2:lenc),*) ires
          nres=nres+1
          xopy(nres)=xopy(ires)
        else
          nres=nres+1
          read(cbrack,*)xopy(nres)
        endif
        goto 9999
      else if (kbo.ne.0) then
        cline(kbo-1:kbc+1)=trim(cbrack)
        goto 1
      else
        goto 9999
      endif

      write(c32,*)nres
      call  util_string_trim(c32,nfirst,nlast)
      cline(kbo-1:kbc+1)='x'//c32(nfirst:nlast)
      goto 1

9999  continue

      res=xopy(nres)
      return
      end
