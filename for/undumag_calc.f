*CMZ :  2.04/00 14/01/2023  14.39.42  by  Michael Scheer
*CMZ :  2.02/02 06/07/2022  15.16.53  by  Michael Scheer
*CMZ :  2.02/01 04/10/2021  12.42.59  by  Michael Scheer
*CMZ :  2.02/00 30/03/2021  16.58.55  by  Michael Scheer
*CMZ :  2.01/08 28/07/2020  11.52.35  by  Michael Scheer
*CMZ :  2.01/05 21/07/2020  12.59.07  by  Michael Scheer
*CMZ :  1.25/00 24/01/2018  15.15.53  by  Michael Scheer
*CMZ :  1.17/08 30/05/2017  14.55.39  by  Michael Scheer
*CMZ :  1.11/01 09/01/2017  13.51.01  by  Michael Scheer
*CMZ :  1.11/00 06/12/2016  19.35.14  by  Michael Scheer
*CMZ :  1.10/02 25/11/2016  12.01.14  by  Michael Scheer
*CMZ :  1.10/00 11/11/2016  12.55.12  by  Michael Scheer
*CMZ :  1.02/00 22/08/2016  09.11.46  by  Michael Scheer
*CMZ :  0.00/10 13/07/2016  09.36.23  by  Michael Scheer
*CMZ :  0.00/09 30/06/2016  15.12.31  by  Michael Scheer
*CMZ :  0.00/05 10/06/2016  15.58.48  by  Michael Scheer
*CMZ :  0.00/04 11/05/2016  11.22.46  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2016
      subroutine undumag_calc(kecho,mode)

      use commandlinef90m

      implicit none

      save

      integer nparp
      parameter (nparp=10000)

      double precision par(nparp),result,parlen(nparp),parcop(nparp)

      integer itry,ntry,npar,ipar,kpar(nparp),lunin,lunout,luncld,nclc,
     &  linepa,linepe,lineva,lineve,nbuff,ibuff,ivar,nvar,i,lenc,lenck,lenw,k,
     &  ifound,lencc,lenc1,lenc2,kecho,mode,
     &  nfirst,nlast,ifirstvar,
     &  nfirst1,nlast1,
     &  nfirstw,nlastw,
     &  nfirst2,nlast2,kconvhull(nparp),mtetmesh,ntetmesh,nplan,itet,ieof,
     &  kcomm,ianf,iend,istat,nhead,ktest,icount

      logical lexist

      character(128) cpar(nparp),cparcop(nparp), cword

      ! cbuff darf nicht zu grosz sein!!
      character(512) cbuff(nparp),cline,cfiletet(nparp),
     &  cpart1,cpart2,
     &  clc(100000),cld(100000),chead(100000),cvar(nparp)

      character c1,c2

      inquire(file=trim(Fclc),exist=lexist)

      if (lexist.eqv..false.) then
        write(lun6,*)""
        write(lun6,*)"*** Error in undumag_calc: File " // trim(Fclc) // " not found ***"
        write(lun6,*)"*** Program UNDUMAG aborted ***"
        stop
      endif

      open(newunit=luncld,file=trim(Fclc),status='old')

      npar=0
      nbuff=0
      linepa=0
      nvar=0
      ntetmesh=0
      kconvhull=0
      lineva=0
      lineve=0
      nhead=0

      nclc=0
      do while (.true.)
        read(luncld,'(a)',end=91) cline
        nclc=nclc+1
        c1=cline(1:1)
        c2=cline(2:2)
        ifound=0
        do i=1,len_trim(cline)
          if (cline(i:i).eq.'!') then
            exit
          else if (cline(i:i).eq.'=') then
            ifound = 1
            exit
          endif
        enddo
        if (c1.eq.'p'.and.ifound.eq.0) then
          cycle
        else if (c1.eq.'p'.and.c2.eq.' '.and.ifound.eq.1) then
          if (ifirstvar.eq.0) ifirstvar=nclc
          if (nhead.eq.0) nhead=nclc-1
          npar=npar+1
          cpar(npar)=cline
        else if (c1.eq.'$'.and.ifound.eq.1) then
          if (ifirstvar.eq.0) ifirstvar=nclc
          if (nhead.eq.0) nhead=nclc-1
          nvar=nvar+1
          cvar(nvar)=cline
        else if (ifirstvar.eq.0) then
          nhead=nhead+1
          chead(nhead)=cline
        else
          nbuff=nbuff+1
          cbuff(nbuff)=cline
        endif
      enddo

91    close(luncld)

      open(newunit=lunin,file='undumag.cld')

      do i=1,nhead
        write(lunin,'(a)') trim(chead(i))
      enddo

      do i=1,npar
        write(lunin,'(a)') trim(cpar(i))
      enddo

      do i=1,nvar
        write(lunin,'(a)') trim(cvar(i))
      enddo

      do i=1,nbuff
        if (len_trim(cbuff(i)).gt.0) write(lunin,'(a)') trim(cbuff(i))
      enddo

      flush(lunin)
      rewind(lunin)

      npar=0
      nbuff=0
      linepa=0
      nvar=0
      ntetmesh=0
      kconvhull=0
      lineva=0
      lineve=0

      do i=1,nparp
        cbuff(i)=""
      enddo

      icount=0
 1    continue
      icount=icount+1
      read(lunin,'(a)',end=9)cline
      if (kecho.ne.0) then
        write(lun6,*)icount
        write(lun6,*)trim(cline)
      endif
      if (cline(1:1).eq.'$') then
        ifound=0
        do i=1,512
          if (cline(i:i).eq.'!') then
            ifound=i
            exit
          endif
        enddo
        if (ifound.ne.0) then
          cline=cline(1:ifound-1)
        endif
      endif
      nbuff=nbuff+1
      if (cline(1:1).eq.'{') then
        kcomm=kcomm+1
      else if (cline(1:1).eq.'}') then
        kcomm=kcomm-1
      endif
      if (kcomm.lt.0) then
        write(lun6,*)"*** Error in undumag_calc: Missing '{' before line ",nbuff
        stop
      endif
      call util_string_trim(cline,nfirst,nlast)
      lenc=nlast-nfirst+1
      cbuff(nbuff)=''
      if (nlast.gt.0) cbuff(nbuff)(nfirst:nlast)=cline(nfirst:nlast)
      call util_string_substring(cline,'File',ianf,iend,istat)
      if (istat.eq.0) then
        nplan=-9
      else
        read(cline,*,end=39,err=39) nplan
      endif
      if (kcomm.eq.0.and.nplan.eq.-9) then
        ntetmesh=ntetmesh+1
        kconvhull(ntetmesh)=nbuff
      endif
 39   continue
      if (ntetmesh.gt.0) then
        if (nbuff.eq.kconvhull(ntetmesh)+1) then
          cline=cline(nfirst:nlast)//'.dat'
          nlast=nlast+4
          cfiletet(ntetmesh)=cline(nfirst:nlast)
        endif
      else if (cline(1:1).eq.'p') then
        if (linepa.eq.0) linepa=nbuff
        do i=1,lenc
          if (cline(i:i).eq.'=') then
            npar=npar+1
            cpar(npar)=cline(3:i-1)
            read(cline(i+1:lenc),*) par(npar)
            goto 1
          endif
        enddo
      else if (cline(1:14).eq.'*EndParameters') then
        linepe=nbuff
      else if (cline(1:1).eq.'$') then
        do i=1,lenc
          if (cline(i:i).eq.'=') then
            nvar=nvar+1
          endif
        enddo
        if (nvar.eq.1) linepe=nbuff-1
        if (lineva.eq.0) lineva=nbuff
      else if (cline(1:8).eq.'*EndCalc') then
        lineve=nbuff-1
      endif

      goto 1

9     flush(lunin)
      close(lunin)

      if (kcomm.ne.0) then
        write(lun6,*)"*** Error in undumag_calc: Unbalanced curled brackets ***"
        stop
      endif

      if (lineve.eq.0.and.nvar.gt.0) then
        write(lun6,*)"*** Error in undumag_calc: No *EndCalc marker found ***"
        stop
      endif

      if (lineva.eq.0) lineva=nbuff

      do ibuff=lineva,lineve

        cline=cbuff(ibuff)

        call util_string_trim(cline,nfirst,nlast)

        lenc=nlast-nfirst+1

        if (kecho.ne.0.and.nlast.gt.0) write(lun6,*)ibuff,': ',cline(nfirst:nlast)

        if (cline(1:1).eq.'{') then
          kcomm=kcomm+1
        else if (cline(1:1).eq.'}') then
          kcomm=kcomm-1
        endif

        if (kcomm.lt.0) then
          write(lun6,*)"*** Error in undumag_calc: Missing '{' before line ",ibuff
          stop
        endif

        if (kcomm.gt.0.or.cline(1:1).ne.'$') cycle

        if (mode.lt.0) then
          do i=1,lenc
            if (cline(i:i).eq.'[') then
              cline(i:i)='('
            else if (cline(i:i).eq.']') then
              cline(i:i)=')'
            endif
          enddo
        endif

        ! sort cpar by length
        call undumag_sort_cpar(nparp,npar,cpar,par)
        call undumag_calc_cline(cline,nparp,npar,par,cpar)

      enddo !ibuff

      call undumag_sort_cpar(nparp,npar,cpar,par)
      open(newunit=lunout,file='undumag.in')

      do ibuff=1,linepa-1
        call util_string_trim(cbuff(ibuff),nfirst,nlast)
        lenc=nlast-nfirst+1
        if (lenc.gt.0) write(lunout,'(a)') cbuff(ibuff)(nfirst:nlast)
      enddo

      write(lunout,'(a)')"* Results of undumag_calc"

      do ipar=1,npar
        if (nint(par(ipar)).eq.par(ipar)) then
          write(cword,*)nint(par(ipar))
        else
          write(cword,*)par(ipar)
        endif
        write(cline,*)'*',trim(cpar(ipar)),": ",trim(cword)
        call util_string_trim(cline,nfirst,nlast)
        lenc=nlast-nfirst+1
        write(lunout,'(a)')cline(nfirst:nlast)
      enddo

      ntetmesh=ntetmesh+1
      itet=0

49    continue

      ntetmesh=ntetmesh-1
      mtetmesh=1

      if (kcomm.ne.0) then
        write(lun6,*)"*** Error in undumag_calc: Unbalanced curled brackets ***"
        stop
      endif


      do ibuff=lineve+1,nbuff

        cline=''
        call util_string_trim(cbuff(ibuff),nfirst,nlast)
        cline=cbuff(ibuff)(nfirst:nlast)

        if (itet.eq.0.and.ntetmesh.gt.0) then
          if (ibuff.eq.kconvhull(mtetmesh)+1) then
            cline=cfiletet(mtetmesh)
            mtetmesh=mtetmesh+1
            call util_string_trim(cline,nfirst,nlast)
          endif
        endif

c        write(lun6,*)ibuff,cline(nfirst:nlast)

19      continue

        if (cline(1:1).eq.'{') then
          kcomm=kcomm+1
        else if (cline(1:1).eq.'}') then
          kcomm=kcomm-1
        endif

        if (kcomm.lt.0) then
          write(lun6,*)"*** Error in undumag_calc: Missing '{' before line ",ibuff
          if (itet.ne.0) write(lun6,*)"in file: ",trim(cfiletet(mtetmesh-1))
          stop
        endif

        if (kcomm.gt.0.or.cline(1:1).eq.'*') then
          write(lunout,'(a)') cline(nfirst:nlast)
          cycle
        endif

        k=index(cline,"!")
        call util_string_trim(cline,nfirst,nlast)
        lenck=nlast-nfirst+1
        lenc=lenck
        if (k.ne.0) then
          lenck=k-1
        endif

        ifound=0

        do i=1,lenck

          if (cline(i:i).eq.'$') then
            do ipar=1,npar

              k=index(cline,"$"//trim(cpar(ipar)))

              if (k.ne.0) then

                k=k+1
                ifound=1

                cpart1=cline(1:k-2)
                call util_string_trim(cpart1,nfirst1,nlast1)
                cpart1=cpart1(nfirst1:nlast1)
                nlast1=nlast1-nfirst1+1
                nfirst1=1

                lencc=len_trim(cpar(ipar))
                ktest=k+lencc
                if (ktest.gt.0.and.cline(ktest:ktest).ne.' ') then
                  print*,"*** Error in undumag_calc: Can not resolve:"
                  print*,trim(cline)
                  write(lun6,*)"*** Program UNDUMAG aborted ***"
                  stop
                endif
                cpart2=cline(k+1+lencc:lenc)
                call util_string_trim(cpart2,nfirst2,nlast2)
                cpart2=cpart2(nfirst2:nlast2)
                nlast2=nlast2-nfirst2+1
                nfirst2=1

                if (nint(par(ipar)).eq.par(ipar)) then
                  write(cword,*)nint(par(ipar))
                else
                  write(cword,*)par(ipar)
                endif

                call util_string_trim(cword,nfirstw,nlastw)
                cword=cword(nfirstw:nlastw)
                nlastw=nlastw-nfirstw+1
                nfirstw=1

                cline=''
                if (nlast1.gt.0) cline=cpart1(nfirst1:nlast1)
                call util_string_trim(cline,nfirst,nlast)
                if (nlastw.gt.0) cline=cline(nfirst:nlast)//' '//
     &            cword(nfirstw:nlastw)
                call util_string_trim(cline,nfirst,nlast)
                if (nlast2.gt.0) cline=cline(nfirst:nlast)//' '//
     &            cpart2(nfirst2:nlast2)

                goto 19
              endif
            enddo
          endif
        enddo

        if (ifound.ne.0) goto 19

        call util_string_trim(cline,nfirst,nlast)
        lenc=nlast-nfirst+1

        if (lenc.gt.0) then
          if(index(cline(1:lenck),"$").ne.0) then
            write(lun6,*)"*** Error in undumag_calc: Could not resolve line:"
            write(lun6,*)trim(cline)
            stop
          endif
          write(lunout,'(a)') cline(nfirst:nlast)
        endif

      enddo

      flush(lunout)
      close(lunout)

      if (ntetmesh.gt.0) then

        lineve=0
        nbuff=0
        itet=1

        call util_string_trim(cfiletet(ntetmesh),nfirst,nlast)
        open(newunit=lunin,file=cfiletet(ntetmesh)(nfirst:nlast-4),
     &    status='old')
        open(newunit=lunout,file=cfiletet(ntetmesh)(nfirst:nlast))
41      call util_skip_comment_end(lunin,ieof)
        if (ieof.eq.0) then
          nbuff=nbuff+1
          read(lunin,'(a)')cbuff(nbuff)
          goto 41
        endif

        flush(lunin)
        close(lunin)

        if (ntetmesh.gt.0) goto 49

      endif

      return
      end
