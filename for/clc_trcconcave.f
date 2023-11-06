*CMZ :  2.05/02 03/11/2023  15.48.42  by  Michael Scheer
*-- Author :    Michael Scheer   26/10/2023
      subroutine clc_trcconcave

      use magnets_structure

      implicit none

      Type(T_concave) tcon

      double precision :: t8(8)

      integer :: icon,itr,ipos(2,4),nwords,istat,k,ln,lnt,lmt,lc1,lc2,
     &  itrunk,ntrunk,nt,idebug=0,ish

      character(512) cnam,cnamc,cmotht,cnamt,cline,ctrans
      character(32) c32

      if (idebug.ne.0) call util_break

      nt=ntransrotcop
      do itr=1,nt

        t8=transrotcop(8,itr)

        if (t8(8).ge.0.0d0) then
          cycle
        endif ! not copy

c        ic=-int(t8(8))

        ctrans=ctransrotcop(itr)
        cline=clccop(itr)

        call  util_string_split(cline,4,nwords,ipos,istat)

        cnamc = cline(ipos(1,1):ipos(2,1))
        lc2=ipos(2,1)-ipos(1,1)+1

        nwords=min(3,nwords)

        if(nwords.eq.3) then
          lc1=ipos(1,3)
          if (
     &      cline(lc1:lc1).eq.'!'.or.
     &      cline(lc1:lc1).eq.'*')
     &      nwords=2
        endif

        if (nwords.ne.3) cycle

        cnamt=cline(ipos(1,2):ipos(2,2))
        cmotht=cline(ipos(1,3):ipos(2,3))

        call util_string_trim(cnamt,k,lnt)
        call util_string_trim(cmotht,k,lmt)

        do icon=1,nconcave_t

          tcon=t_concaves(icon)

          ntrunk=0

          cnam=tcon%tmag%cnam
          call util_string_trim(cnam,k,ln)

          do while (cnam(ln:ln).eq.'&')
            cnam(ln:ln)=''
            call util_string_trim(cnam,k,ln)
            cnam=cnam(1:ln)
            ntrunk=ntrunk+1
          enddo

          if (cnam(1:ln).eq.cnamc(1:lc2)) then

            c32=''
            do itrunk=1,ntrunk

              c32(itrunk:itrunk)='&'

              ntransrotcop=ntransrotcop+1

              if (ntransrotcop.gt.ntransrotcop_p) then
                stop "*** Error in clc_ctrcconcave: Too many copies, check parameter ntransrotcop_p ***"
              endif

              call util_string_trim(tcon%tmag%cnam,k,ln)

              nclccop_t=nclccop_t+1

              clccop(nclccop_t)=
     &          tcon%tmag%cnam(1:ln) // ' ' //
     &          cnamt(1:lnt) // c32(1:itrunk) // ' ' //
     &          cmotht(1:lmt)

              t8(8)=-icon

              ish=ntransrotcop
              do while (ish.gt.itr+1)
                transrotcop(:,ish)=transrotcop(:,ish-1)
                ctransrotcop(ish)=ctransrotcop(ish-1)
                ish=ish-1
              enddo

              transrotcop(:,itr+1)=t8
              ctransrotcop(itr+1)=tcon%tmag%cnam(1:ln)

            enddo !ntrunk

          endif

        enddo !icon

      enddo !itr

      return
      end
