*CMZ :  2.04/00 14/01/2023  11.37.07  by  Michael Scheer
*CMZ :  2.02/01 26/04/2021  14.38.34  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.17/08 30/05/2017  14.55.39  by  Michael Scheer
*CMZ :  1.10/00 09/11/2016  14.18.47  by  Michael Scheer
*CMZ :  1.02/00 22/08/2016  09.11.46  by  Michael Scheer
*CMZ :  0.00/10 13/07/2016  09.36.23  by  Michael Scheer
*CMZ :  0.00/09 30/06/2016  15.12.31  by  Michael Scheer
*CMZ :  0.00/05 10/06/2016  15.58.48  by  Michael Scheer
*CMZ :  0.00/04 11/05/2016  11.22.46  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2016
      subroutine undumag_calc_cline(cline,nparp,npar,par,cpar)

      use commandlinef90m

      implicit none

      double precision par(nparp),result
      integer :: ical=0,idebug=0
      integer :: kpar,kfound

      integer nparp,npar,nfirst,nlast,k,lenc,lencc,ifound,ipar,i,keq,ieq,
     &  nfirst1,nlast1,
     &  nfirstw,nlastw,
     &  nfirst2,nlast2

      character(128) cpar(nparp),cword
      character(512) cline,cpart1,cpart2

      if (idebug.gt.0) then
        ical=ical+1
        print*,ical,trim(cline)
      endif

      cpart1=cline
      call util_string_trim(cline,nfirst,nlast)
      lenc=nlast-nfirst+1

      cpart1=cline(nfirst:nlast)
      cline=''
      cline=cpart1(1:nlast-nfirst+1)

      ieq=0
      do ieq=1,lenc
        if (cline(ieq:ieq).eq.'=') then
          keq=ieq
          exit
        endif
      enddo

7     ifound=0
      call util_string_trim(cline,nfirst,nlast)

      lenc=nlast-nfirst+1

      do i=max(keq+1,nfirst),nlast

        if (cline(i:i).eq.'$') then

          kfound=0
          do kpar=1,npar
            k=index(cline(keq+1:len_trim(cline)),"$"//trim(cpar(kpar)))
            if (kfound.ne.0) then
              if (cpar(kpar).ne.cpar(kfound)) cycle
            endif
            if (k.gt.0) then
              if (kfound.eq.0) then
                kfound=kpar
              endif
              ipar=kpar
              !if (idebug.gt.0) exit
            endif
          enddo

          !do ipar=1,npar

          if (ipar.gt.0) then
            k=index(cline(keq+1:len_trim(cline)),"$"//trim(cpar(ipar)))
          else
            k=0
          endif
c            write(lun6,*)ipar,trim(cpar(ipar))

          if (k.gt.0) then

            k=keq+k+1
            ifound=1

            cpart1=cline(1:k-2)
            call util_string_trim(cpart1,nfirst1,nlast1)
            cpart1=cpart1(nfirst1:nlast1)
            nlast1=nlast1-nfirst1+1
            nfirst1=1

            lencc=len_trim(cpar(ipar))
            cpart2=cline(k+lencc:lenc)
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
     &        cword(nfirstw:nlastw)
            call util_string_trim(cline,nfirst,nlast)
            if (nlast2.gt.0) cline=cline(nfirst:nlast)//' '//
     &        cpart2(nfirst2:nlast2)
            goto 7
          endif ! Substitution

          !enddo !npar

        endif ! Variable found

      enddo ! cline(i:i)

      if (ifound.ne.0) goto 7

      if(index(cline(keq+1:lenc),"$").ne.0) then
        write(lun6,*)"*** Error in undumag_calc_cline: Could not resolve line"
        write(lun6,*)trim(cline)
        stop
      endif

      call util_string_trim(cline,nfirst,nlast)
      lenc=nlast-nfirst+1

      if (ifound.ne.0) then
        call util_mshcalc(cline,result)
        goto 7
      else !ifound
        call util_mshcalc(cline(keq+1:lenc),result)
      endif !ifound

      npar=npar+1
      cpar(npar)=cline(2:keq-1)
      par(npar)=result

      return
      end
