*CMZ :  2.04/00 13/01/2023  12.47.26  by  Michael Scheer
*CMZ :  2.03/00 01/08/2022  08.31.28  by  Michael Scheer
*CMZ :  2.02/02 29/06/2022  16.19.02  by  Michael Scheer
*CMZ :  2.02/01 17/04/2021  09.10.36  by  Michael Scheer
*CMZ :  2.00/03 20/04/2018  11.43.00  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  14.13.11  by  Michael Scheer
*CMZ :  1.10/00 09/11/2016  13.57.10  by  Michael Scheer
*-- Author :    Michael Scheer   13/10/2016
      subroutine  util_mshcalc(clinein,res)

      implicit none

      integer ndimp,nfunp
      parameter (ndimp=1000,nfunp=20)

      double precision res,xopy(ndimp)
      double precision :: pi=3.141592653589793d0

      integer ndim,istat,kfuns(nfunp)
      integer lenc,jc,ic1,ic,kbo,kbc,iw,nfirst,nlast,ianf,iend,kfun
      integer :: nres=0,ifun=0,ical=0,lfun=0,loop=0

      character(*) clinein
      character(2048) cline
      character(4096) cbrack,cwork
      character(32) c32,cfun(nfunp)
      character c1

      equivalence(c1,ic1)

      save ical

      ic1=0

      if (ical.eq.0) then
        cfun(1)='asind'
        cfun(2)='acosd'
        cfun(3)='atand'
        cfun(4)='acotd'
        cfun(5)='asin'
        cfun(6)='acos'
        cfun(7)='atan'
        cfun(8)='acot'
        cfun(9)='sind'
        cfun(10)='cosd'
        cfun(11)='tand'
        cfun(12)='cotd'
        cfun(13)='sin'
        cfun(14)='cos'
        cfun(15)='tan'
        cfun(16)='cot'
        cfun(17)='nint'
        cfun(18)='abs'
        cfun(19)='sqrt'
        cfun(20)='int'
      endif
      ical=ical+1
      !print*,"ical:",ical

      ndim=ndimp

      call util_string_trim(clinein,nfirst,nlast)
      cline=''
      cline=clinein(nfirst:nlast)

      loop=0


      call util_string_trim(cline,nfirst,nlast)
      lenc=nlast-nfirst+1

      do while(lenc.gt.0)

        loop=loop+1
        !print*,trim(cline)

        cwork=''
        lenc=nlast-nfirst+1
        cwork=cline(nfirst:nlast)

        cline=''
        iw=0
        ic=0

        do jc=1,lenc
          ic=ic+1
          c1=cwork(ic:ic)
          if (c1.ne.' ') then
            iw=iw+1
            cline(iw:iw)=c1
          endif
        enddo

        call util_string_trim(cline,nfirst,nlast)
        lenc=nlast-nfirst+1

        kfun=0

        do ifun=1,nfunp
          ianf=1
          iend=lenc
          call util_string_substring(cline,trim(cfun(ifun)),ianf,iend,istat)
          if (istat.eq.0) then
            kfun=ifun
            kfuns(ifun)=iend
            exit
          endif
        enddo !all known functions

        kbo=0
        kbc=0

        call util_string_trim(cline,nfirst,nlast)
        lenc=nlast-nfirst+1

        if (kfun.gt.0) then

          do ic=kfuns(kfun)+1,nlast
            c1=cline(ic:ic)
            if (c1.eq.'[') then
              kbo=ic+1
            else if (c1.eq.']') then
              kbc=ic-1
              exit
            endif
          enddo !lenc

          if (kbo.gt.0.and.kfun.eq.0) then
            print*,"*** Error in util_mshcalc: Unknown function in line"
            call util_string_trim(cline,nfirst,nlast)
            print*,cline(nfirst:nlast)
          endif

          if (kbo.gt.0.and.kbc.lt.kbo) then
            print*,"*** Error in util_mshcalc: Missing bracket in line"
            call util_string_trim(cline,nfirst,nlast)
            print*,cline(nfirst:nlast)
            stop
          endif

        endif !kfun

        if (kbo.gt.0) then
          cbrack=cline(kbo:kbc)
        else
          cbrack=cline(nfirst:nlast)
        endif

        !print*,trim(cbrack)
        call util_mshcalc_basics(cbrack,xopy,res)
        nres=nres+1
        xopy(nres)=res
        !print*,nres,xopy(nres)

        if (kbo.eq.0.or.kfun.eq.0) goto 9999

        write(c32,*)nres
        call  util_string_trim(c32,nfirst,nlast)

        if (kfun.eq.1) then
          xopy(nres)=asin(res)/pi*180.0d0
          cline(kbo-6:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.2) then
          xopy(nres)=acos(res)/pi*180.0d0
          cline(kbo-6:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.3) then
          xopy(nres)=atan(res)/pi*180.0d0
          cline(kbo-6:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.4) then
          xopy(nres)=atan(1.0d0/res)/pi*180.0d0
          cline(kbo-6:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.5) then
          xopy(nres)=asin(res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.6) then
          xopy(nres)=acos(res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.7) then
          xopy(nres)=atan(res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.8) then
          xopy(nres)=atan(1.0d0/res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.9) then
          xopy(nres)=sin(res*pi/180.0d0)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.10) then
          xopy(nres)=cos(res*pi/180.0d0)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.11) then
          xopy(nres)=tan(res*pi/180.0d0)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.12) then
          xopy(nres)=1.0d0/tan(res*pi/180.0d0)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.13) then
          xopy(nres)=sin(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.14) then
          xopy(nres)=cos(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.15) then
          xopy(nres)=tan(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.16) then
          xopy(nres)=1.0d0/tan(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.17) then
          xopy(nres)=nint(res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.18) then
          xopy(nres)=abs(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.19) then
          xopy(nres)=sqrt(res)
          cline(kbo-5:kbc+1)='x'//c32(nfirst:nlast)
        else if (kfun.eq.20) then
          xopy(nres)=int(res)
          cline(kbo-4:kbc+1)='x'//c32(nfirst:nlast)
        endif

c      call util_string_trim(cline,nfirst,nlast)
c      print*,cline(nfirst:nlast)

        call util_string_trim(cline,nfirst,nlast)
        lenc=nlast-nfirst+1

      enddo

9999  continue

      res=xopy(nres)

      return
      end
