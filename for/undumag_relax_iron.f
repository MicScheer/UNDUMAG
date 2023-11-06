*CMZ :  2.04/06 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  20.27.45  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.01/03 30/04/2019  13.33.26  by  Michael Scheer
*CMZ :  2.00/03 24/04/2018  12.16.06  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  08.58.25  by  Michael Scheer
*CMZ :  1.23/03 22/09/2017  17.51.41  by  Michael Scheer
*CMZ :  1.23/02 31/08/2017  12.21.44  by  Michael Scheer
*CMZ :  1.19/00 19/06/2017  12.56.59  by  Michael Scheer
*CMZ :  1.17/08 29/05/2017  16.45.03  by  Michael Scheer
*CMZ :  1.17/07 22/05/2017  09.09.02  by  Michael Scheer
*CMZ :  1.17/06 21/05/2017  12.27.21  by  Michael Scheer
*CMZ :  1.17/05 18/05/2017  09.23.26  by  Michael Scheer
*CMZ :  1.17/04 09/05/2017  14.57.54  by  Michael Scheer
*CMZ :  1.17/03 09/05/2017  13.13.21  by  Michael Scheer
*CMZ :  1.17/02 09/05/2017  13.07.06  by  Michael Scheer
*CMZ :  1.17/01 08/05/2017  20.49.50  by  Michael Scheer
*CMZ :  1.17/00 08/05/2017  16.36.43  by  Michael Scheer
*CMZ :  1.16/00 07/05/2017  13.33.40  by  Michael Scheer
*CMZ :  1.15/12 05/05/2017  10.18.32  by  Michael Scheer
*CMZ :  1.15/11 03/05/2017  09.18.32  by  Michael Scheer
*CMZ :  1.15/10 19/04/2017  12.30.22  by  Michael Scheer
*CMZ :  1.14/00 21/03/2017  16.50.20  by  Michael Scheer
*CMZ :  1.13/02 09/03/2017  16.18.05  by  Michael Scheer
*CMZ :  1.11/04 24/01/2017  15.44.29  by  Michael Scheer
*CMZ :  1.11/03 17/01/2017  15.24.09  by  Michael Scheer
*CMZ :  1.10/02 28/11/2016  16.36.32  by  Michael Scheer
*CMZ :  1.07/00 22/09/2016  11.21.55  by  Michael Scheer
*CMZ :  1.06/00 21/09/2016  12.04.03  by  Michael Scheer
*CMZ :  1.05/00 16/09/2016  12.00.51  by  Michael Scheer
*CMZ :  1.04/02 15/09/2016  15.11.30  by  Michael Scheer
*CMZ :  1.04/01 14/09/2016  14.42.07  by  Michael Scheer
*CMZ :  1.02/01 04/09/2016  16.30.16  by  Michael Scheer
*CMZ :  1.02/00 30/08/2016  12.06.55  by  Michael Scheer
*CMZ :  1.01/00 20/08/2016  20.03.15  by  Michael Scheer
*CMZ :  0.00/13 18/08/2016  09.26.09  by  Michael Scheer
*CMZ :  0.00/12 21/07/2016  09.19.24  by  Michael Scheer
*CMZ :  0.00/11 20/07/2016  16.26.19  by  Michael Scheer
*CMZ :  0.00/10 14/07/2016  12.10.17  by  Michael Scheer
*CMZ :  0.00/09 06/07/2016  08.44.44  by  Michael Scheer
*CMZ :  0.00/02 02/05/2016  08.14.51  by  Michael Scheer
*CMZ :  1.17/11 05/04/2016  15.04.02  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  19.29.14  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  12.58.26  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  10.50.55  by  Michael Scheer
*CMZ :  1.17/04 24/03/2016  18.47.26  by  Michael Scheer
*CMZ :  1.17/03 22/03/2016  09.00.36  by  Michael Scheer
*CMZ :  1.17/02 14/03/2016  18.22.42  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2016
      subroutine undumag_relax_iron

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m

      implicit none

      real, dimension (:), allocatable :: rshuffle
      integer, dimension (:), allocatable :: ishuffle,iwarn

      double precision bc(3),x,y,z,bx,by,bz,dh3(3),h3(3),b,b3(3),h3e(3),he,
     &  h3t(3),ht,bc3(3),f3(3),
     &  hx,hy,hz,bpebc1(9),bn,
     &  dmag1,dmag2,dmag,dbc(3),bco(3),h,hcold,hold,hmin,hmax,femag,
     &  htest,hxtest,hytest,hztest,h1,vmaglab(3),dhmin5,dh,dho,
     &  fesat,htesto,chi,bcvox(3),ftol,dhth,dhtho,dum1,dum2,
     &  dumv1(3),dumv2(3),fm0,fm1,h0,ddhth,chio

      double precision vnor,vx,vy,vz

*KEEP,bcbuff.
      include 'bcbuff.cmn'
*KEND.

      integer kfail,ifail,imoth,lmag,kmag,imag,iter,k,i,iron1,iron2,
     &  iplan,ih,il,
     &  nhz,mat,mapmode,ichi,kdump8,i1,i2

      save hold,ishuffle,rshuffle,iwarn

      vnor(vx,vy,vz)=sqrt(vx**2+vy**2+vz**2) !inline code

      iron1=nrec+1
      iron2=nmag

      if (ncwires.eq.0.and.nrec.eq.0) then
        do kmag=iron1,iron2
          bpebc(4:7,kmag)=0.0d0
        enddo
        return
      endif

      kconviron=0
      h=0.0d0
      kdump8=0

      if (kiter.eq.1) then

        allocate (ishuffle(nmag))
        allocate (rshuffle(nmag))
        allocate (iwarn(nmag))

        iwarn=0
        lmag=0

        do kmag=iron1,iron2
          lmag=lmag+1
          ishuffle(lmag)=kmag
        enddo


        write(lun6,*)
        write(lun6,*)"Max. number iterations in undumag_relax_iron, i.e. maxiterion:",maxiteriron
        write(lun6,*)

        lmag=0

        magmag=0
        hold=0.0d0

      endif !(kiter.eq.1) then

      if (kshuffle.ne.0) then
        call util_random(niron,rshuffle)
        do i=1,niron
          k=niron-i+1
          kmag=rshuffle(i)*k+1.0
          lmag=ishuffle(kmag)
          ishuffle(kmag)=ishuffle(k)
          ishuffle(k)=lmag
        enddo
      endif !kshuffle

      hxtest=0.0d0
      hytest=0.0d0
      hztest=0.0d0
      htest=0.0d0
      kinside=-1

      if (kdumpconv.ne.0) then
        lmag=0
        do kmag=iron1,iron2
          lmag=lmag+1
          imag=ishuffle(lmag)
          x=bpebc(1,imag)/1000.0d0
          y=bpebc(2,imag)/1000.0d0
          z=bpebc(3,imag)/1000.0d0
          imoth=nint(bpebc(15,imag))
          call undumag_field(x,y,z,hx,hy,hz,ifail)
          write(lunconv,*)kiter,iterirontot,imoth,imag,
     &      x*1000.0d0,y*1000.0d0,z*1000.0d0,
     &        hx,hy,hz,sqrt(hx**2+hy**2+hz**2)
     &      ,hxtest,hytest,hztest,htest,
     &      bpebc(4:6,imag),
     &      sqrt(bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
        enddo
      endif

      hcold=0.0d0

      do iter=1,maxiteriron

        iterirontot=iterirontot+1

        bc0(4:6,iron1:iron2)=bpebc(4:6,iron1:iron2)

        kinside=-1
        lmag=0

        do kmag=iron1,iron2

          lmag=lmag+1
          imag=ishuffle(lmag)

          mat=nint(bpebc(9,imag))
          mapmode=matmaps(3,mat)
          bco=bc0(4:6,imag)

          if (mapmode.eq.0) then ! mu is infinite until saturation is reached

            if (matrix.eq.0) then
              write(lun6,*)"*** Error in undumag_relax_iron: Mapmode=0 requires matrix mode ***"
              stop
            else
              call undumag_bpolyeder_matrix(imag,h3t(1),h3t(2),h3t(3),ifail)
              h3=bc00(1:3,imag)*bco
            endif

            h3e=h3t-h3
            he=sqrt(h3e(1)**2+h3e(2)**2+h3e(3)**2)

            bc3=-h3e/bc00(1:3,imag)
            bn=vnor(bc3(1),bc3(2),bc3(3))
            femag=bn

            fesat=bcmat(2,1,mat)

            if (bn.gt.fesat) then
              bc3=bc3/femag*fesat
            endif

            dbc=bc3-bco
            bc0(4:6,imag)=bco+dbc*dampiron

            if (ibulk.eq.0) then
              ! For this mode, magnetization is immediatly updated
              bpebc(4:6,imag)=bc0(4:6,imag)
              vmaglab(1:3)=bpebc(4:6,imag)
              do iplan=1,iabs(ibpeplan(imag))
                bpetm(1,7,iplan,imag)=
     &            vmaglab(1)*bpetm(1,8,iplan,imag)+
     &            vmaglab(2)*bpetm(2,8,iplan,imag)+
     &            vmaglab(3)*bpetm(3,8,iplan,imag)
              enddo
            endif !ibulk

          else if (mapmode.eq.2) then

            nhz=matmaps(4,mat)
            fesat=bcmat(2,nhz,mat)

            if (kiter.eq.1.and.iter.eq.1.and.isplinefm.ne.0) then
              call util_spline_coef(feh1,fem1,nhz,0.0d0,0.0d0,
     &          fespl1,fewspl1,fewspl2,fewspl3,fewspl4)
              call util_spline_coef(fem1,feh1,nhz,0.0d0,0.0d0,
     &          ufespl1,fewspl1,fewspl2,fewspl3,fewspl4)
            endif

            if (matrix.eq.0) then
              write(lun6,*)"*** Error in undumag_relax_iron: Mapmode=2 requires matrix mode ***"
              stop
            else
              call undumag_bpolyeder_matrix(imag,h3t(1),h3t(2),h3t(3),ifail)
              if (ifail.ne.0) then
                write(lun6,*)"*** Error in undumag_relax_iron: Bad return from undumag_bpolyeder_matrix for magnet ",imag," ***"
              endif
              h3=bc00(1:3,imag)*bco
            endif

            ht=sqrt(h3t(1)**2+h3t(2)**2+h3t(3)**2)
            h3e=h3t-h3
            he=sqrt(h3e(1)**2+h3e(2)**2+h3e(3)**2)
            h3=h3t
            h=ht

            if (nhz.eq.3) then
              ifail=0
              if (h.lt.feh1(2)) then
                femag=fem1(1)+(fem1(2)-fem1(1))/(feh1(2)-feh1(1))*(h-feh1(1))
              else
                femag=fem1(2)+(fem1(3)-fem1(2))/(feh1(3)-feh1(2))*(h-feh1(2))
              endif
            else
              hmin=bcmat(1,1,mat)
              hmax=bcmat(1,nhz,mat)
              femag=fesat
              ifail=-1
              if (h.ge.hmin.and.h.le.hmax) then
                !non-equidistant linear interpolation
                call util_interpol_linear(nhz,feh1,fem1,h,femag,ifail)
              endif
              if (ifail.ne.0) then
                write(lun6,*)"*** Error in undumag_relax_iron: H is out of range for material or bad interpolation",mat
                write(lun6,*)"iter, mag, Hmin, Hmax, H:", iter,imag, hmin,hmax,h
                stop
              endif
            endif

            chio=0.0d0
            chi=1.0d0

            ifail=-1

            do ichi=1,nchiiron
              chio=chi
              chi=femag/h
              if(abs(chio/chi-1.0d0).lt.chicut) then
                exit
              endif
              h3(1:3)=h3e(1:3)/(1.0d0-bc00(1:3,imag)*chi)
              h=sqrt(h3(1)**2+h3(2)**2+h3(3)**2)
              if (nhz.eq.3) then
                ifail=0
                if (h.lt.feh1(2)) then
                  femag=fem1(1)+(fem1(2)-fem1(1))/(feh1(2)-feh1(1))*(h-feh1(1))
                else
                  femag=fem1(2)+(fem1(3)-fem1(2))/(feh1(3)-feh1(2))*(h-feh1(2))
                endif
              else
                hmin=bcmat(1,1,mat)
                hmax=bcmat(1,nhz,mat)
                femag=fesat
                ifail=-1
                if (h.ge.hmin.and.h.le.hmax) then
                  !non-equidistant linear interpolation
                  if (isplinefm.eq.0) then
                    call util_interpol_linear(nhz,feh1,fem1,h,femag,kfail)
                    if (kfail.ne.0) then
                      write(lun6,*)"*** Error in undumag_relax_iron: H is out of range for material or bad interpolation",mat
                      write(lun6,*)"mag, Hmin, Hmax, H:", imag, hmin,hmax,h
                      stop
                    endif
                  else
                    call util_spline_inter(feh1,fem1,fespl1,nhz,h,femag,0)
                  endif
                endif
              endif
              bcvox=h3/h*femag
            enddo !ichi

            dbc=bcvox-bco
            bc0(4:6,imag)=bco+dbc*dampiron

            if (ichi.gt.nchiiron) ichi=nchiiron
            if (ichi.gt.nchimax) nchimax=ichi

            if (ibulk.eq.0) then
              ! For this mode, magnetization is immediatly updated
              bpebc(4:6,imag)=bc0(4:6,imag)
              vmaglab(1:3)=bpebc(4:6,imag)
              do iplan=1,iabs(ibpeplan(imag))
                bpetm(1,7,iplan,imag)=
     &            vmaglab(1)*bpetm(1,8,iplan,imag)+
     &            vmaglab(2)*bpetm(2,8,iplan,imag)+
     &            vmaglab(3)*bpetm(3,8,iplan,imag)
              enddo
            endif !ibulk

          else if (mapmode.eq.3) then

            nhz=matmaps(4,mat)
            fesat=bcmat(2,nhz,mat)

            if (kiter.eq.1.and.iter.eq.1.and.isplinefm.ne.0) then
              call util_spline_coef(feh1,fem1,nhz,0.0d0,0.0d0,
     &          fespl1,fewspl1,fewspl2,fewspl3,fewspl4)
              call util_spline_coef(fem1,feh1,nhz,0.0d0,0.0d0,
     &          ufespl1,fewspl1,fewspl2,fewspl3,fewspl4)
            endif

            if (matrix.eq.0) then
              write(lun6,*)"*** Error in undumag_relax_iron: Mapmode=3 requires matrix mode ***"
              stop
            else
              call undumag_bpolyeder_matrix(imag,h3t(1),h3t(2),h3t(3),ifail)
              if (ifail.ne.0) then
                write(lun6,*)"*** Error in undumag_relax_iron: Bad return from undumag_bpolyeder_matrix for magnet ",imag," ***"
              endif
              h3=bc00(1:3,imag)*bco
            endif

            h3e=h3t-h3
            he=sqrt(h3e(1)**2+h3e(2)**2+h3e(3)**2)
            h3=h3e
            h=he

            ifail=1

            i1=1
            i2=nhz

            h=feh1(i1)
            femag=fem1(i1)

            bc3=h3e/(h/femag-bc00(1:3,imag))
            h3t=h3e+bc00(1:3,imag)*bc3
            ht=vnor(h3t(1),h3t(2),h3t(3))

            dhth=ht-h

            hhbuff(1)=dhth
            bcbuff(1)=femag

            h=feh1(i2)
            femag=fem1(i2)

            bc3=h3e/(h/femag-bc00(1:3,imag))
            h3t=h3e+bc00(1:3,imag)*bc3
            ht=vnor(h3t(1),h3t(2),h3t(3))

            dhth=ht-h

            hhbuff(2)=dhth
            bcbuff(2)=femag

            il=i1
            ih=i2

            do while ((ih-il).gt.1)

              i=(ih+il)/2

              h=feh1(i)
              femag=fem1(i)

              bc3=h3e/(h/femag-bc00(1:3,imag))
              h3t=h3e+bc00(1:3,imag)*bc3
              ht=vnor(h3t(1),h3t(2),h3t(3))
              dhth=ht-h

              hhbuff(3)=dhth
              bcbuff(3)=femag

              if (
     &            hhbuff(1).gt.0.0d0.and.hhbuff(3).lt.0.0d0.or.
     &            hhbuff(1).lt.0.0d0.and.hhbuff(3).gt.0.0d0
     &            ) then
                ih=i
                hhbuff(2)=hhbuff(3)
                bcbuff(2)=bcbuff(3)
              else if (
     &            hhbuff(2).gt.0.0d0.and.hhbuff(3).lt.0.0d0.or.
     &            hhbuff(2).lt.0.0d0.and.hhbuff(3).gt.0.0d0
     &            ) then
                il=i
                hhbuff(1)=hhbuff(3)
                bcbuff(1)=bcbuff(3)
              else
                ifail=91
                goto 91
              endif

            enddo !while

            dhtho=hhbuff(1)
            dhth=hhbuff(2)
            fm0=bcbuff(1)
            fm1=bcbuff(2)

            do ichi=1,nchiiron

              ddhth=(dhth-dhtho)/(fm1-fm0)
              femag=fm0-dhtho/ddhth
              fm0=fm1
              fm1=femag

              if (fm1.lt.fem1(nhz)) then
                if (isplinefm.eq.0) then
                  call util_interpol_linear(nhz,fem1,feh1,fm1,h,kfail)
                  if (kfail.ne.0) then
                    write(lun6,*)"*** Error in undumag_relax_iron: H is out of range for material or bad interpolation",mat
                    write(lun6,*)"mag, Hmin, Hmax, H:", imag,hmin,hmax,h
                    stop
                  endif
                else
                  call util_spline_inter(fem1,feh1,ufespl1,nhz,fm1,h,0)
                endif
              else
                fm1=fem1(nhz)
                h=feh1(nhz)
              endif

              bc3=h3e/(h/fm1-bc00(1:3,imag))
              h3t=h3e+bc00(1:3,imag)*bc3
              ht=vnor(h3t(1),h3t(2),h3t(3))
              dhtho=dhth
              dhth=ht-h
              femag=fm1

              if (abs(dhth).lt.chicut) then
                ifail=0
                exit
              endif

            enddo !nchiiron

91          continue

            if (ifail.ne.0.and.iwarn(imag).eq.0) then

              write(lun6,*)
              write(lun6,*)"*** Error in undumag_relax_iron: No solution found for magnet ",imag
              write(lun6,*)"Total iteration:",kiter
              write(lun6,*)"Iron iteration:",iter
              write(lun6,*)"Check parameters like nchiiron or chicut or try relaxation mode 2!"
              write(lun6,*)
              iwarn(imag)=1

            else

              bc3=h3e/(h/femag-bc00(1:3,imag))

              dbc=bc3-bco
              bc0(4:6,imag)=bco+dbc*dampiron

              if (ibulk.eq.0) then
                ! For this mode, magnetization is immediatly updated
                bpebc(4:6,imag)=bc0(4:6,imag)
                vmaglab(1:3)=bpebc(4:6,imag)
                do iplan=1,iabs(ibpeplan(imag))
                  bpetm(1,7,iplan,imag)=
     &              vmaglab(1)*bpetm(1,8,iplan,imag)+
     &              vmaglab(2)*bpetm(2,8,iplan,imag)+
     &              vmaglab(3)*bpetm(3,8,iplan,imag)
                enddo
              endif !ibulk

            endif !ifail

            !end mapmode.eq.3

          else !mapmode
            write(lun6,*)"*** Error in undumag_relax_iron: Unknown material mode:",mapmode
            stop
          endif !mapmode

        enddo !niron

        if (ibulk.ne.0) then
          lmag=0
          do kmag=iron1,iron2
            lmag=lmag+1
            imag=ishuffle(lmag)
            if (
     &          bc0(4,imag).ne.bc0(4,imag)
     &          .or.
     &          bc0(5,imag).ne.bc0(5,imag)
     &          .or.
     &          bc0(6,imag).ne.bc0(6,imag)
     &          ) then
              write(lun6,*)"*** Warning in undumag_relax_iron: Magnetization bpebc is not a number (NaN) ***"
              write(lun6,*)
     &          "kiter,iterirontot,imag,x,y,z:",
     &          kiter,iterirontot,imag,bpebc(1:3,imag)
              write(lun6,*)"Previous values of magnetization kept:"
              write(lun6,*)bpebc(4:6,imag)
            else
              bpebc(4:6,imag)=bc0(4:6,imag)
              bpebc(7,imag)=sqrt(
     &          bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
            endif

            vmaglab(1:3)=bpebc(4:6,imag)
            do iplan=1,iabs(ibpeplan(imag))
              bpetm(1,7,iplan,imag)=
     &          vmaglab(1)*bpetm(1,8,iplan,imag)+
     &          vmaglab(2)*bpetm(2,8,iplan,imag)+
     &          vmaglab(3)*bpetm(3,8,iplan,imag)
            enddo
          enddo

        endif !ibulk

        niteriron=iter
        kinside=0

        htesto=htest
        htest=0.0d0

        if (maxiteriron.gt.1) then

          call undumag_bconv(htest)
          if (
     &        htest-hold.gt.0.0d0.and.hcold.lt.0.0d0
     &        .or.
     &        htest-hold.lt.0.0d0.and.hcold.gt.0.0d0
     &        ) then
            lmag=0
            do kmag=iron1,iron2
              lmag=lmag+1
              imag=ishuffle(lmag)
              bpebc(4:6,imag)=(bpebc(4:6,imag)+bc00(4:6,imag))/2.0d0
              bpebc(7,imag)=sqrt(
     &          bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
              vmaglab(1:3)=bpebc(4:6,imag)
              do iplan=1,iabs(ibpeplan(imag))
                bpetm(1,7,iplan,imag)=
     &            vmaglab(1)*bpetm(1,8,iplan,imag)+
     &            vmaglab(2)*bpetm(2,8,iplan,imag)+
     &            vmaglab(3)*bpetm(3,8,iplan,imag)
              enddo
            enddo
          endif

          if (kdumpconv.ne.0) then
            kinside=-1
            lmag=0
            do kmag=iron1,iron2
              lmag=lmag+1
              imag=ishuffle(lmag)
              x=bpebc(1,imag)/1000.0d0
              y=bpebc(2,imag)/1000.0d0
              z=bpebc(3,imag)/1000.0d0
              call undumag_field(x,y,z,hx,hy,hz,ifail)
              imoth=nint(bpebc(15,imag))
              write(lunconv,*)kiter,iterirontot,imoth,imag,
     &          x*1000.0d0,y*1000.0d0,z*1000.0d0,
     &          hx,hy,hz,sqrt(hx**2+hy**2+hz**2)
     &          ,hxtest,hytest,hztest,htest,
     &          bpebc(4:6,imag),
     &          sqrt(bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
            enddo
          endif

          if (kconviron.gt.2) goto 9999

          if (hold.ne.0.0.and.dampfac.eq.0.0d0) then
            if (abs(htest/hold-1.0d0).lt.hconva) then
              kconviron=1
              goto 9999
            endif
          endif

          hcold=htest-hold
          hold=htest
          bc00(4:6,iron1:iron2)=bpebc(4:6,iron1:iron2)

        endif !maxiteriron.gt.1

      enddo !iter

9999  continue

      hold=htest

      return
      end
