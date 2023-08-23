*CMZ :  2.02/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 26/02/2021  17.57.38  by  Michael Scheer
*CMZ :  2.01/08 14/08/2020  11.48.46  by  Michael Scheer
*CMZ :  2.01/02 27/04/2018  12.08.59  by  Michael Scheer
*CMZ :  2.00/02 17/04/2018  13.43.36  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  15.38.41  by  Michael Scheer
*CMZ :  2.00/00 09/04/2018  12.17.36  by  Michael Scheer
*CMZ :  1.25/05 05/04/2018  19.27.23  by  Michael Scheer
*CMZ :  1.25/03 23/03/2018  17.02.28  by  Michael Scheer
*CMZ :  1.25/02 21/03/2018  12.54.05  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  16.04.47  by  Michael Scheer
*CMZ :  1.25/00 16/03/2018  12.51.10  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_bcoils(xin,yin,zin,bxout,byout,bzout,istat)

      use undumagf90m

      implicit none

      double precision, dimension (:,:), allocatable :: wold
      double precision xin,yin,zin,bxout,byout,bzout,xsymmm

      integer istat,i,ical,lunfil,j,nallo,icoil,k,icold,ic

      data ical/0/

      save ical,xsymmm

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      istat=0

      ncoil=nwind+ncwires+nrace+ncrace+narc+ncarc+nthwir+nrbar

      if (ncoil.eq.0) return

      if (ical.eq.0.and.kwave.eq.0) then

        if (ncwires.gt.0) then
          icoil=nint(wire(11,ncwires))
        else
          icoil=0
        endif

        xsymmm=xsym*1000.0d0

        ncfila=ncwires

        if (nwind+nrace+ncrace+narc+ncarc+nthwir+nrbar.gt.0) then

          nwwind=0
          nwrace=0
          nwcrace=0
          nwarc=0
          nwcarc=0
          nwthwir=0
          nwrbar=0

          do i=1,ncrace
            if (crace(1,i).eq.0.0d0) cycle
            if (nint(crace(14,i)).le.0) crace(14,i)=1.0d0
            if (nint(crace(15,i)).le.0) crace(15,i)=1.0d0
            if (nint(crace(16,i)).le.0) crace(16,i)=1.0d0
            nwcrace=nwcrace+nint(4*crace(14,i)*crace(15,i)*(1+crace(16,i)))
          enddo

          do i=1,nrace
            if (race(1,i).eq.0.0d0) cycle
            if (nint(race(14,i)).le.0) race(14,i)=1.0d0
            if (nint(race(15,i)).le.0) race(15,i)=1.0d0
            if (nint(race(16,i)).le.0) race(16,i)=1.0d0
            nwrace=nwrace+nint(4*race(14,i)*race(15,i)*(1+race(16,i)))
          enddo

          do i=1,nwind
            if (wind(1,i).eq.0.0d0) cycle
            if (nint(wind(14,i)).le.0) wind(14,i)=1.0d0
            if (nint(wind(15,i)).le.0) wind(15,i)=1.0d0
            if (nint(wind(16,i)).le.0) wind(16,i)=1.0d0
            nwwind=nwwind+nint(4*wind(14,i)*wind(15,i)*(1+wind(16,i)))
          enddo

          do i=1,narc
            if (arc(1,i).eq.0.0d0) cycle
            if (nint(arc(9,i)).le.0) arc(9,i)=1.0d0
            if (nint(arc(10,i)).le.0) arc(10,i)=1.0d0
            if (nint(arc(11,i)).le.0) arc(11,i)=1.0d0
            nwarc=nwarc+nint(arc(9,i)*arc(10,i)*arc(11,i))
          enddo

          do i=1,nthwir
            if (thickwire(1,i).eq.0.0d0) cycle
            if (nint(thickwire(8,i)).le.0) thickwire(8,i)=1.0d0
            if (nint(thickwire(7,i)).le.0) thickwire(7,i)=1.0d0
            nwthwir=nwthwir+nint(thickwire(8,i)*thickwire(7,i))
          enddo

          do i=1,nrbar
            if (rectbar(1,i).eq.0.0d0) cycle
            if (nint(rectbar(8,i)).le.0) rectbar(8,i)=1.0d0
            if (nint(rectbar(9,i)).le.0) rectbar(9,i)=1.0d0
            nwrbar=nwrbar+nint(rectbar(8,i)*rectbar(9,i))
          enddo

          do i=1,ncarc
            if (carc(1,i).eq.0.0d0) cycle
            if (nint(carc(8,i)).le.0) carc(8,i)=1.0d0
            if (nint(carc(9,i)).le.0) carc(9,i)=1.0d0
            if (nint(carc(10,i)).le.0) carc(10,i)=1.0d0
            nwcarc=nwcarc+nint(carc(8,i)*carc(9,i)*carc(10,i))
          enddo

          nallo=ncfila+nwwind+nwrace+nwcrace+nwarc+nwcarc+nwrbar+nwthwir
          if (ncfila.gt.0) then
            allocate(wold(nwitems,ncfila))
            wold=wire
            deallocate(wire)
            allocate(wire(nwitems,nallo))
            wire(:,1:ncfila)=wold(:,1:ncfila)
            deallocate(wold)
          else
            allocate(wire(nwitems,nallo))
            wire=0.0d0
          endif

          do i=1,nwind
            if (wind(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_wind_to_fila(i,icoil)
          enddo

          do i=1,nrace
            if (race(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_race_to_fila(i,icoil)
          enddo

          do i=1,ncrace
            if (crace(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_crace_to_fila(i,icoil)
          enddo

          do i=1,narc
            if (arc(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_arc_to_fila(i,icoil)
          enddo

          do i=1,nrbar
            if (rectbar(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_bar_to_fila(i,icoil)
          enddo

          do i=1,ncarc
            if (carc(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_circ_arc_to_fila(i,icoil)
          enddo

          do i=1,nthwir
            if (thickwire(1,i).eq.0.0d0) cycle
            icoil=icoil+1
            call undumag_thwire_to_fila(i,icoil)
          enddo

        endif !coils

        if (ncwires.eq.0) return

        do i=1,ncwires
          do j=2,8
            if (abs(wire(j,i)).lt.1.0d-9) wire(j,i)=0.0d0
          enddo
        enddo

        nallo=0
        do i=1,ncwires
          if (wire(2,i).ne.0.0d0) nallo=nallo+1
        enddo

        allocate(wold(nwitems,ncwires))
        wold=wire
        deallocate(wire)
        allocate(wire(nwitems,nallo))

        nallo=0
        do i=1,ncwires
          if (wold(2,i).eq.0.0d0) cycle
          nallo=nallo+1
          wire(:,nallo)=wold(:,i)
        enddo
        ncwires=nallo
        deallocate(wold)

c concept of wires:
        ! 1: type of coil
        ! 2: curr
        ! 3: x1
        ! 4: y1
        ! 5: z1
        ! 6: x2
        ! 7: y2
        ! 8: z2
        ! 9: color
        ! 10: number coil in group
        ! 11: absolute coil number

        open(newunit=lunfil,file='undumag.fil')
        write(lunfil,'(a)')trim(cundutit)
        k=0
        icold=nint(wire(11,1))
        do i=1,ncwires
          k=k+1
          ic=nint(wire(11,i))
          write(lunfil,'(i5,7g15.5,3i5)')
     &      nint(wire(1,i)),wire(2:8,i),nint(wire(9,i)),k,ic
          if (icold.ne.ic) k=0
          icold=ic
        enddo
        close(lunfil)

        ical=1

      endif !ical

      if (ncwires.eq.0) return

      call undumag_bcoils_omp(xin,yin,zin,bxout,byout,bzout,istat)

      if (bxout.ne.bxout.or.byout.ne.byout.or.bzout.ne.bzout) then
        print*," "
        print*,"*** Error in undumag_bcoils: Singularity hit for"
        print*,"x,y,z [mm]:", xin*1000.,yin*1000.,zin*1000.
        bxout = 9999.
        byout = 9999.
        bzout = 9999.
        istat=-1
      endif

      return
      end
