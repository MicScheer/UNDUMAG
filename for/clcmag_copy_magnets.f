*CMZ :  2.04/08 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  15.35.36  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/01 22/01/2023  13.28.01  by  Michael Scheer
*CMZ :  2.02/01 10/02/2022  22.24.03  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_copy_magnets

! Rotates primary magnets according to module rotation
! The rotated primary magnets are copied and shiftet according to
! module definitions.

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      double precision, dimension (:), allocatable :: xsort
      double precision gcen(3),xyz(3),vspace(3),off(3),phi,shift(3),rot(3,3),
     &  scalmag(3),brn,xmin,xmax,ymin,ymax,zmin,zmax

      integer imag,imodul,ic,i,ispole,k,kmag,nesti,kproto,istat

      character(32) cmod,ccop

*KEEP,grarad,T=F77.
c-----------------------------------------------------------------------
c     grarad.cmn
c-----------------------------------------------------------------------
      double precision, parameter ::
     &  PI1=3.141592653589793D0,
     &  TWOPI1=2.0D0*PI1,HALFPI1=PI1/2.0D0,
     &  GRARAD1=PI1/180.0d0,RADGRA1=180.0d0/PI1
*KEND.

      type (T_Module) tmod
      type (T_Magnet_Copy), dimension (:), allocatable :: tmc

      nmagtot_t=0

      if (nmodule_t.gt.0) then
        do imodul=1,nmodule_t
          tmod=t_modules(imodul)
          do imag=1,nmag_t
            nmagtot_t=nmagtot_t+tmod%ncopy
            t_magnets(imag)%ncopy=t_magnets(imag)%ncopy+tmod%ncopy
          enddo !imag
        enddo
      else
        nmagtot_t=nmag_t
      endif

      do imag=1,nmag_t+nspecmag_t
        allocate(t_magnets(imag)%kcopy(t_magnets(imag)%ncopy))
      enddo !imag

      nmagtot_t=nmagtot_t+nspecmag_t

      allocate(ksort_t(nmagtot_t),t_magcopy(nmagtot_t))

      allocate(xsort(nmagtot_t))
      nmagtot_t=0

      do imodul=1,nmodule_t

        write(cmod,*)imodul

        tmod=t_modules(imodul)
        vspace=tmod%vspace
        off=tmod%offset
        phi=tmod%phi*grarad1
        rot=tmod%rotmat
        scalmag=tmod%scalmag

        do imag=1,nmag_t

          if (t_magnets(imag)%kmodule.ne.imodul) cycle

          gcen=t_magnets(imag)%gcen
          ispole=t_magnets(imag)%ispole

          xmin=1.0d30
          xmax=-1.0d30
          ymin=1.0d30
          ymax=-1.0d30
          zmin=1.0d30
          zmax=-1.0d30

          do i=1,t_magnets(imag)%nhull
            xyz=[t_magnets(imag)%xhull(i),t_magnets(imag)%yhull(i),t_magnets(imag)%zhull(i)]
            call util_mat_mul_vec_3x3(rot,xyz,xyz)
            t_magnets(imag)%xhull(i)=xyz(1)
            t_magnets(imag)%yhull(i)=xyz(2)
            t_magnets(imag)%zhull(i)=xyz(3)
            !if (t_magnets(imag)%ctype.ne.'Cylinder') then
               if (t_magnets(imag)%xhull(i).lt.xmin) xmin=t_magnets(imag)%xhull(i)
              if (t_magnets(imag)%xhull(i).gt.xmax) xmax=t_magnets(imag)%xhull(i)
              if (t_magnets(imag)%yhull(i).lt.ymin) ymin=t_magnets(imag)%yhull(i)
              if (t_magnets(imag)%yhull(i).gt.ymax) ymax=t_magnets(imag)%yhull(i)
              if (t_magnets(imag)%zhull(i).lt.zmin) zmin=t_magnets(imag)%zhull(i)
              if (t_magnets(imag)%zhull(i).gt.zmax) zmax=t_magnets(imag)%zhull(i)
            !else
            !  size=t_magnets(imag)%size
            !endif
          enddo

          t_magnets(imag)%xmin=xmin
          t_magnets(imag)%xmax=xmax
          t_magnets(imag)%ymin=ymin
          t_magnets(imag)%ymax=ymax
          t_magnets(imag)%zmin=zmin
          t_magnets(imag)%zmax=zmax

          if (t_magnets(imag)%ctype.ne.'Cylinder') then
            t_magnets(imag)%size(1)=xmax-xmin
            t_magnets(imag)%size(2)=ymax-ymin
            t_magnets(imag)%size(3)=zmax-zmin
          endif

          call util_mat_mul_vec_3x3(rot,t_magnets(imag)%gcen,t_magnets(imag)%gcen)
          call util_mat_mul_vec_3x3(rot,t_magnets(imag)%xyz,t_magnets(imag)%xyz)

          do ic=1,tmod%ncopy
            write(ccop,*)ic
            nmagtot_t=nmagtot_t+1
            t_magnets(imag)%kcopy(ic)=nmagtot_t
            ksort_t(nmagtot_t)=nmagtot_t
            shift=off+(ic-1)*vspace
            t_magcopy(nmagtot_t)%gcen=t_magnets(imag)%gcen+shift
            t_magcopy(nmagtot_t)%xmin=t_magnets(imag)%xmin
            t_magcopy(nmagtot_t)%xmax=t_magnets(imag)%xmax
            t_magcopy(nmagtot_t)%ymin=t_magnets(imag)%ymin
            t_magcopy(nmagtot_t)%ymax=t_magnets(imag)%ymax
            t_magcopy(nmagtot_t)%zmin=t_magnets(imag)%zmin
            t_magcopy(nmagtot_t)%zmax=t_magnets(imag)%zmax
            t_magcopy(nmagtot_t)%size=t_magnets(imag)%size
            ksort_t(nmagtot_t)=nmagtot_t
            xsort(nmagtot_t)=t_magcopy(nmagtot_t)%gcen(1)
            t_magcopy(nmagtot_t)%ispole=ispole
            t_magcopy(nmagtot_t)%isspecial=0
            t_magcopy(nmagtot_t)%kproto=imag
            t_magcopy(nmagtot_t)%kmodule=imodul
            t_magcopy(nmagtot_t)%kcopy=ic
            t_magcopy(nmagtot_t)%br=t_magnets(imag)%br*scalmag
            brn=norm2(t_magcopy(nmagtot_t)%br)
            t_magcopy(nmagtot_t)%brn=brn
            if (brn.ne.0.0d0)
     &        t_magcopy(nmagtot_t)%br=t_magcopy(nmagtot_t)%br/brn
            if (imodul.ne.1.or.ic.ne.1) then
              t_magcopy(nmagtot_t)%cnam=trim(adjustl(t_magnets(imag)%cnam)) // "_"
     &          // trim(adjustl(cmod)) // "_" // trim(adjustl(ccop))
              t_magcopy(nmagtot_t)%cmoth=trim(adjustl(t_magnets(imag)%cmoth)) // "_"
     &          // trim(adjustl(cmod)) // "_" // trim(adjustl(ccop))
            else
              t_magcopy(nmagtot_t)%cnam=t_magnets(imag)%cnam
              t_magcopy(nmagtot_t)%cmoth=t_magnets(imag)%cmoth
            endif
          enddo
        enddo !imag
      enddo

      do i=1,nspecmag_t

        imag=nmag_t+i

        ispole=t_magnets(imag)%ispole
        gcen=t_magnets(imag)%gcen

        xmin=1.0d30
        xmax=-1.0d30
        ymin=1.0d30
        ymax=-1.0d30
        zmin=1.0d30
        zmax=-1.0d30

        do k=1,t_magnets(imag)%nhull
          if (t_magnets(imag)%xhull(k).lt.xmin) xmin=t_magnets(imag)%xhull(k)
          if (t_magnets(imag)%xhull(k).gt.xmax) xmax=t_magnets(imag)%xhull(k)
          if (t_magnets(imag)%yhull(k).lt.ymin) ymin=t_magnets(imag)%yhull(k)
          if (t_magnets(imag)%yhull(k).gt.ymax) ymax=t_magnets(imag)%yhull(k)
          if (t_magnets(imag)%zhull(k).lt.zmin) zmin=t_magnets(imag)%zhull(k)
          if (t_magnets(imag)%zhull(k).gt.zmax) zmax=t_magnets(imag)%zhull(k)
        enddo

        t_magnets(imag)%xmin=xmin
        t_magnets(imag)%xmax=xmax
        t_magnets(imag)%ymin=ymin
        t_magnets(imag)%ymax=ymax
        t_magnets(imag)%zmin=zmin
        t_magnets(imag)%zmax=zmax

        if (t_magnets(imag)%ctype.ne.'Cylinder') then
          t_magnets(imag)%size(1)=xmax-xmin
          t_magnets(imag)%size(2)=ymax-ymin
          t_magnets(imag)%size(3)=zmax-zmin
        endif

        nmagtot_t=nmagtot_t+1
        ksort_t(nmagtot_t)=nmagtot_t
        xsort(nmagtot_t)=t_magnets(imag)%gcen(1)

        t_magcopy(nmagtot_t)%ispole=ispole
        t_magcopy(nmagtot_t)%isspecial=1
        t_magcopy(nmagtot_t)%kproto=imag
        t_magcopy(nmagtot_t)%kmodule=0
        t_magcopy(nmagtot_t)%kcopy=0
        t_magcopy(nmagtot_t)%cnam=t_magnets(imag)%cnam
        t_magcopy(nmagtot_t)%cmoth=t_magnets(imag)%cmoth
        t_magcopy(nmagtot_t)%gcen=t_magnets(imag)%gcen
        t_magcopy(nmagtot_t)%br=t_magnets(imag)%br
        t_magcopy(nmagtot_t)%brn=t_magnets(imag)%brn

        t_magcopy(nmagtot_t)%xmin=t_magnets(imag)%xmin
        t_magcopy(nmagtot_t)%xmax=t_magnets(imag)%xmax
        t_magcopy(nmagtot_t)%ymin=t_magnets(imag)%ymin
        t_magcopy(nmagtot_t)%ymax=t_magnets(imag)%ymax
        t_magcopy(nmagtot_t)%zmin=t_magnets(imag)%zmin
        t_magcopy(nmagtot_t)%zmax=t_magnets(imag)%zmax

      enddo !imag

      call util_sort_index(nmagtot_t,xsort,ksort_t)
      deallocate(xsort)

      allocate(tmc(nmagtot_t))
      tmc=t_magcopy

      xmin_t=1.0d30
      xmax_t=-1.0d30
      ymin_t=1.0d30
      ymax_t=-1.0d30
      zmin_t=1.0d30
      zmax_t=-1.0d30

      do imag=1,nmagtot_t
        t_magcopy(imag)=tmc(ksort_t(imag))
        gcen=t_magcopy(imag)%gcen
        if (t_magcopy(imag)%xmin+gcen(1).lt.xmin_t) xmin_t=gcen(1)+t_magcopy(imag)%xmin
        if (t_magcopy(imag)%xmax+gcen(1).gt.xmax_t) xmax_t=gcen(1)+t_magcopy(imag)%xmax
        if (t_magcopy(imag)%ymin+gcen(2).lt.ymin_t) ymin_t=gcen(2)+t_magcopy(imag)%ymin
        if (t_magcopy(imag)%ymax+gcen(2).gt.ymax_t) ymax_t=gcen(2)+t_magcopy(imag)%ymax
        if (t_magcopy(imag)%zmin+gcen(3).lt.zmin_t) zmin_t=gcen(3)+t_magcopy(imag)%zmin
        if (t_magcopy(imag)%zmax+gcen(3).gt.zmax_t) zmax_t=gcen(3)+t_magcopy(imag)%zmax
      enddo

      tmc=t_magcopy
      kmag=0
      nesti=0

      do imag=1,nmagtot_t
        if (tmc(imag)%IsPole.ne.0) cycle
        kproto=tmc(imag)%kproto
        nesti=nesti+
     &    t_magnets(kproto)%nxdiv*t_magnets(kproto)%nydiv*t_magnets(kproto)%nzdiv
        kmag=kmag+1
        t_magcopy(kmag)=tmc(imag)
      enddo

      do imag=1,nmagtot_t
        if (tmc(imag)%IsPole.eq.0) cycle
        kproto=tmc(imag)%kproto
        nesti=nesti+
     &    t_magnets(kproto)%nxdiv*t_magnets(kproto)%nydiv*t_magnets(kproto)%nzdiv
        kmag=kmag+1
        t_magcopy(kmag)=tmc(imag)
      enddo

      deallocate(tmc)

      if (matrix.ne.0) then
        allocate(wwmatrix4(3,3,nesti,nesti),stat=istat)
        if (istat.ne.0) then
          write(lun6,*)'*** Warning in clcmag_copy_magnets: Not enough memory for interaction matrix of size 3 x 3 x',nesti,' x ',nesti
          write(lun6,*) '** UNDUMAG will fail in undumag_proc ***'
        else
          deallocate(wwmatrix4)
          write(lun6,*)
          write(lun6,*)'--- Expected size of interaction matrix of size 3x3x',nesti,' x ',nesti
        endif
      endif

      return
      end
