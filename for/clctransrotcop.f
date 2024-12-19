*CMZ :          17/12/2024  15.54.59  by  Michael Scheer
*CMZ :  2.05/05 27/02/2024  14.40.04  by  Michael Scheer
*CMZ :  2.05/02 03/11/2023  16.01.07  by  Michael Scheer
*CMZ :  2.04/11 26/08/2023  09.53.27  by  Michael Scheer
*CMZ :  2.04/10 23/08/2023  08.05.44  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  14.49.50  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  09.15.04  by  Michael Scheer
*CMZ :  2.04/06 01/08/2023  15.05.17  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/01 20/01/2023  08.02.02  by  Michael Scheer
*CMZ :  2.04/00 16/01/2023  15.35.00  by  Michael Scheer
*CMZ :  2.02/01 26/01/2022  11.22.41  by  Michael Scheer
*-- Author :    Michael Scheer   29/12/2021
      subroutine clctransrotcop

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none
*KEEP,grarad,T=F77.
c-----------------------------------------------------------------------
c     grarad.cmn
c-----------------------------------------------------------------------
      double precision, parameter ::
     &  PI1=3.141592653589793D0,
     &  TWOPI1=2.0D0*PI1,HALFPI1=PI1/2.0D0,
     &  GRARAD1=PI1/180.0d0,RADGRA1=180.0d0/PI1
*KEND.

      type(T_magnet) tmag
      double precision rm(3,3),t8(8),r(3)
      integer mag,itr,ipoi,istat,ifound,iold,i,key,ipos(2,100),nwords
      integer :: ndim=100,j,nvar,iwc,ismoth
      character(2048) cline
      character(128) ctrans,cvar
      character(32) cconc

      double precision undumag_variable_getval


      do itr=1,ntransrotcop

        t8=transrotcop(:,itr)
        key=int(t8(8))
        i=int(t8(1))
        print*,itr,key

        if (i.le.0) cycle

        nvar=3
        if (key.eq.3) nvar=6

        cline=clcbuff(i)
        call util_string_split(cline,ndim,nwords,ipos,istat)

        do j=1,nvar
          cvar=cline(ipos(1,j):ipos(2,j))
          if (cvar(1:1).eq.'$') then
            transrotcop(j,itr)=undumag_variable_getval(trim(cvar))
          else
            read(cvar,*) transrotcop(j,itr)
          endif
        enddo !nvar

        if (key.eq.2) then
          ! Rotation
          cline=clcbuff(i+1)
          call util_string_split(cline,ndim,nwords,ipos,istat)
          do j=1,4
            cvar=cline(ipos(1,j):ipos(2,j))
            if (cvar(1:1).eq.'$') then
              transrotcop(3+j,itr)=undumag_variable_getval(trim(cvar))
            else
              read(cvar,*) transrotcop(3+j,itr)
            endif
          enddo
        endif !key

      enddo !ntransrotcop

      iold=0
      ctrans=''
      do itr=1,ntransrotcop
        t8=transrotcop(:,itr)
        ifound=0
        mag=1
        do while (mag.le.nmagtot_t)
          tmag=t_magnets(mag)
          iwc=tmag%iwasconcave
          if (iwc.gt.0) then
            cconc=t_concaves(tmag%iwasconcave)%tmag%cnam
          endif
          if (tmag%ctype.eq.'Cylinder') then
            ifound=-1
            cycle
          endif
          if(
     &        tmag%cmoth.eq.ctransrotcop(itr).or.
     &        tmag%cnam.eq.ctransrotcop(itr).or.
     &        iwc.gt.0.and.cconc.eq.ctransrotcop(itr)
     &        ) then
            ifound=itr
            if(tmag%cmoth.eq.ctransrotcop(itr)) then
              ismoth=1
            else
              ismoth=0
            endif
            if (t8(8).eq.0.0d0) then
              t_magnets(mag)%xmin=t_magnets(mag)%xmin+t8(1)
              t_magnets(mag)%xmax=t_magnets(mag)%xmax+t8(1)
              t_magnets(mag)%ymin=t_magnets(mag)%ymin+t8(2)
              t_magnets(mag)%ymax=t_magnets(mag)%ymax+t8(2)
              t_magnets(mag)%zmin=t_magnets(mag)%zmin+t8(3)
              t_magnets(mag)%zmax=t_magnets(mag)%zmax+t8(3)
              t_magnets(mag)%xyz=t_magnets(mag)%xyz+t8(1:3)
              t_magnets(mag)%gcen=t_magnets(mag)%gcen+t8(1:3)
              do ipoi=1,tmag%nhull
                t_magnets(mag)%xhull0(ipoi)=t_magnets(mag)%xhull0(ipoi)+t8(1)
                t_magnets(mag)%yhull0(ipoi)=t_magnets(mag)%yhull0(ipoi)+t8(2)
                t_magnets(mag)%zhull0(ipoi)=t_magnets(mag)%zhull0(ipoi)+t8(3)
                t_magnets(mag)%xhull(ipoi)=t_magnets(mag)%xhull(ipoi)+t8(1)
                t_magnets(mag)%yhull(ipoi)=t_magnets(mag)%yhull(ipoi)+t8(2)
                t_magnets(mag)%zhull(ipoi)=t_magnets(mag)%zhull(ipoi)+t8(3)
              enddo
            else if (t8(8).eq.3.0d0) then
              if (tmag%IsPole.ne.0) then
                write(lun6,*)"*** Warning in clctransrotcop: Setting remanence not allowed for iron ***"
                write(lun6,*)"*** Pole:",tmag%cnam
                cycle
              endif
              t_magnets(mag)%Brn=t8(1)
              t_magnets(mag)%Br=0.0d0
              if(norm2(t8(2:4)).ne.0.0d0)
     &          t_magnets(mag)%Br=t8(1)*t8(2:4)/norm2(t8(2:4))
              t_magnets(mag)%imat=int(t8(5))
              t_magnets(mag)%icol=int(t8(6))
            else if (t8(8).eq.1.0d0.or.t8(8).eq.2.0d0) then
              if (t8(8).eq.2.0d0) t_magnets(mag)%IsRotated=1
              call util_rotmat(t8(4:6),t8(7)*grarad1,rm,istat)
              if (t8(8).eq.2) then
                call util_mat_mul_vec_3x3(rm,tmag%br,t_magnets(mag)%br)
              endif
              r=tmag%gcen-t8(1:3)
              call util_mat_mul_vec_3x3(rm,r,r)
              t_magnets(mag)%gcen=r+t8(1:3)
              r=tmag%gcen-t8(1:3)
              call util_mat_mul_vec_3x3(rm,r,r)
              t_magnets(mag)%gcen=r+t8(1:3)
              do ipoi=1,tmag%nhull
                r=[tmag%xhull0(ipoi)-t8(1),tmag%yhull0(ipoi)-t8(2),tmag%zhull0(ipoi)-t8(3)]
                call util_mat_mul_vec_3x3(rm,r,r)
                t_magnets(mag)%xhull0(ipoi)=r(1)+t8(1)
                t_magnets(mag)%yhull0(ipoi)=r(2)+t8(2)
                t_magnets(mag)%zhull0(ipoi)=r(3)+t8(3)
                r=[tmag%xhull(ipoi)-t8(1),tmag%yhull(ipoi)-t8(2),tmag%zhull(ipoi)-t8(3)]
                call util_mat_mul_vec_3x3(rm,r,r)
                t_magnets(mag)%xhull(ipoi)=r(1)+t8(1)
                t_magnets(mag)%yhull(ipoi)=r(2)+t8(2)
                t_magnets(mag)%zhull(ipoi)=r(3)+t8(3)
              enddo
            else if (t8(8).lt.0.0d0) then
              call clcmag_copy(-int(t8(8)))
              exit
            else
              ifound=0
            endif
          endif
          if (iwc.eq.0.and.ifound.ne.0.and.ismoth.eq.0) exit
          mag=mag+1
        enddo !mag
c        if (ifound.eq.0.and.itr.ne.iold.and.ctrans.ne.ctransrotcop(itr)) then
        if (ifound.eq.0) then
          write(lun6,*)"*** Warning in clctransrotcop: No magnet found or undefined action ***"
          write(lun6,*)"Action, Magnet:",itr,ctransrotcop(itr)
          write(lun6,*)
          iold=itr
          ctrans=ctransrotcop(itr)
        endif
      enddo !ntransrotcop


      call transrotcopcyl

      return
      end
