*CMZ :  2.04/11 26/08/2023  09.55.25  by  Michael Scheer
*CMZ :  2.04/10 23/08/2023  08.07.21  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  14.49.50  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  09.15.04  by  Michael Scheer
*CMZ :  2.04/06 01/08/2023  15.05.17  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/01 20/01/2023  08.02.02  by  Michael Scheer
*CMZ :  2.04/00 16/01/2023  15.35.00  by  Michael Scheer
*CMZ :  2.02/01 26/01/2022  11.22.41  by  Michael Scheer
*-- Author :    Michael Scheer   29/12/2021
      subroutine transrotcopcyl

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none
*KEEP,grarad.
      include 'grarad.cmn'
*KEND.

      type(T_magnet) tmag
      double precision rm(3,3),t8(8),r(3)
      integer imag,itr,ipoi,istat,ifound,iold,iv


      if(nmagcyl.eq.0) return

      iold=0
      do itr=1,ntransrotcop
        ifound=0
        do imag=1,nmagtot_t
          tmag=t_magnets(imag)
          t8=transrotcop(:,itr)
          if(
     &        tmag%cmoth.eq.ctransrotcop(itr).or.
     &        tmag%cnam.eq.ctransrotcop(itr)
     &        ) then
            ifound=itr
            if (tmag%ctype.ne.'Cylinder') then
              ifound=-itr
              cycle
            endif
            if (t8(8).eq.0.0d0) then
              t_magnets(imag)%xmin=t_magnets(imag)%xmin+t8(1)
              t_magnets(imag)%xmax=t_magnets(imag)%xmax+t8(1)
              t_magnets(imag)%ymin=t_magnets(imag)%ymin+t8(2)
              t_magnets(imag)%ymax=t_magnets(imag)%ymax+t8(2)
              t_magnets(imag)%zmin=t_magnets(imag)%zmin+t8(3)
              t_magnets(imag)%zmax=t_magnets(imag)%zmax+t8(3)
              t_magnets(imag)%xyz=t_magnets(imag)%xyz+t8(1:3)
              t_magnets(imag)%gcen=t_magnets(imag)%gcen+t8(1:3)
              do ipoi=1,tmag%nhull
                t_magnets(imag)%xhull(ipoi)=t_magnets(imag)%xhull(ipoi)+t8(1)
                t_magnets(imag)%yhull(ipoi)=t_magnets(imag)%yhull(ipoi)+t8(2)
                t_magnets(imag)%zhull(ipoi)=t_magnets(imag)%zhull(ipoi)+t8(3)
              enddo
              do iv=1,t_magnets(imag)%nvoxels
c                t_magnets(imag)%t_voxels(iv)%xmin=t_magnets(imag)%t_voxels(iv)%xmin+t8(1)
c                t_magnets(imag)%t_voxels(iv)%xmax=t_magnets(imag)%t_voxels(iv)%xmax+t8(1)
c                t_magnets(imag)%t_voxels(iv)%ymin=t_magnets(imag)%t_voxels(iv)%ymin+t8(2)
c                t_magnets(imag)%t_voxels(iv)%ymax=t_magnets(imag)%t_voxels(iv)%ymax+t8(2)
c                t_magnets(imag)%t_voxels(iv)%zmin=t_magnets(imag)%t_voxels(iv)%zmin+t8(3)
c                t_magnets(imag)%t_voxels(iv)%zmax=t_magnets(imag)%t_voxels(iv)%zmax+t8(3)
c                t_magnets(imag)%t_voxels(iv)%xyz=t_magnets(imag)%t_voxels(iv)%xyz+t8(1:3)
c                t_magnets(imag)%t_voxels(iv)%gcen=t_magnets(imag)%t_voxels(iv)%gcen+t8(1:3)
                do ipoi=1,t_magnets(imag)%t_voxels(iv)%nhull
                  t_magnets(imag)%t_voxels(iv)%xhull(ipoi)=t_magnets(imag)%t_voxels(iv)%xhull(ipoi)+t8(1)
                  t_magnets(imag)%t_voxels(iv)%yhull(ipoi)=t_magnets(imag)%t_voxels(iv)%yhull(ipoi)+t8(2)
                  t_magnets(imag)%t_voxels(iv)%zhull(ipoi)=t_magnets(imag)%t_voxels(iv)%zhull(ipoi)+t8(3)
                enddo
              enddo
            else if (t8(8).eq.3.0d0) then
              if (tmag%IsPole.ne.0) then
                write(lun6,*)"*** Warning in ransrotcopcyl: Setting remanence not allowed for iron ***"
                write(lun6,*)"*** Pole:",tmag%cnam
                cycle
              endif
              t_magnets(imag)%Brn=t8(1)
              t_magnets(imag)%Br=0.0d0
              if(norm2(t8(2:4)).ne.0.0d0)
     &          t_magnets(imag)%Br=t8(1)*t8(2:4)/norm2(t8(2:4))
              t_magnets(imag)%imat=int(t8(5))
              t_magnets(imag)%icol=int(t8(6))
            else if (t8(8).eq.1.0d0.or.t8(8).eq.2.0d0) then
              if (t8(8).eq.2.0d0) t_magnets(imag)%IsRotated=1
              call util_rotmat(t8(4:6),t8(7)*grarad1,rm,istat)
              if (t8(8).eq.2) then
                call util_mat_mul_vec_3x3(rm,tmag%br,t_magnets(imag)%br)
              endif
              r=tmag%xyz-t8(1:3)
              call util_mat_mul_vec_3x3(rm,r,r)
              t_magnets(imag)%xyz=r+t8(1:3)
              r=tmag%gcen-t8(1:3)
              call util_mat_mul_vec_3x3(rm,r,r)
              t_magnets(imag)%gcen=r+t8(1:3)
              !xmin=1.0d30
              !xmax=-1.0d30
              !ymin=1.0d30
              !ymax=-1.0d30
              !zmin=1.0d30
              !zmax=-1.0d30
              do ipoi=1,tmag%nhull
                r=[tmag%xhull(ipoi)-t8(1),tmag%yhull(ipoi)-t8(2),tmag%zhull(ipoi)-t8(3)]
                call util_mat_mul_vec_3x3(rm,r,r)
                t_magnets(imag)%xhull(ipoi)=r(1)+t8(1)
                t_magnets(imag)%yhull(ipoi)=r(2)+t8(2)
                t_magnets(imag)%zhull(ipoi)=r(3)+t8(3)
                !if (r(1)+t8(1).lt.xmin) xmin=r(1)+t8(1)
                !if (r(1)+t8(1).gt.xmax) xmax=r(1)+t8(1)
                !if (r(2)+t8(2).lt.ymin) ymin=r(2)+t8(2)
                !if (r(2)+t8(2).gt.ymax) ymax=r(2)+t8(2)
                !if (r(3)+t8(3).lt.zmin) zmin=r(3)+t8(3)
                !if (r(3)+t8(3).gt.zmax) zmax=r(3)+t8(3)
              enddo
              do iv=1,t_magnets(imag)%nvoxels
                if (t8(8).eq.2) then
                  call util_mat_mul_vec_3x3(rm,tmag%br,t_magnets(imag)%t_voxels(iv)%br)
                endif
                r=t_magnets(imag)%t_voxels(iv)%xyz-t8(1:3)
                call util_mat_mul_vec_3x3(rm,r,r)
                t_magnets(imag)%t_voxels(iv)%xyz=r+t8(1:3)
                r=t_magnets(imag)%t_voxels(iv)%gcen-t8(1:3)
                call util_mat_mul_vec_3x3(rm,r,r)
                t_magnets(imag)%t_voxels(iv)%gcen=r+t8(1:3)
                !xmin=1.0d30
                !xmax=-1.0d30
                !ymin=1.0d30
                !ymax=-1.0d30
                !zmin=1.0d30
                !zmax=-1.0d30
                do ipoi=1,t_magnets(imag)%t_voxels(iv)%nhull
                  r=[t_magnets(imag)%t_voxels(iv)%xhull(ipoi)-t8(1),
     &              t_magnets(imag)%t_voxels(iv)%yhull(ipoi)-t8(2),
     &              t_magnets(imag)%t_voxels(iv)%zhull(ipoi)-t8(3)]
                  call util_mat_mul_vec_3x3(rm,r,r)
                  t_magnets(imag)%t_voxels(iv)%xhull(ipoi)=r(1)+t8(1)
                  t_magnets(imag)%t_voxels(iv)%yhull(ipoi)=r(2)+t8(2)
                  t_magnets(imag)%t_voxels(iv)%zhull(ipoi)=r(3)+t8(3)
                  !if (r(1)+t8(1).lt.xmin) xmin=r(1)+t8(1)
                  !if (r(1)+t8(1).gt.xmax) xmax=r(1)+t8(1)
                  !if (r(2)+t8(2).lt.ymin) ymin=r(2)+t8(2)
                  !if (r(2)+t8(2).gt.ymax) ymax=r(2)+t8(2)
                  !if (r(3)+t8(3).lt.zmin) zmin=r(3)+t8(3)
                  !if (r(3)+t8(3).gt.zmax) zmax=r(3)+t8(3)
                enddo
              enddo
            else if (t8(8).lt.0.0d0) then
              call clcmag_copy(-int(t8(8)))
            else
              ifound=0
            endif
          endif
        enddo !imag

        if (ifound.eq.0) then
          write(lun6,*)"*** Warning in transrotcopcyl: No magnet found or undefined action ***"
          write(lun6,*)"Action, Magnet:",itr,ctransrotcop(itr)
          write(lun6,*)
          iold=itr
        endif
      enddo !ntransrotcop


      return
      end
