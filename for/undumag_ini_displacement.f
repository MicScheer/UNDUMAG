*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*-- Author :    Michael Scheer   19/10/2021
      subroutine undumag_ini_displacement

      use undumagf90m
      use commandlinef90m
      use displacement

      implicit none

      real g(100)
      integer lun,ieof,k,i

      character(2048) cline

      if (kdisplace.ne.0) then
        kdisplace=0
        if (ixsym.gt.0.or.iysym.gt.0.or.izsym.gt.0) then
          write(lun6,*)
          write(lun6,*)"*** Warning in undumag_ini_old: kdisplace is set, but set-up is mirrored due to ixsym, iysym, or izsym ***"
          write(lun6,*)
        endif
        open(newunit=lun,file="undumag.dis",status="old")
        do while (.true.)
          call util_skip_comment_end(lun,ieof)
          read(lun,'(a)')cline
          kdisplace=kdisplace+1
        enddo
        rewind(lun)
        allocate(chdisp(kdisplace))
        allocate(displace(18,kdisplace))
        do i=1,kdisplace
          ! displace(1:3,i) displacement of magnet
          ! displace(4:6,i) yrot, zrot, dphi of rotation around x-axis
          ! displace(7:9,i) change of magnetization
          ! displace(10:18,i)
          !mode: 0, apply changes as they are
          !mode: 1, apply changes with randomization
          call util_skip_comment(lun)
          read(lun,*)chdisp(i),displace(1:18,i)
          call util_random(9,g)
          do k=1,9
            if (displace(k+9,i).ne.0.0d0) displace(k,i)=displace(k,i)*g(k)
          enddo
        enddo
        close(lun)
      endif

      return
      end
