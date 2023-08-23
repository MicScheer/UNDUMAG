*CMZ :  2.04/03 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/01 20/01/2023  17.31.48  by  Michael Scheer
*CMZ :  2.04/00 14/01/2023  16.20.31  by  Michael Scheer
*CMZ :  2.02/01 24/01/2022  16.24.16  by  Michael Scheer
*-- Author :    Michael Scheer   25/10/2021
      subroutine clcmag_shrink_xyz(imag)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure

      implicit none

      Type(T_Magnet) tmi,tmo

      double precision, dimension(:), allocatable :: x,y,z

      integer, dimension(:), allocatable :: khull,kface
      integer, dimension(:,:), allocatable :: kedge

      ! Shrinking of the magnet to the effective size
      ! xhull etc. refer to center of mass system

      double precision gcen(3)
      integer imag,i,istat,nin

      tmi=t_magnets(imag)
      tmo=t_magnets(imag)

      if (tmi%IsPole.ne.0) then
        write(lun6,*)"*** Warning clcmag_shrink_xyz is called for a pole ***"
      endif

      gcen=0.0d0
      call util_shrink_xyz(tmi%nhull,
     &  tmi%xhull0,tmi%yhull0,tmi%zhull0,gcen,coating,hulltiny,
     &  tmo%nhull,tmo%xhull,tmo%yhull,tmo%zhull,istat)

      nin=tmo%nhull

      allocate(x(nin),y(nin),z(nin),
     &  khull(nin),kedge(4,2*nin),kface((nin+1)*nin))

      call clcmag_update_magnet(tmo)

      t_magnets(imag)=tmo

      deallocate(x,y,z,khull,kedge,kface)

      return
      end
