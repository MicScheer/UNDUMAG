*CMZ :  2.04/13 04/09/2023  08.27.06  by  Michael Scheer
*CMZ :  2.04/08 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/06 04/08/2023  11.24.35  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 19/01/2022  10.38.36  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_cut

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision, dimension (:,:,:), allocatable :: corn1,corn2
      integer, dimension (:), allocatable :: ncorn1,ncorn2

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      integer imag

      ! for the voxels xyz = gcen!
      ! xhull,yhull,zhull refer to gcen for voxels

      call util_zeit_kommentar(lun6,"Cutting magnets")

      allocate(corn1(3,2*ncornmax,2*nplanmax),corn2(3,2*ncornmax,2*nplanmax))
      allocate(ncorn1(2*nplanmax),ncorn2(2*nplanmax))

      allocate(khull(2*ncornmax*nplanmax),kedge(4,2*ncornmax*nplanmax-2),
     &  kface(5*ncornmax*nplanmax))

      do imag=1,nmag_t+nspecmag_t
        if (t_magnets(imag)%ctype.ne.'Cylinder') then
          call clcmag_xcut(imag)
          call clcmag_ycut(imag)
          call clcmag_zcut(imag)
        endif
      enddo !nmag_t

      deallocate(corn1,corn2)

      call clcmag_voxels
      call clcmag_copy_voxels
c      call clcmag_facets


      call util_zeit_kommentar(lun6,"Done")

      return
      end
