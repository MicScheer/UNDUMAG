*CMZ :  2.05/01 02/10/2023  16.19.49  by  Michael Scheer
*CMZ :  2.04/13 04/09/2023  10.23.44  by  Michael Scheer
*CMZ :  2.04/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/00 25/02/2023  14.36.37  by  Michael Scheer
*CMZ :  2.02/02 02/03/2022  13.26.45  by  Michael Scheer
*CMZ :  2.02/01 11/02/2022  09.49.54  by  Michael Scheer
*-- Author :    Michael Scheer   01/04/2016
      subroutine undumag_ini_magnets(kseg)

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      integer:: kseg,itrace=0

c      print*," "
c      print*," "
c      print*,"Baustellen:"
c      print*,"bpebc(18:20),corrtiny, dedgefb, simpson-file, chicut und andere Variablen aus undumag_ini_old.Randommechnismus, spez. mit Blick auf 1. Integral"
      print*," "

      ! Evaluate buffer of coils
      call clccoil_to_coils

      if (kseg.eq.2) return

      call undumag_read_modules

      ! Treat concave shapes
      if (nconcave_t.gt.0) then
        call clc_concave_to_convex
      endif

      ! Evaluate buffer of magnets and poles
      if (itrace.ne.0) print*,"calling clcbuff_to_magnets"
      call clcbuff_to_magnets

      ! Apply translations, rotations, and copying of magnets and poles
      if (ntransrotcop.ne.0) call clctransrotcop

      ! Drop zero magnets , after the call, xhull, yhull, zhull refer to gcen
      if (itrace.ne.0) print*,"calling clcmag_drop_zero_magnets"
      call clcmag_drop_zero_magnets

      ! Coating of magnets
      if (itrace.ne.0) print*,"calling clcmag_shrink_magnets"
      call clcmag_shrink_magnets

      ! Calculate center and normals of faces
      if (itrace.ne.0) print*,"calling clcmag_faces"
      call util_zeit_kommentar(lun6,"Calculating faces of magnets")
      call clcmag_faces
      call util_zeit_kommentar(lun6,"Done")

      ! Mothers
      call clcmag_mothers

      ! inhomogenities of magnets
      call clcmag_inhom

      ! Evaluate modules
      call clcmag_copy_magnets

      ! Apply longitudinal symmetry and center set-up
      call clcmag_sym

      ! segmentation of magnets
      if (itrace.ne.0) print*,"calling clcmag_cut"
      call clcmag_cut

      if (kseg.ne.0) then
        if (itrace.ne.0) print*,"leaving undumag_ini_magnets"
        call util_zeit_kommentar(lun6,"Leaving undumag_ini_magnets")
        return
      endif

      ! convert to internal arrays for undumag_proc and undumag_end
      call util_zeit_kommentar(lun6,"Preparing magnet structure for relaxation")
      if (itrace.ne.0) print*,"calling clcmag_to_bpe"
      call clcmag_to_bpe
      if (itrace.ne.0) print*,"calling clcmag_ini_bpetm"
      call undumag_ini_bpetm
      call util_zeit_kommentar(lun6,"Done")


      ! write set-up to lists
      if (itrace.ne.0) print*,"calling clcmag_magnets_list"
      call clcmag_magnets_list

      call clcmag_ini_force
      if (itrace.ne.0) print*,"leaving undumag_ini_magnets"

      return
      end
