*CMZ :  2.04/06 04/08/2023  11.26.53  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/00 24/12/2022  13.07.12  by  Michael Scheer
*CMZ :  2.02/01 19/01/2022  10.54.41  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_copy_voxels

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      integer iundumag_is_block
      integer imag,ivox,icopy,imodul,kmag

      type (T_Module) tmod
      type (T_Magnet) tmag
      type (T_Voxel) tv
      type (T_Voxel_Copy) tvc

      nvoxcopy_t=0
      do imag=1,nmagtot_t
        nvoxcopy_t=nvoxcopy_t+t_magnets(t_magcopy(imag)%kproto)%nvoxels
      enddo !imag

      allocate(t_voxcopy(nvoxcopy_t))

      nrec=0
      niron=0
      nvoxcopy_t=0

      do imag=1,nmagtot_t
        kmag=t_magcopy(imag)%kproto
        imodul=t_magcopy(imag)%kmodule
        icopy=t_magcopy(imag)%kcopy
        do ivox=1,t_magnets(kmag)%nvoxels
          nvoxcopy_t=nvoxcopy_t+1
          t_voxcopy(nvoxcopy_t)%kmodule=imodul
          t_voxcopy(nvoxcopy_t)%kmagnet=imag
          t_voxcopy(nvoxcopy_t)%kproto=kmag
          t_voxcopy(nvoxcopy_t)%kvoxel=ivox
          t_voxcopy(nvoxcopy_t)%kcopy=icopy
          t_voxcopy(nvoxcopy_t)%gcen=t_magnets(kmag)%t_voxels(ivox)%gcen
     &      +t_magcopy(imag)%gcen-t_magnets(kmag)%gcen
          t_voxcopy(nvoxcopy_t)%br=t_magnets(kmag)%t_voxels(ivox)%br
          t_voxcopy(nvoxcopy_t)%Ispole=t_magnets(kmag)%Ispole
          if (t_magnets(kmag)%Ispole.eq.0) then
            nrec=nrec+1
          else
            niron=niron+1
          endif
        enddo !ivox
      enddo !imag

      do ivox=1,nvoxcopy_t
        tvc=t_voxcopy(ivox)
        imag=tvc%kproto
        tmag=t_magnets(imag)
        tv=t_magnets(imag)%t_voxels(tvc%kvoxel)
        t_magnets(imag)%t_voxels(tvc%kvoxel)%IsBlock=tmag%IsBlock
        if (tmag%ctype.ne.'Cylinder'
     &      .and.tv%nhull.eq.8.and.tv%nedge.eq.12.and.tv%nface.eq.6
     &      .and.irecrepl.gt.0) then
          t_magnets(imag)%t_voxels(tvc%kvoxel)%IsBlock=
     &      iundumag_is_block(tv%xhull,tv%yhull,tv%zhull,tiny)
        endif
      enddo !ivox

      return
      end
