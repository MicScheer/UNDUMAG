*CMZ :  2.04/13 04/09/2023  10.46.42  by  Michael Scheer
*CMZ :  2.04/07 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  20.55.09  by  Michael Scheer
*CMZ :  2.02/01 11/02/2022  09.47.13  by  Michael Scheer
*-- Author :    Michael Scheer   01/04/2016
      subroutine clcmag_to_bpe

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision xyz(3),br(3),bn,dx,dy,dz,bpe17,gcen(3)

      integer :: idebug=0

      integer kmod,kmag,kcopy,npoi,n,k,iv,ipoi,i,im,lunlis,ieof,lunkill,
     &  last,kill,kv

      character(128) cline
      character(32) chsel

*KEEP,random.
      include 'random.cmn'
*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

c      type (T_Magnet) :: tmag
      type (T_Voxel) :: tv

      allocate(bpebc(20,nvoxcopy_t))
      bpebc=0.0d0

      ! bpebc(1:3,... position x,y,z
      ! bpebc(4:6,... M vector, might be changed during relaxation
      ! bpebc(7,... length of M vector, might be changed during relaxation
      ! bpebc(8,... type
      ! bpebc(9,imag)=imat
      ! bpebc(10,imag) special magnet flag
      ! bpebc(11:13,imag) normalized M vector at the beginning, will survive
      ! bpebc(14,imag) length M vector at the beginning, will survive
      ! bpebc(15,imag) Mother volume
      ! bpebc(16,imag) kill flag
      ! bpebc(17,imag) select flag
      ! bpebc(18:20,imag) B of coils in voxel

      ! ibpeplan(imag)=nplan
      ! ibpecorn(iplan,imag)=ncorn
      ! ibpecol(imag) color index

      allocate(ibpeplan(nvoxcopy_t))
      allocate(ibpecol(nvoxcopy_t))
      allocate(ibpecorn(nplanmax,nvoxcopy_t))

      allocate(bpetm(3,8,nplanmax,nvoxcopy_t))
      allocate(bpemag(3,ncornmax,nplanmax,nvoxcopy_t))
      allocate(bperot(3,ncornmax,nplanmax,nvoxcopy_t))

      kv=0

      do iv=1,nvoxcopy_t

        if (idebug.ne.0) then
          print*,iv
        endif

        kmod=t_voxcopy(iv)%kmodule
        kmag=t_voxcopy(iv)%kproto

c        tmag=t_magnets(kmag)
        if (t_magnets(kmag)%IsPole.ne.0) cycle

        kv=kv+1

        im=t_voxcopy(iv)%kmagnet
        kvox=t_voxcopy(iv)%kvoxel
        kcopy=t_voxcopy(iv)%kcopy

        tv=t_magnets(kmag)%t_voxels(kvox)

        gcen=t_voxcopy(iv)%gcen
        br=t_voxcopy(iv)%br

        bpebc(1,kv)=gcen(1)+xcentershift
        bpebc(2:3,kv)=gcen(2:3)

        bpebc(4:6,kv)=br
        bpebc(7,kv)=norm2(br)

        if ((irecrepl.ne.0.and.tv%isblock.ne.0).or.t_magnets(kmag)%isblock.gt.0) then
          bpebc(8,kv)=-6.0
          dx=tv%xmax-tv%xmin
          dy=tv%ymax-tv%ymin
          dz=tv%zmax-tv%zmin
          call clcmag_bpemag(dx,dy,dz,bpemag(1:3,1:5,1:6,kv),bpebc(1:3,kv))
          ibpecorn(:,kv)=5
        else !Is Block}
          bpebc(8,kv)=1.0
        endif

        bpebc(9,kv)=t_magnets(kmag)%imat
        bpebc(10,kv)=t_magnets(kmag)%isspecial
        bn=norm2(br)

        if (bn.ne.0.0d0) bpebc(11:13,kv)=br/bn

        bpebc(14,kv)=bn
        bpebc(15,kv)=im
        bpebc(16,kv)=0.0
        bpebc(17,kv)=0.0

        ibpecol(kv)=t_magnets(kmag)%icol
        ibpeplan(kv)=tv%nface

        if (tv%IsBlock.gt.0) cycle

        k=1
        do n=1,tv%nface
          npoi=tv%kface(k)
          ibpecorn(n,kv)=npoi+1
          do i=1,npoi
            k=k+1
            ipoi=tv%kface(k)
            xyz=[tv%xhull(ipoi),tv%yhull(ipoi),tv%zhull(ipoi)]
            bpemag(1:3,i,n,kv)=xyz+bpebc(1:3,kv)
          enddo
          bpemag(1:3,npoi+1,n,kv)=bpemag(1:3,1,n,kv)
          k=k+1
        enddo

      enddo !voxel

      do iv=1,nvoxcopy_t

        kmod=t_voxcopy(iv)%kmodule
        kmag=t_voxcopy(iv)%kproto

c        tmag=t_magnets(kmag)
        if (t_magnets(kmag)%IsPole.eq.0) cycle

        kv=kv+1

        im=t_voxcopy(iv)%kmagnet
        kvox=t_voxcopy(iv)%kvoxel
        kcopy=t_voxcopy(iv)%kcopy

        tv=t_magnets(kmag)%t_voxels(kvox)

        gcen=t_voxcopy(iv)%gcen
        br=t_voxcopy(iv)%br

        bpebc(1,kv)=gcen(1)+xcentershift
        bpebc(2:3,kv)=gcen(2:3)

        bpebc(4:6,kv)=br
        bpebc(7,kv)=norm2(br)

        if ((irecrepl.ne.0.and.tv%isblock.ne.0).or.t_magnets(kmag)%isblock.gt.0) then
          bpebc(8,kv)=-6.0
          dx=tv%xmax-tv%xmin
          dy=tv%ymax-tv%ymin
          dz=tv%zmax-tv%zmin
          call clcmag_bpemag(dx,dy,dz,bpemag(1:3,1:5,1:6,kv),bpebc(1:3,kv))
          ibpecorn(:,kv)=5
        else !Is Block}
          bpebc(8,kv)=1.0
        endif

        bpebc(9,kv)=t_magnets(kmag)%imat
        bpebc(10,kv)=t_magnets(kmag)%isspecial
        bn=norm2(br)

        if (bn.ne.0.0d0) bpebc(11:13,kv)=br/bn

        bpebc(14,kv)=bn
        bpebc(15,kv)=im
        bpebc(16,kv)=0.0
        bpebc(17,kv)=0.0

        ibpecol(kv)=t_magnets(kmag)%icol
        ibpeplan(kv)=tv%nface

        if (tv%IsBlock.gt.0) cycle

        k=1
        do n=1,tv%nface
          npoi=tv%kface(k)
          ibpecorn(n,kv)=npoi+1
          do i=1,npoi
            k=k+1
            ipoi=tv%kface(k)
            xyz=[tv%xhull(ipoi),tv%yhull(ipoi),tv%zhull(ipoi)]
            bpemag(1:3,i,n,kv)=xyz+bpebc(1:3,kv)
          enddo
          bpemag(1:3,npoi+1,n,kv)=bpemag(1:3,1,n,kv)
          k=k+1
        enddo

      enddo !voxel

      if (killbadmag.eq.-9999) then
        bpebc(16,:)=1.0d0
      else
        bpebc(16,:)=0.0d0
      endif

      if (killbadmag.gt.0.or.killbadmag.eq.-9999) then
        write(lun6,*)
        write(lun6,*)"Killing all voxels, but"
        open(newunit=lunkill,file="undumag.kll")
        do while (.true.)
          call util_read_line(lunkill,cline,last)
          if (last.le.0) exit
          read(cline,*)kmag,kill
          if (kmag.gt.nmag) then
            write(lun6,*)"*** Error in undumag_ini_magnets: Non existing voxel ",kmag," on undumag.kll ***"
            stop
          endif
          if (killbadmag.ne.-9999) then
            bpebc(16,kmag)=kill
            write(lun6,*)"Killing voxel ",kmag," due to flag killbadmag"
          else
            write(lun6,*)kmag
            bpebc(16,kmag)=0.0d0
          endif
        enddo
        close(lunkill)
        write(lun6,*)
      endif

      if (intmaglis.ne.0) then
        write(lun6,*)
        write(lun6,*)" --- Due to flag intmaglis, only magnets on file undumag_magmap.lis do contribute to field map ---"
        write(lun6,*)
        open(newunit=lunlis,file="undumag_magmap.lis")
        do while (.true.)
          call util_skip_comment_end(lunlis,ieof)
          if (ieof.ne.0) exit
          read(lunlis,*)chsel,bpe17
          do iv=1,nvoxcopy_t
            kmag=t_voxcopy(iv)%kproto
            if (t_magnets(kmag)%cmoth.eq.chsel) then
              if (bpe17.gt.0.0d0) then
                bpebc(17,iv)=1.0d0
              else if (bpe17.lt.0.0d0) then
                bpebc(17,iv)=2.0d0
              endif
            endif
          enddo
        enddo
        close(lunlis)
      endif !intmaglis

      return
      end
