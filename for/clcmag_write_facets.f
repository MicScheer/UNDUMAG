*CMZ :  2.04/20 19/09/2023  14.42.36  by  Michael Scheer
*CMZ :  2.04/17 13/09/2023  16.11.53  by  Michael Scheer
*CMZ :  2.04/16 11/09/2023  11.54.29  by  Michael Scheer
*CMZ :  2.04/14 05/09/2023  09.52.46  by  Michael Scheer
*CMZ :  2.04/13 03/09/2023  20.28.53  by  Michael Scheer
*CMZ :  2.04/09 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  12.58.25  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  12.43.30  by  Michael Scheer
*CMZ :  2.04/06 04/08/2023  11.32.02  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  19.30.13  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  20.28.35  by  Michael Scheer
*CMZ :  2.04/00 09/12/2022  11.08.58  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_write_facets

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      Type(T_Magnet) tmag
      Type(T_Voxel) tv

      double precision, dimension(:,:), allocatable :: buff
      integer, dimension(:), allocatable :: ibuff
      character(2048), dimension(:), allocatable :: cbuff

      double precision gcen(3)
      integer imag,luno,npoi,iv,i,imago,ivo,iface,l,j,nf,lin,ivc,kcopy,ib
      integer :: idebug=0

      allocate(ibuff(8*nvoxcopy_t*ncornmax),buff(3,8*nvoxcopy_t*ncornmax),
     &  cbuff(8*nvoxcopy_t*ncornmax))

      write(lun6,*)
      write(lun6,*)'Writing faces to undumag_facets.fct'

      imago=0
      ivo=0
      lin=0
      nfacets=0

      do ivc=1,nvoxcopy_t

        gcen=t_voxcopy(ivc)%gcen
        kcopy=t_voxcopy(ivc)%kcopy

        do iface=1,t_voxcopy(ivc)%nface

          npoi=t_voxcopy(ivc)%isfacet(iface)
          if (npoi.eq.0) cycle

          nfacets=nfacets+1
          imag=t_voxcopy(ivc)%kproto
          iv=t_voxcopy(ivc)%kvoxel
          ibuff(nfacets)=npoi

          if (imag.ne.imago) then
            tmag=t_magnets(imag)
            ivo=0
          endif

          if (iv.ne.ivo) then
            tv=tmag%t_voxels(iv)
            ivo=0
          endif

          write(cbuff(nfacets),*)npoi,tmag%icol,tmag%icol,iv,kcopy,imag,
     &      tmag%cnam,tmag%cmoth

          do j=1,npoi
            l=tv%kface(tv%lface(iface)+j)
            lin=lin+1
            buff(:,lin)=
     &        [tv%xhull(l)+gcen(1),
     &        tv%yhull(l)+gcen(2),
     &        tv%zhull(l)+gcen(3)]
            if (idebug.ne.0) then
              write(66,*)tmag%kmag,ivc,iface,nfacets,lin,buff(:,lin)
            endif
          enddo

          imago=imag
          ivo=iv

        enddo !ifac

        ivo=iv
        imago=imag

      enddo !nvoxcopy_t

      open(newunit=luno,file='undumag_facets.fct')

      nf=nfacets
      if(izsym.ne.0) nf=nf*2

      write(luno,*) nf

      lin=0
      do i=1,nfacets
        write(luno,*)trim(cbuff(i))
        do l=1,ibuff(i)
          lin=lin+1
          write(luno,*)buff(:,lin)
        enddo
      enddo

      if (izsym.ne.0)  then
        lin=0
        do i=1,nfacets
          write(luno,*)trim(cbuff(i))
          do l=1,ibuff(i)
            lin=lin+1
            write(luno,*)buff(1,lin),buff(2,lin),-buff(3,lin)
          enddo
        enddo
      endif

      if (iysym.ne.0) then

        if (idebug.ne.0) then
          flush(luno)
          close(luno)
          open(newunit=luno,file='undumag_facets.fct')
        else
          rewind(luno)
        endif

        read(luno,*) nf
        lin=0
        ib=0
        do i=1,nf
          read(luno,'(a)',end=91) cbuff(i)
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            read(luno,*,end=91)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo

91      if (idebug.ne.0) then
          print*,i,lin
          !stop "91"
          flush(luno)
          close(luno)
          open(newunit=luno,file='undumag_facets.fct')
        else
          rewind(luno)
        endif

        lin=0
        write(luno,*) 2*nf
        ib=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            write(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        lin=0
        ib=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            write(luno,*)buff(1,lin),-buff(2,lin),buff(3,lin)
          enddo
        enddo
      endif

      if (ixsym.ne.0) then
        if (idebug.ne.0) then
          flush(luno)
          close(luno)
          open(newunit=luno,file='undumag_facets.fct')
        else
          rewind(luno)
        endif
        lin=0
        read(luno,*) nf
        ib=0
        do i=1,nf
          read(luno,'(a)',end=92) cbuff(i)
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            read(luno,*,end=92)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
92      if (idebug.ne.0) then
          flush(luno)
          close(luno)
          print*,i,lin
          !stop "92"
          open(newunit=luno,file='undumag_facets.fct')
        else
          rewind(luno)
        endif
        write(luno,*) 2*nf
        lin=0
        ib=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            write(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        lin=0
        ib=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          ib=ib+1
          if (ib.gt.nfacets) ib=1
          do l=1,ibuff(ib)
            lin=lin+1
            write(luno,*)-buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
      endif

      flush(luno)
      close(luno)

      write(lun6,*)
      write(lun6,*)'Done'

      deallocate(buff,cbuff,ibuff)

      return
      end
