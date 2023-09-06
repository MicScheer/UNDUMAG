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
      character(128), dimension(:), allocatable :: cbuff
      integer imag,luno,npoi,iv,i,imago,ivo,iface,l,lpoi,j,ll,nf,lin

      allocate(ibuff(8*nfacets),buff(3,8*nfacets*ncornmax),cbuff(8*nfacets))

      write(lun6,*)
      write(lun6,*)'Writing faces to undumag_facets.fct'

      imago=0
      ivo=0
      lin=0

      do i=1,nfacets

        imag=ifacets(1,i)
        iv=ifacets(2,i)
        iface=ifacets(3,i)
        lpoi=ifacets(4,i)
        npoi=ifacets(5,i)

        ibuff(i)=npoi
        do j=1,7
          ibuff(i+j*nfacets)=npoi
        enddo

        if (imag.ne.imago) then
          tmag=t_magnets(imag)
          ivo=0
        endif

        if (iv.ne.ivo) then
          tv=tmag%t_voxels(iv)
          ivo=0
        endif

c        write(luno,*)npoi,tmag%icol,tmag%icol,iv,imag,tmag%cnam,tmag%cmoth
        write(cbuff(i),*)npoi,tmag%icol,tmag%icol,iv,imag,tmag%cnam,tmag%cmoth

        do j=1,npoi
          ll=lpoi+j
          l=tv%kface(ll)
c          write(luno,*)
c     &      tv%xhull(l)+tv%gcen(1),
c     &      tv%yhull(l)+tv%gcen(2),
c     &      tv%zhull(l)+tv%gcen(3)
          lin=lin+1
          buff(:,lin)=
     &      [tv%xhull(l)+tv%gcen(1),
     &      tv%yhull(l)+tv%gcen(2),
     &      tv%zhull(l)+tv%gcen(3)]
        enddo

        ivo=iv
        imago=imag

      enddo

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
        rewind(luno)
        read(luno,*) nf
        lin=0
        do i=1,nf
          read(luno,'(a)') cbuff(i)
          do l=1,ibuff(i)
            lin=lin+1
            read(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        rewind(luno)
        lin=0
        write(luno,*) 2*nf
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          do l=1,ibuff(i)
            lin=lin+1
            write(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        lin=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          do l=1,ibuff(i)
            lin=lin+1
            write(luno,*)buff(1,lin),-buff(2,lin),buff(3,lin)
          enddo
        enddo
      endif

      if (ixsym.ne.0) then
        rewind(luno)
        lin=0
        read(luno,*) nf
        do i=1,nf
          read(luno,'(a)') cbuff(i)
          do l=1,ibuff(i)
            lin=lin+1
            read(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        rewind(luno)
        write(luno,*) 2*nf
        lin=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          do l=1,ibuff(i)
            lin=lin+1
            write(luno,*)buff(1,lin),buff(2,lin),buff(3,lin)
          enddo
        enddo
        lin=0
        do i=1,nf
          write(luno,*)trim(cbuff(i))
          do l=1,ibuff(i)
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
