*CMZ :  2.04/14 05/09/2023  10.58.39  by  Michael Scheer
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
      subroutine clcmag_magnet_main_facets

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      double precision, dimension(:,:), allocatable :: buff
      integer, dimension(:), allocatable :: ibuff
      character(128), dimension(:), allocatable :: cbuff
      integer imag,luno,npoi,iv,i,l,j,nf,lin,mmag,nface,k

      mmag=nmag_t+nspecmag_t

      allocate(ibuff(8*mmag*nplanmax),buff(3,8*mmag*nplanmax*ncornmax),
     &  cbuff(8*mmag*nplanmax))

      write(lun6,*)
      write(lun6,*)'Writing faces to undumag_main_facets.fct'

      lin=0
      nface=0

      do imag=1,mmag
        k=1
        do l=1,t_magnets(imag)%kfacelast
          npoi=t_magnets(imag)%kface(k)
          nface=nface+1
          ibuff(nface)=npoi
          write(cbuff(nface),*)
     &      npoi,
     &      t_magnets(imag)%icol,t_magnets(imag)%icol,iv,
     &      imag,t_magnets(imag)%cnam,t_magnets(imag)%cmoth
          do j=1,npoi
            k=k+1
            i=t_magnets(imag)%kface(k)
            lin=lin+1
            buff(:,lin)=
     &        [t_magnets(imag)%xhull(i)+t_magnets(imag)%gcen(1),
     &        t_magnets(imag)%yhull(i)+t_magnets(imag)%gcen(2),
     &        t_magnets(imag)%zhull(i)+t_magnets(imag)%gcen(3)]
          enddo
          k=k+1
          if (k.gt.t_magnets(imag)%kfacelast) exit
        enddo
      enddo

      do k=1,nface
        do j=1,7
          ibuff(k+j*nface)=ibuff(k)
        enddo
      enddo

      open(newunit=luno,file='undumag_main_facets.fct')

      nf=nface
      if(izsym.ne.0) nf=nf*2

      write(luno,*) nf

      lin=0
      do i=1,nface
        write(luno,*)trim(cbuff(i))
        do l=1,ibuff(i)
          lin=lin+1
          write(luno,*)buff(:,lin)
        enddo
      enddo

      if (izsym.ne.0)  then
        lin=0
        do i=1,nface
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
