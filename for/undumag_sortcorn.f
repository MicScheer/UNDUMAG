*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.14/00 17/03/2017  13.29.25  by  Michael Scheer
*CMZ :  1.13/03 13/03/2017  18.25.01  by  Michael Scheer
*CMZ :  1.13/01 03/03/2017  14.32.41  by  Michael Scheer
*CMZ :  1.10/00 09/11/2016  12.50.19  by  Michael Scheer
*CMZ :  1.03/00 12/09/2016  19.05.15  by  Michael Scheer
*CMZ :  1.02/03 12/09/2016  11.54.07  by  Michael Scheer
*CMZ :  1.02/01 11/09/2016  12.33.10  by  Michael Scheer
*-- Author :    Michael Scheer   11/09/2016
      subroutine undumag_sortcorn(ncornmax,nplanmax,nplan,ncorn1,corn1,corn2,
     &  tiny,ifail)

      use commandlinef90m

      implicit none

      double precision, dimension (:), allocatable :: x,y
      integer, dimension (:), allocatable :: ihull

      integer nplan,ifail,nplanmax,ncornmax
      double precision corn1(3,ncornmax,nplanmax),corn2(3,ncornmax,nplanmax),
     &  p1(3),p2(3),p3(3),p21(3),p32(3),pc(3),pcm,tiny
      integer ncorn1(ncornmax),iax,icorn,iplan,nh,kfail,ncorn

      allocate(x(ncornmax))
      allocate(y(ncornmax))
      allocate(ihull(ncornmax))

      do iplan=1,nplan

        ncorn=ncorn1(iplan)

c        do icorn=1,ncorn
c          write(lun6,*)"sort:",iplan,icorn,corn1(1:3,icorn,iplan)
c        enddo

        p1(1:3)=corn1(1:3,1,iplan)
        p2(1:3)=corn1(1:3,2,iplan)
        p3(1:3)=corn1(1:3,3,iplan)

        p21=p2-p1
        p32=p3-p2

        pc(1)=p21(2)*p32(3)-p21(3)*p32(2)
        pc(2)=p21(3)*p32(1)-p21(1)*p32(3)
        pc(3)=p21(1)*p32(2)-p21(2)*p32(1)

        pcm=0.0d0
        if (Abs(pc(1)).gt.pcm) then
          pcm=Abs(pc(1))
          iax=1
        endif
        if (Abs(pc(2)).gt.pcm) then
          pcm=Abs(pc(2))
          iax=2
        endif
        if (Abs(pc(3)).gt.pcm) then
          pcm=Abs(pc(3))
          iax=3
        endif

        if (iax.eq.1) then
          do icorn=1,ncorn
            x(icorn)=corn1(2,icorn,iplan)
            y(icorn)=corn1(3,icorn,iplan)
          enddo
        else if (iax.eq.2) then
          do icorn=1,ncorn
            x(icorn)=corn1(3,icorn,iplan)
            y(icorn)=corn1(1,icorn,iplan)
          enddo
        else if (iax.eq.3) then
          do icorn=1,ncorn
            x(icorn)=corn1(1,icorn,iplan)
            y(icorn)=corn1(2,icorn,iplan)
          enddo
        endif !iax

        call util_convex_hull_2d(ncorn,x,y,nh,ihull,tiny,kfail)
        ifail=ifail+kfail
        if (nh.ne.ncorn) then
          ifail=ifail+1
          return
        endif

        p1(1:3)=corn1(1:3,ihull(1),iplan)
        p2(1:3)=corn1(1:3,ihull(2),iplan)
        p3(1:3)=corn1(1:3,ihull(3),iplan)

        p21=p2-p1
        p32=p3-p2

        pc(1)=p21(2)*p32(3)-p21(3)*p32(2)
        pc(2)=p21(3)*p32(1)-p21(1)*p32(3)
        pc(3)=p21(1)*p32(2)-p21(2)*p32(1)

        if (p1(1)*pc(1)+p1(2)*pc(2)+p1(3)*pc(3).gt.0.0d0) then
          do icorn=1,ncorn
            corn2(1:3,icorn,iplan)=corn1(1:3,ihull(icorn),iplan)
          enddo
        else if (p1(1)*pc(1)+p1(2)*pc(2)+p1(3)*pc(3).lt.0.0d0) then
          do icorn=1,ncorn
            corn2(1:3,ncorn+1-icorn,iplan)=corn1(1:3,ihull(icorn),iplan)
          enddo
        else
          ifail=ifail+1
          return
        endif

      enddo !iplan

      deallocate(x)
      deallocate(y)
      deallocate(ihull)

      return
      end
