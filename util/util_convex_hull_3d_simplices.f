*CMZ :  2.05/02 30/10/2023  09.34.16  by  Michael Scheer
*CMZ :  2.04/17 11/09/2023  14.37.58  by  Michael Scheer
*CMZ :  2.04/16 10/09/2023  19.49.45  by  Michael Scheer
*CMZ :  2.02/02 05/07/2022  11.16.26  by  Michael Scheer
*CMZ :  2.02/01 03/11/2021  13.16.39  by  Michael Scheer
*CMZ :  2.02/00 17/09/2020  13.46.21  by  Michael Scheer
*CMZ :  2.01/03 28/05/2019  15.59.50  by  Michael Scheer
*CMZ :  2.00/03 24/04/2018  09.24.03  by  Michael Scheer
*CMZ :  1.23/04 02/10/2017  11.25.14  by  Michael Scheer
*CMZ : 00.00/21 13/03/2017  11.19.41  by  Michael Scheer
*CMZ : 00.00/20 12/03/2017  13.22.16  by  Michael Scheer
*CMZ :  1.11/05 20/02/2017  22.15.13  by  Michael Scheer
*CMZ :  1.11/04 21/01/2017  17.14.54  by  Michael Scheer
*-- Author :    Michael Scheer   22/11/2016
      subroutine util_convex_hull_3d_simplices(nin,xin,yin,zin,tiny,nsimp,ksimp,vn,kfail)

      implicit none

      double precision xin(*),yin(*),zin(*),vn(3,*),vnn,tiny
      double precision p1(3),p2(3),p3(3),vnor(3),cen(3),p(3),gcen(3),
     &  p32(3),p21(3),dist

      integer ksimp(*)
      integer nin,kfail,nsimp,klast,i1,i2,i3,i,ifound,iover,istat,ipos,ineg

      nsimp=0
      klast=0

      kfail=1
      if (nin.lt.4) return

      gcen=0.0d0
      do i1=1,nin
        gcen=gcen+[xin(i1),yin(i1),zin(i1)]
      enddo
      gcen=gcen/dble(nin)

      do i1=1,nin
        p1=[xin(i1),yin(i1),zin(i1)]
        do i2=1,nin
          if (i2.eq.i1) cycle
          p2=[xin(i2),yin(i2),zin(i2)]
          do i3=1,nin
            if (i3.eq.i1.or.i3.eq.i2) cycle
            p3=[xin(i3),yin(i3),zin(i3)]
            cen=(p1+p2+p3)/3.0d0
            p21=p2-p1
            p32=p3-p2
            dist=0.0d0
            ipos=0
            ineg=0
            ifound=1
            do i=1,nin
              if (i.eq.i1.or.i.eq.i2.or.i.eq.i3) cycle
              p=[xin(i),yin(i),zin(i)]
              call util_plane_tiny(p1,p2,p3,p,vnor,dist,tiny,iover,istat)
              if (istat.ne.0) then
                ifound=0
                exit
              endif
              if (dot_product(cen-gcen,vnor).lt.0.0d0) then
                vnor=-vnor
              endif
              if (abs(dist).lt.tiny) dist=0.0d0
              if (dist.gt.0.0d0) then
                ipos=1
              else if (dist.lt.0.0d0) then
                ineg=1
              endif

              if (ipos.eq.1.and.ineg.eq.1) then
                ifound=0
                exit
              endif

            enddo
            if (ifound.ne.0) then
              vnn=norm2(vnor)
              if (vnn.eq.0.0d0) cycle
              vnor=vnor/vnn
              nsimp=nsimp+1
              vn(:,nsimp)=vnor
              klast=klast+1
              ksimp(klast)=3
              klast=klast+1
              ksimp(klast)=i1
              klast=klast+1
              ksimp(klast)=i2
              klast=klast+1
              ksimp(klast)=i3
            endif
          enddo !i3
        enddo !i2
      enddo !i1

      kfail=0

      return
      end
