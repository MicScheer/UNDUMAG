*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.11/05 07/02/2017  14.49.12  by  Michael Scheer
*CMZ :  1.17/14 11/04/2016  13.07.23  by  Michael Scheer
*-- Author :    Michael Scheer   11/04/2016
      subroutine undumag_cut_plane(
     &  nin,xin,yin,zin,
     &  n1,x1,y1,z1,
     &  n2,x2,y2,z2
     &  ,mode,cut,
     &  ifail)

      use commandlinef90m

      implicit none

      double precision
     &  xin(nin),yin(nin),zin(nin),
     &  x1(n1),y1(n1),z1(n1), x2(n2),y2(n2),z2(n2),cut

      double precision :: tol=1.0d-6

      integer nin,mode,ifail,i1,i2,n2,n1,ii,kcut(4),ncut,i,n

      ifail=0

      if (mode.eq.1) then

        ! x-cut, i.e. normal vector of cutting plane is (1,0,0)

        n2=0
        n1=0

        ncut=0
        do i=1,nin
          if (xin(i).ge.cut-tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n2=nin
          x2(1:nin)=xin(1:nin)
          y2(1:nin)=yin(1:nin)
          z2(1:nin)=zin(1:nin)
          goto 9999
        endif

        ncut=0
        do i=1,nin
          if (xin(i).le.cut+tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n1=nin
          x1(1:nin)=xin(1:nin)
          y1(1:nin)=yin(1:nin)
          z1(1:nin)=zin(1:nin)
          goto 9999
        endif

        do i=1,nin

          i1=i
          i2=i1+1
          if (i2.gt.nin) i2=1

          if (abs(xin(i1)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else if (abs(xin(i2)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i2)
            y1(n1)=yin(i2)
            z1(n1)=zin(i2)

            n2=n2+1
            x2(n2)=xin(i2)
            y2(n2)=yin(i2)
            z2(n2)=zin(i2)

          else if (xin(i1).lt.cut.and.xin(i2).gt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n1=n1+1
            x1(n1)=cut
            y1(n1)=yin(i1)+(yin(i2)-yin(i1))/(xin(i2)-xin(i1))*(cut-xin(i1))
            z1(n1)=zin(i1)+(zin(i2)-zin(i1))/(xin(i2)-xin(i1))*(cut-xin(i1))

            n2=n2+1
            x2(n2)=cut
            y2(n2)=y1(n1)
            z2(n2)=z1(n1)

          else if (xin(i1).gt.cut.and.xin(i2).lt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

            n2=n2+1
            x2(n2)=cut
            y2(n2)=yin(i1)+(yin(i2)-yin(i1))/(xin(i2)-xin(i1))*(cut-xin(i1))
            z2(n2)=zin(i1)+(zin(i2)-zin(i1))/(xin(i2)-xin(i1))*(cut-xin(i1))

            n1=n1+1
            x1(n1)=cut
            y1(n1)=y2(n2)
            z1(n1)=z2(n2)

          else if (xin(i1).lt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

          else if (xin(i1).gt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else

            write(lun6,*)"*** Error in undumag_cut_plane:: ipoi,x,y,z:",
     &        i1,xin(i1),yin(i1),zin(i1)

          endif

        enddo !nin

      else if (mode.eq.2) then

        ! y-cut, i.e. normal vector of cutting plane is (0,1,0)

        n2=0
        n1=0

        ncut=0
        do i=1,nin
          if (yin(i).ge.cut-tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n2=nin
          x2(1:nin)=xin(1:nin)
          y2(1:nin)=yin(1:nin)
          z2(1:nin)=zin(1:nin)
          goto 9999
        endif

        ncut=0
        do i=1,nin
          if (yin(i).le.cut+tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n1=nin
          x1(1:nin)=xin(1:nin)
          y1(1:nin)=yin(1:nin)
          z1(1:nin)=zin(1:nin)
          goto 9999
        endif

        do i=1,nin

          i1=i
          i2=i1+1
          if (i2.gt.nin) i2=1

          if (abs(yin(i1)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else if (abs(yin(i2)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i2)
            y1(n1)=yin(i2)
            z1(n1)=zin(i2)

            n2=n2+1
            x2(n2)=xin(i2)
            y2(n2)=yin(i2)
            z2(n2)=zin(i2)

          else if (yin(i1).lt.cut.and.yin(i2).gt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n1=n1+1
            x1(n1)=xin(i1)+(xin(i2)-xin(i1))/(yin(i2)-yin(i1))*(cut-yin(i1))
            y1(n1)=cut
            z1(n1)=zin(i1)+(zin(i2)-zin(i1))/(yin(i2)-yin(i1))*(cut-yin(i1))

            n2=n2+1
            x2(n2)=x1(n1)
            y2(n2)=y1(n1)
            z2(n2)=z1(n1)

          else if (yin(i1).gt.cut.and.yin(i2).lt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

            n2=n2+1
            x2(n2)=xin(i1)+(xin(i2)-xin(i1))/(yin(i2)-yin(i1))*(cut-yin(i1))
            y2(n2)=cut
            z2(n2)=zin(i1)+(zin(i2)-zin(i1))/(yin(i2)-yin(i1))*(cut-yin(i1))

            n1=n1+1
            x1(n1)=x2(n2)
            y1(n1)=y2(n2)
            z1(n1)=z2(n2)

          else if (yin(i1).lt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

          else if (yin(i1).gt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else

            write(lun6,*)"*** Error in undumag_cut_plane:: ipoi,x,y,z:",
     &        i1,xin(i1),yin(i1),zin(i1)

          endif

        enddo !nin

      else if (mode.eq.3) then

        ! z-cut, i.e. normal vector of cutting plane is (0,0,1)

        n2=0
        n1=0

        ncut=0
        do i=1,nin
          if (zin(i).ge.cut-tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n2=nin
          x2(1:nin)=xin(1:nin)
          y2(1:nin)=yin(1:nin)
          z2(1:nin)=zin(1:nin)
          goto 9999
        endif

        ncut=0
        do i=1,nin
          if (zin(i).le.cut+tol) ncut=ncut+1
        enddo !n1
        if (ncut.eq.nin) then
          n1=nin
          x1(1:nin)=xin(1:nin)
          y1(1:nin)=yin(1:nin)
          z1(1:nin)=zin(1:nin)
          goto 9999
        endif

        do i=1,nin

          i1=i
          i2=i1+1
          if (i2.gt.nin) i2=1

          if (abs(zin(i1)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else if (abs(zin(i2)-cut).lt.tol) then

            n1=n1+1
            x1(n1)=xin(i2)
            y1(n1)=yin(i2)
            z1(n1)=zin(i2)

            n2=n2+1
            x2(n2)=xin(i2)
            y2(n2)=yin(i2)
            z2(n2)=zin(i2)

          else if (zin(i1).lt.cut.and.zin(i2).gt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

            n1=n1+1
            x1(n1)=xin(i1)+(xin(i2)-xin(i1))/(zin(i2)-zin(i1))*(cut-zin(i1))
            y1(n1)=yin(i1)+(yin(i2)-yin(i1))/(zin(i2)-zin(i1))*(cut-zin(i1))
            z1(n1)=cut

            n2=n2+1
            x2(n2)=x1(n1)
            y2(n2)=y1(n1)
            z2(n2)=z1(n1)

          else if (zin(i1).gt.cut.and.zin(i2).lt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

            n2=n2+1
            x2(n2)=xin(i1)+(xin(i2)-xin(i1))/(zin(i2)-zin(i1))*(cut-zin(i1))
            y2(n2)=yin(i1)+(yin(i2)-yin(i1))/(zin(i2)-zin(i1))*(cut-zin(i1))
            z2(n2)=cut

            n1=n1+1
            x1(n1)=x2(n2)
            y1(n1)=y2(n2)
            z1(n1)=z2(n2)

          else if (zin(i1).lt.cut) then

            n1=n1+1
            x1(n1)=xin(i1)
            y1(n1)=yin(i1)
            z1(n1)=zin(i1)

          else if (zin(i1).gt.cut) then

            n2=n2+1
            x2(n2)=xin(i1)
            y2(n2)=yin(i1)
            z2(n2)=zin(i1)

          else

            write(lun6,*)"*** Error in undumag_cut_plane:: ipoi,x,y,z:",
     &        i1,xin(i1),yin(i1),zin(i1)

          endif

        enddo !nin

      else
        ifail=1
        goto 9999
      endif !mode

9999  continue

      call util_weed_points(n1,x1,y1,z1,tol)
      call util_weed_points(n2,x2,y2,z2,tol)

      return
      end
