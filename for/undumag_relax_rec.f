*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  08.59.21  by  Michael Scheer
*CMZ :  1.23/02 31/08/2017  12.21.55  by  Michael Scheer
*CMZ :  1.16/00 07/05/2017  13.42.22  by  Michael Scheer
*CMZ :  1.15/01 28/03/2017  14.14.30  by  Michael Scheer
*CMZ :  1.15/00 23/03/2017  17.02.57  by  Michael Scheer
*CMZ :  1.14/00 21/03/2017  16.57.45  by  Michael Scheer
*CMZ :  1.13/03 16/03/2017  09.36.56  by  Michael Scheer
*CMZ :  1.13/02 09/03/2017  16.17.07  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  10.40.13  by  Michael Scheer
*CMZ :  1.10/02 25/11/2016  10.03.38  by  Michael Scheer
*CMZ :  1.07/00 22/09/2016  11.23.13  by  Michael Scheer
*CMZ :  1.05/00 17/09/2016  10.31.45  by  Michael Scheer
*CMZ :  1.04/02 15/09/2016  15.11.30  by  Michael Scheer
*CMZ :  1.04/01 14/09/2016  14.32.50  by  Michael Scheer
*CMZ :  0.00/13 28/07/2016  12.39.02  by  Michael Scheer
*CMZ :  0.00/12 21/07/2016  09.04.42  by  Michael Scheer
*CMZ :  0.00/09 25/06/2016  14.02.31  by  Michael Scheer
*CMZ :  0.00/07 22/06/2016  14.20.00  by  Michael Scheer
*CMZ :  0.00/06 22/06/2016  13.40.01  by  Michael Scheer
*CMZ :  0.00/04 13/05/2016  14.50.43  by  Michael Scheer
*CMZ :  0.00/02 30/04/2016  13.06.46  by  Michael Scheer
*CMZ :  1.17/08 04/04/2016  08.57.15  by  Michael Scheer
*CMZ :  1.17/07 04/04/2016  08.47.36  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  12.58.26  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  12.18.50  by  Michael Scheer
*CMZ :  1.17/04 24/03/2016  18.46.54  by  Michael Scheer
*CMZ :  1.17/02 11/03/2016  16.48.04  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2016
      subroutine undumag_relax_rec

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m

!      bpebc(1:3,imag) contains position of magnet imag
!      bpebc(4:6,imag) contains normalized magnetization vector of magnet imag

      implicit none

      double precision bn,bx,by,bz,hx,hy,hz,bc(3),hpar,hper,parmag,permag,
     &  x,y,z,bcrot(3),hrot(3),hh(3),easy(3),hzmax,hzmin,bcint,dbcdh,
     &  h,hold,ht(3),vnormlab(3),vmaglab(3),hxtest,hytest,hztest,htest

      integer ifail,imoth,imag,iter,i,j,mat,matmode,ihz,nhz,mapmode,ical,iplan

      data ical/0/

      kconvrec=0
      h=0.0d0

      hxtest=0.0d0
      hytest=0.0d0
      hztest=0.0d0
      htest=0.0d0
      kinside=-1

      if (kdumpconv.ne.0) then
        do imag=1,nrec
          imoth=bpebc(15,imag)
          if (matrix.eq.0) then
            x=bpebc(1,imag)/1000.0d0
            y=bpebc(2,imag)/1000.0d0
            z=bpebc(3,imag)/1000.0d0
            call undumag_field(x,y,z,hx,hy,hz,ifail)
          else
            call undumag_bpolyeder_matrix(imag,hx,hy,hz,ifail)
          endif
          write(lunconv,*)kiter,iterrectot,imoth,imag,
     &      x*1000.0d0,y*1000.0d0,z*1000.0d0,
     &        hx,hy,hz,sqrt(hx**2+hy**2+hz**2)
     &      ,hxtest,hytest,hztest,htest,
     &      bpebc(4:6,imag),
     &      sqrt(bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
        enddo
      endif

      do iter=1,maxiterrec

        iterrectot=iterrectot+1

        bc0(4:6,1:nrec)=bpebc(4:6,1:nrec)

        do imag=1,nrec

          mat=nint(bpebc(9,imag))
          easy=bpebc(11:13,imag)

          mapmode=matmaps(3,mat)
          nhz=matmaps(4,mat)

          if (matrix.eq.0) then
            x=bpebc(1,imag)/1000.0d0
            y=bpebc(2,imag)/1000.0d0
            z=bpebc(3,imag)/1000.0d0
            ! bpolyeder returns H, if inside magnet!
            kinside=-1
            call undumag_field(x,y,z,hh(1),hh(2),hh(3),ifail)
          else
            call undumag_bpolyeder_matrix(imag,hh(1),hh(2),hh(3),ifail)
          endif

          hpar=hh(1)*easy(1)+hh(2)*easy(2)+hh(3)*easy(3)
          hrot=hh-hpar*easy

          if (mapmode.eq.1) then
            ! equidistant map
            parksi=bcmat(2,1,mat)
            perksi=bcmat(3,1,mat)
            bc0(4:6,imag)=hrot*perksi+(bc0(10,imag)+hpar*parksi)*easy
          else if (mapmode.eq.2) then
            ! equidistant map
            perksi=bcmat(3,1,mat)
            hzmin=bcmat(1,1,mat)
            hzmax=bcmat(1,nhz,mat)
            if (hpar.ge.hzmin.and.hpar.le.hzmax) then
              if (nhz.gt.1) then
                ihz=int((hpar-hzmin)/((hzmax-hzmin)/(nhz-1)))+1
                ihz=min(ihz,nhz-1)
                if (ihz.lt.nhz) then
                  bc0(4:6,imag)=hrot*perksi+
     &              (bcmat(2,ihz,mat)+(bcmat(2,ihz+1,mat)-bcmat(2,ihz,mat))/
     &              (bcmat(1,ihz+1,mat)-bcmat(1,ihz,mat))*
     &              (hpar-bcmat(1,ihz,mat)))*easy
                else
                  bc0(4:6,imag)=hrot*perksi+
     &              (bcmat(2,ihz-1,mat)+(bcmat(2,ihz,mat)-bcmat(2,ihz-1,mat))/
     &              (bcmat(1,ihz,mat)-bcmat(1,ihz-1,mat))*
     &              (hpar-bcmat(1,ihz-1,mat)))*easy
                endif
              else
                write(lun6,*)"*** Error in undumag_relax_rec: Only one data point for mat:",mat
                stop
              endif
            else
              write(lun6,*)"*** Error in undumag_relax_rec: H out of range, mat, H:",mat,hpar
              stop
            endif
          else
            write(lun6,*)"*** Error in undumag_relax_rec: Unknown material mode:",mapmode
            stop
          endif !mapmode

        enddo !nrec

        do imag=1,nrec

          if (
     &        bc0(4,imag).ne.bc0(4,imag)
     &        .or.
     &        bc0(5,imag).ne.bc0(5,imag)
     &        .or.
     &        bc0(6,imag).ne.bc0(6,imag)
     &        ) then
            write(lun6,*)"*** Warning in undumag_relax_rec: Magnetization bpebc is not a number ***"
            write(lun6,*)
     &        "kiter,iterrectot,imag,x,y,z:",
     &        kiter,iterrectot,imag,bpebc(1:3,imag)
            write(lun6,*)"Previous values of magnetization kept:"
            write(lun6,*)bpebc(4:6,imag)
          else
            bpebc(4:6,imag)=bc0(4:6,imag)
            bpebc(7,imag)=sqrt(
     &        bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
          endif
          vmaglab(1:3)=bpebc(4:6,imag)
          do iplan=1,iabs(ibpeplan(imag))
            vnormlab(1:3)=bpetm(1:3,8,iplan,imag)
            bpetm(1,7,iplan,imag)=
     &        vmaglab(1)*vnormlab(1)+
     &        vmaglab(2)*vnormlab(2)+
     &        vmaglab(3)*vnormlab(3)
          enddo
        enddo

        niterrec=iter

        if (kdumpconv.ne.0) then
          imoth=bpebc(15,imag)
          hxtest=0.0d0
          hytest=0.0d0
          hztest=0.0d0
          htest=0.0d0
          do imag=1,nrec
            if (matrix.eq.0) then
              x=bpebc(1,imag)/1000.0d0
              y=bpebc(2,imag)/1000.0d0
              z=bpebc(3,imag)/1000.0d0
              call undumag_field(x,y,z,hx,hy,hz,ifail)
            else
              call undumag_bpolyeder_matrix(imag,hx,hy,hz,ifail)
            endif
            write(lunconv,*)kiter,iterrectot,imoth,imag,
     &        x*1000.0d0,y*1000.0d0,z*1000.0d0,
     &        hx,hy,hz,sqrt(hx**2+hy**2+hz**2)
     &        ,hxtest,hytest,hztest,htest,
     &        bpebc(4:6,imag),
     &        sqrt(bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)
          enddo
        endif

        kinside=0
        hold=h
        h=0.0d0

        if (maxiterrec.gt.1) then
          call undumag_bconv(h)
          if (hold.ne.0.0d0) then
            if (abs(h/hold-1.0d0).lt.hconva) then
              kconvrec=1
              goto 9999
            endif
          endif
        endif

      enddo !iter

9999  continue

      ical=1

      return
      end
