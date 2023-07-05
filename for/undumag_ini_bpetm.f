*CMZ :  2.04/03 02/03/2023  17.33.47  by  Michael Scheer
*CMZ :  2.04/02 24/02/2023  17.30.41  by  Michael Scheer
*CMZ :  2.02/01 28/12/2021  11.43.51  by  Michael Scheer
*-- Author :    Michael Scheer   01/04/2016
      subroutine undumag_ini_bpetm

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

*KEEP,random.
      include 'random.cmn'
*KEND.

      double precision rmag(3),vnormlab(3),r1(3),r1lab(3),vmaglab(3),
     &  p1(3),p2(3),p3(3),ts(3,3),tsinv(3,3),tz(3,3),ws(3,3),vx,vy,vn,
     &  ts1(3,3),ts1inv(3,3),sa,dum,ca,bc,q,qsign,r2(3),a,b

      integer im,i,iplan,nplan,j,icorn,ifail,ip2,imag
      integer :: idebug=0

      nmag=nvoxcopy_t


      do im=1,nmag
c center of magnet in lab

        rmag(1)=bpebc(1,im)
        rmag(2)=bpebc(2,im)
        rmag(3)=bpebc(3,im)

c magnetization vector in lab

        vmaglab(1)=bpebc(4,im)
        vmaglab(2)=bpebc(5,im)
        vmaglab(3)=bpebc(6,im)

        bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)

c        write(lun6,*)"im,rmag:",im,rmag

        nplan=ibpeplan(im)
        do iplan=1,nplan

c three points defining plane (lab.-system)

          p1(1)=bpemag(1,1,iplan,im)
          p1(2)=bpemag(2,1,iplan,im)
          p1(3)=bpemag(3,1,iplan,im)

          p2(1)=bpemag(1,2,iplan,im)
          p2(2)=bpemag(2,2,iplan,im)
          p2(3)=bpemag(3,2,iplan,im)

          p3(1)=bpemag(1,3,iplan,im)
          p3(2)=bpemag(2,3,iplan,im)
          p3(3)=bpemag(3,3,iplan,im)

          call undumag_bpen(im,iplan,p1,p2,p3,vnormlab,ifail)

          if (ifail.ne.0) then
            write(lun6,*)"*** Error 13 in undumag_ini_bpetm: Failure in undumag_bpen, mag, plane:",
     &        im,iplan
            stop
          endif

c check if normal vector is perpendicular to magnetization vector
c if mag. vector is parallel, skip plane

          if (bc.ne.0.0d0) then
            dum=abs(
     &        (vnormlab(1)*vmaglab(1)+vnormlab(2)*vmaglab(2)+
     &        vnormlab(3)*vmaglab(3))
     &        /bc
     &        )
          else
            dum=0.0d0
          endif

            bpetm(1,7,iplan,im)=
     &        vmaglab(1)*vnormlab(1)+
     &        vmaglab(2)*vnormlab(2)+
     &        vmaglab(3)*vnormlab(3)
            bpetm(1,8,iplan,im)=vnormlab(1)
            bpetm(2,8,iplan,im)=vnormlab(2)
            bpetm(3,8,iplan,im)=vnormlab(3)

c get matrices ts and tsinv. Ts transforms vnormlab to (0,0,1)

            call undumag_bpet(vnormlab,ts,tsinv)

            if (bpebc(8,im).eq.-6) then

c for rectangular magnets, we rotate the plans such, that the edges coinside
c with the axis of the coord.-system.

c All planes are rotated to the system of the
c first plane

              if (iplan.eq.1) then
                ts1=ts
                ts1inv=tsinv
              else
                ts=ts1
                tsinv=ts1inv
              endif !(iplan.eq.1)

              do icorn=1,5

                r1lab(1)=bpemag(1,icorn,iplan,im)
                r1lab(2)=bpemag(2,icorn,iplan,im)
                r1lab(3)=bpemag(3,icorn,iplan,im)

                r1(1)=ts(1,1)*r1lab(1)+ts(1,2)*r1lab(2)+ts(1,3)*r1lab(3)
                r1(2)=ts(2,1)*r1lab(1)+ts(2,2)*r1lab(2)+ts(2,3)*r1lab(3)
                r1(3)=ts(3,1)*r1lab(1)+ts(3,2)*r1lab(2)+ts(3,3)*r1lab(3)

                bperot(1,icorn,iplan,im)=r1(1)
                bperot(2,icorn,iplan,im)=r1(2)
                bperot(3,icorn,iplan,im)=r1(3)

              enddo !icorn=1,ncorn

              vx=bperot(1,2,iplan,im)-bperot(1,1,iplan,im)
              vy=bperot(2,2,iplan,im)-bperot(2,1,iplan,im)
              vn=sqrt(vx*vx+vy*vy)

              sa=vy/vn
              ca=vx/vn

              tz=ts

              ts(1,1)=ca
              ts(1,2)=sa
              ts(1,3)=0.0d0

              ts(2,1)=-sa
              ts(2,2)=ca
              ts(2,3)=0.0d0

              ts(3,1)=0.0d0
              ts(3,2)=0.0d0
              ts(3,3)=1.0d0

              call util_matrix_multiplication(3,3,3,ts,tz,ts,ws)

              do i=1,3
                do j=1,3
                  tsinv(i,j)=ts(j,i)
                enddo
              enddo

            endif ! if (bpebc(8,im).eq.-6)

            do i=1,3
              do j=1,3
                bpetm(i,j,iplan,im)=ts(i,j)
                bpetm(i,j+3,iplan,im)=tsinv(i,j)
              enddo
            enddo

        enddo !iplan

      enddo !imag

c transform everything to the nz=(0,0,1) system

      do imag=1,nmag

        qsign=0.0d0

        do iplan=1,ibpeplan(imag)

            do i=1,3
              do j=1,3
                ts(i,j)=bpetm(i,j,iplan,imag)
              enddo
            enddo

            vnormlab(1)=bpetm(1,8,iplan,imag)
            vnormlab(2)=bpetm(2,8,iplan,imag)
            vnormlab(3)=bpetm(3,8,iplan,imag)

            do icorn=1,ibpecorn(iplan,imag)

              r1lab(1)=bpemag(1,icorn,iplan,imag)
              r1lab(2)=bpemag(2,icorn,iplan,imag)
              r1lab(3)=bpemag(3,icorn,iplan,imag)

              r1(1)=ts(1,1)*r1lab(1)+ts(1,2)*r1lab(2)+ts(1,3)*r1lab(3)
              r1(2)=ts(2,1)*r1lab(1)+ts(2,2)*r1lab(2)+ts(2,3)*r1lab(3)
              r1(3)=ts(3,1)*r1lab(1)+ts(3,2)*r1lab(2)+ts(3,3)*r1lab(3)

              bperot(1,icorn,iplan,imag)=r1(1)
              bperot(2,icorn,iplan,imag)=r1(2)
              bperot(3,icorn,iplan,imag)=r1(3)

            enddo !icorn=1,ncorn

            do icorn=1,ibpecorn(iplan,imag)-1

              ip2=icorn+1

              r1(1)=bperot(1,icorn,iplan,imag)
              r1(2)=bperot(2,icorn,iplan,imag)
              r1(3)=bperot(3,icorn,iplan,imag)

              r2(1)=bperot(1,ip2,iplan,imag)
              r2(2)=bperot(2,ip2,iplan,imag)
              r2(3)=bperot(3,ip2,iplan,imag)

c              write(lun6,*)imag,iplan,icorn,r1(1)-r2(1)
c              if (abs(r1(1)-r2(1)).gt.tiny) then
              if (r1(1)-r2(1).ne.0.0d0) then

                a=(r2(2)-r1(2))/(r2(1)-r1(1))
                b=r1(2)-a*r1(1)

              else

                a=0.0d0
                b=r1(2)

              endif !(abs(r1(1)-r2(1)).gt.tiny)

              q=-((a*r1(1)+ a*r2(1) + 2.0d0*b)*(r1(1) - r2(1)))/2.0d0

              if (idebug.ne.0) then
                print*,"imag,iplan,icorn,a,b,q:",imag,iplan,icorn,a,b,q
              endif

              qsign=qsign+q*(
     &           vnormlab(1)*bpebc(4,imag)
     &          +vnormlab(2)*bpebc(5,imag)
     &          +vnormlab(3)*bpebc(6,imag))

            enddo ! icorn

c          endif !(ibpecorn(iplan,imag).gt.0) then

        enddo ! iplan=1,nplan

        if (abs(qsign/bpebc(7,imag)).gt.1.0d-9.and.bpebc(8,imag).ne.-6) then
c not checked for rectangular magnets since ibpecorn not negative for
c planes with normal vector parallel to surface. Then they are not skipped
c for while calculation qsign
          write(lun6,*)
          write(lun6,*)
     &      '*** Warning in UNDUMAG_INI_BPETM: Sum of magnetic charge not zero ***'
          write(lun6,*)'magnet: ',imag
          write(lun6,*)

          write(lun6,*)"|Q/Br| (supposed to be lower then 1.0e-9:",abs(qsign/bpebc(7,imag))
        endif

      enddo ! imag=1,nmag

      return
      end
