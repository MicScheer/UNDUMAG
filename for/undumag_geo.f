*CMZ :  2.04/09 16/08/2023  14.39.00  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 28/02/2023  12.18.17  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  10.01.47  by  Michael Scheer
*CMZ :  2.02/02 17/02/2022  12.11.11  by  Michael Scheer
*CMZ :  2.02/01 30/01/2022  08.30.33  by  Michael Scheer
*-- Author :    Michael Scheer   01/11/2021
      subroutine undumag_geo

      ! Writes voxels to undumag.geo

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      double precision gcen(3),xc,yc,zc,x1,x2,y1,y2,z1,z2,x21,y21,z21,
     &  x,y,z,bx,by,bz,dx,dy,dz,bc,cw

      integer :: lunvox,imag,nplan,icorn,ncorn,moth,motho,ispec,iplan,
     &  kmod,kmag,im,kcopy,mothdiv(4),ncol,mag,mat,ix,iy,iz,itype,imat,icol,
     &  i

      character(2048) cline

      type (T_Magnet) tmag
      type (T_Magnet_Copy) tmagc

      write(lun6,*)
      write(lun6,*)'Writing geometry to undumag.geo'
      write(lun6,*)

      open(newunit=lunvox,file="undumag.geo")

      write(lunvox,'(a)')
     &  "* mag ityp xc yc zc moth ix iy iz mat icol mx my mz bc iplan icorn x y z cmag cmoth"

      nmag=nvoxcopy_t
      motho=0

      do imag=1,nmag

        kmod=t_voxcopy(imag)%kmodule
        kmag=t_voxcopy(imag)%kproto
        im=t_voxcopy(imag)%kmagnet
        kvox=t_voxcopy(imag)%kvoxel
        kcopy=t_voxcopy(imag)%kcopy

        tmag=t_magnets(kmag)
        tmagc=t_magcopy(im)

        gcen=bpebc(1:3,imag)
        nplan=ibpeplan(imag)
        moth=nint(bpebc(15,imag))

        if(moth.ne.motho) then
          chmag=tmagc%cnam
          write(cline,*)"* ",trim(chmag),moth
          write(lunvox,'(a)')cline(2:len_trim(cline))
          motho=moth
        endif

        imat=nint(bpebc(9,imag))
        ispec=nint(bpebc(10,imag))
        bx=bpebc(11,imag)
        by=bpebc(12,imag)
        bz=bpebc(13,imag)
        bc=bpebc(14,imag)
        ncol=ibpecol(imag)
        itype=nint(bpebc(8,imag))

        mothdiv(1)=moth

        mothdiv(2)=t_magnets(kmag)%t_voxels(kvox)%ixdiv
        mothdiv(3)=t_magnets(kmag)%t_voxels(kvox)%iydiv
        mothdiv(4)=t_magnets(kmag)%t_voxels(kvox)%izdiv
        motho=0

        do iplan=1,nplan
          ncorn=ibpecorn(iplan,imag)
          do icorn=1,ncorn
            x=bpemag(1,icorn,iplan,imag)
            y=bpemag(2,icorn,iplan,imag)
            z=bpemag(3,icorn,iplan,imag)
            write(lunvox,*)imag,itype,
     &        sngl(gcen(1)),sngl(gcen(2)),sngl(gcen(3))
     &        ,mothdiv(1:4),imat,ncol
     &        ,sngl(bx),sngl(by),sngl(bz),sngl(bc)
     &        ,iplan,icorn
     &        ,sngl(x),sngl(y),sngl(z)
     &        ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
            if (kplsym.ne.0) then
              if (ixsym.ne.0.and.iysym.ne.0.and.izsym.ne.0) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)2*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)3*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)4*nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)5*nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)6*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)7*nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(-gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(-y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (ixsym.ne.0.and.iysym.ne.0) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)2*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)3*nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (ixsym.ne.0.and.izsym.ne.0) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)2*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(bz),sngl(-bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)3*nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (iysym.ne.0.and.izsym.ne.0) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)2*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(bz),sngl(-bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
                write(lunvox,*)3*nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (ixsym.ne.0) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(-gcen(1)),sngl(gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(-x),sngl(y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (iysym.ne.0.) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(gcen(1)),sngl(-gcen(2)),sngl(gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(-bx),sngl(by),sngl(bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(-y),sngl(z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              else if (izsym.ne.0.) then
                write(lunvox,*)nmag+imag,itype,
     &            sngl(gcen(1)),sngl(gcen(2)),sngl(-gcen(3))
     &            ,mothdiv(1:4),imat,ncol
     &            ,sngl(bx),sngl(by),sngl(-bz),sngl(bc)
     &            ,iplan,icorn
     &            ,sngl(x),sngl(y),sngl(-z)
     &            ,trim(tmagc%cnam)," ",trim(tmagc%cmoth)
              endif !syms
            endif !(kplsym.ne.0) then
          enddo
        enddo
      enddo !nmag

      dx=xmax_t-xmin_t
      dy=ymax_t-ymin_t
      dz=zmax_t-zmin_t

      do i=1,ncwires
        mag=i
        itype=nint(wire(1,i))
        moth=nint(wire(10,i))
        icol=nint(wire(9,i))
        x1=wire(3,i)
        y1=wire(4,i)
        z1=wire(5,i)
        ix=1
        iy=1
        iz=1
        mat=-1
        x2=wire(6,i)
        y2=wire(7,i)
        z2=wire(8,i)
        xc=(x2+x1)/2.0
        yc=(y2+y1)/2.0
        zc=(z2+z1)/2.0
        x21=(x2-x1)
        y21=(y2-y1)
        z21=(z2-z1)
        cw=wire(2,i)
        iplan=0
        icorn=0
        write(lunvox,*)mag,itype,xc,yc,zc,moth,ix,iy,iz,mat,icol,dx,dy,dz,cw,
     &    iplan,icorn,x1,y1,z1," wire ", adjustl(trim(t_coils(moth)%cnam))
      enddo !ncwires

      write(lun6,*)
      write(lun6,*)'Done'
      write(lun6,*)

      close(lunvox)

      return
      end
