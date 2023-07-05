*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.25/00 13/03/2018  13.43.22  by  Michael Scheer
*CMZ :  1.24/00 12/10/2017  13.53.10  by  Michael Scheer
*CMZ :  1.23/06 06/10/2017  14.14.51  by  Michael Scheer
*CMZ :  1.23/04 04/10/2017  14.24.59  by  Michael Scheer
*CMZ :  1.23/02 18/09/2017  15.06.15  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  14.56.35  by  Michael Scheer
*CMZ :  1.17/07 24/05/2017  13.01.19  by  Michael Scheer
*CMZ :  1.17/06 21/05/2017  14.17.21  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  16.35.25  by  Michael Scheer
*CMZ :  1.13/02 09/03/2017  16.19.19  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  16.37.24  by  Michael Scheer
*-- Author :    Michael Scheer   23/01/2017
      subroutine undumag_dipoles_field(xin,yin,zin,bxout,byout,bzout,kfail)

      use undumagf90m
      use bpolyederf90m

      use commandlinef90m

      implicit none

      double precision gcen(3),ri,rmin,rmax,riv(3),x,y,z,bn(3),b,
     &  bx,by,bz,bxd,byd,bzd,p(3),q,pr,rob,dlen,r2,r3,
     &  xin,yin,zin,bxout,byout,bzout,pn,bdx,bdy,bdz,xm,ym,zm,dom


      integer imag,iplan,icorn,npoi,ifail,kfail,ical,itry,kmag,idip,npoimax,
     &  nplan,ncorn,moth,istat

      save

c dipoles(1:3,imag)=gcen
c dipoles(4,imag)=pn
c dipoles(5:7,imag)=p/pn

      kfail=0

      if (ical.eq.0.or.kallodip.eq.0) then

        ndipoles=nmag
        if (ixsym.ne.0) ndipoles=ndipoles*2
        if (iysym.ne.0) ndipoles=ndipoles*2
        if (izsym.ne.0) ndipoles=ndipoles*2

        allocate(dipoles(16,ndipoles))


         dipoles=0.0d0
         kallodip=1
         ical=1

         kmag=nmag
         do imag=1,nmag

           if (bpebc(7,imag).eq.0.0d0) cycle

           bn=bpebc(4:6,imag) !*bc00(1:3,imag)
           b=sqrt(bn(1)**2+bn(2)**2+bn(3)**2)
           bn=bn/b

           npoi=0
           gcen=0.0d0

           do iplan=1,ibpeplan(imag)
             do icorn=1,ibpecorn(iplan,imag)-1
               npoi=npoi+1
               gcen(1:3)=gcen(1:3)+bpemag(1:3,icorn,iplan,imag)
c               write(lun6,*)imag,iplan,icorn,npoi,bpemag(1:3,icorn,iplan,imag)
             enddo !ncorn
           enddo !nplan

           gcen=gcen/npoi

           dipoles(1:3,imag)=gcen

           rmin=1.0d30
           rmax=-1.0d30
           do iplan=1,ibpeplan(imag)
             do icorn=1,ibpecorn(iplan,imag)
               riv(1:3)=bpemag(1:3,icorn,iplan,imag)-gcen
               ri=sqrt(riv(1)**2+riv(2)**2+riv(3)**2)
               if (ri.lt.rmin) rmin=ri
               if (ri.gt.rmax) rmax=ri
             enddo !ncorn
           enddo !nplan

           itry=0
1          rmax=rmax/2**itry

           rob=100.0d0*rmax
           dlen=rmin/2.0d0

           x=bpebc(1,imag)+bn(1)*rob
           y=bpebc(2,imag)+bn(2)*rob
           z=bpebc(3,imag)+bn(3)*rob

           call undumag_bpolyeder1(imag,x/1.0d3,y/1.0d3,z/1.0d3,bx,by,bz,ifail)
           if (ifail.ne.0) then
             write(lun6,*)"*** Warning in undumag_dipoles: Bad return from undumag_bolyeder1 ***"
             kfail=imag
           endif

           b=bx*bn(1)+by*bn(2)+bz*bn(3)
           q=b*rob**3/dlen
           p=dlen/2.0d0*q*bn

           pn=sqrt(p(1)**2+p(2)**2+p(3)**2)

           if (pn.eq.0.0d0) then
             if (rob.lt.rmax) then
               write(lun6,*)"*** Warning in undumag_dipoles, zero field ***"
               write(lun6,*)"*** Trying to recover, itry:",itry," ***"
               kfail=imag
               cycle
             endif
             itry=itry+1
             goto 1
           endif

           dipoles(4,imag)=pn
           dipoles(5:7,imag)=p/pn
           dipoles(8,imag)=bpebc(17,imag)
           moth=nint(bpebc(15,imag))
           dipoles(9,imag)=moth
           dipoles(10,imag)=imag

           r2=rob**2
           r3=r2*rob

           xm=x/1000.0d0
           ym=y/1000.0d0
           zm=z/1000.0d0

           dom=(r2-3.0d0*(xm**2+ym**2+zm**2))*2.0d0

           p(1)=r3/dom*
     & (-bx*r2+3.0d0*bx*ym**2+3.0d0*bx*zm**2-3.0d0*by*xm*ym-3.0d0*bz*xm*zm)
           p(2)=r3/dom*
     & (-3.0d0*bx*xm*ym-by*r2+3.0d0*by*xm**2+3.0d0*by*zm**2-3.0d0*bz*ym*zm)
           p(3)=r3/dom*
     & (-3.0d0*bx*xm*zm-3.0d0*by*ym*zm-bz*r2+3.0d0*bz*xm**2+3.0d0*bz*ym**2)

           pn=sqrt(p(1)**2+p(2)**2+p(3)**2)

           dipoles(4,imag)=pn
           dipoles(5:7,imag)=-p/pn

           if (nmag.ne.ndipoles) then
             if (ixsym.ne.0.and.iysym.ne.0.and.izsym.ne.0) then
               kmag=kmag+1 !xsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               kmag=kmag+1 !ysym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !zsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(9:10,kmag)=dipoles(9:10,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !ix + iy
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(3,kmag)=dipoles(3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !ix + iz
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(2,kmag)=dipoles(2,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !iy + iz
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1,kmag)=dipoles(1,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=dipoles(7,imag)
               kmag=kmag+1 !ix + iy + iz
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:3,kmag)=-dipoles(1:3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=dipoles(7,imag)
             else if (ixsym.ne.0.and.iysym.ne.0) then
               kmag=kmag+1 !xsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               kmag=kmag+1 !ysym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !ix + iy
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:2,kmag)=-dipoles(1:2,imag)
               dipoles(3:6,kmag)=dipoles(3:6,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
             else if (ixsym.ne.0.and.izsym.ne.0) then
               kmag=kmag+1 !xsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               kmag=kmag+1 !zsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(9:10,kmag)=dipoles(9:10,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !ix + iz
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(2,kmag)=dipoles(2,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
             else if (iysym.ne.0.and.izsym.ne.0) then
               kmag=kmag+1 !ysym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !zsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(9:10,kmag)=dipoles(9:10,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
               kmag=kmag+1 !iy + iz
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1,kmag)=dipoles(1,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(4,kmag)=dipoles(4,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(6,kmag)=dipoles(6,imag)
               dipoles(7,kmag)=dipoles(7,imag)
             else if (ixsym.ne.0) then
               kmag=kmag+1 !xsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(1,kmag)=-dipoles(1,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
             else if (iysym.ne.0) then
               kmag=kmag+1 !ysym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(2,kmag)=-dipoles(2,imag)
               dipoles(5,kmag)=-dipoles(5,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
             else if (izsym.ne.0) then
               kmag=kmag+1 !zsym
               dipoles(8,kmag)=dipoles(8,imag)
               dipoles(9:10,kmag)=dipoles(9:10,imag)
               dipoles(1:7,kmag)=dipoles(1:7,imag)
               dipoles(3,kmag)=-dipoles(3,imag)
               dipoles(7,kmag)=-dipoles(7,imag)
             endif
           endif !(nmag.ne.ndipoles) then

         enddo !nmag

      endif !ical

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      do idip=1,ndipoles
        if (dipoles(8,idip).lt.0.0d0) cycle
        call undumag_dipole_field(idip,xin,yin,zin,bx,by,bz,kfail)
        bxout=bxout+bx
        byout=byout+by
        bzout=bzout+bz
      enddo !nmag

      if (kbextern.ne.0) then
        bxout=bxout+bxex
        byout=byout+byex
        bzout=bzout+bzex
      endif

      if (ncwires+nrace.gt.0) then
        call undumag_bcoils(xin,yin,zin,bx,by,bz,istat)
        bxout=bxout+bx
        byout=byout+by
        bzout=bzout+bz
        if (istat.ne.0) kfail=kfail+1000
      endif

      return
      end
