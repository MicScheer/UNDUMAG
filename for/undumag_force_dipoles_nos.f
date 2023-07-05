*CMZ :  2.01/03 13/06/2019  15.53.32  by  Michael Scheer
*CMZ :  1.24/01 12/10/2017  16.37.55  by  Michael Scheer
*CMZ :  1.24/00 12/10/2017  14.42.07  by  Michael Scheer
*CMZ :  1.23/05 06/10/2017  12.50.32  by  Michael Scheer
*CMZ :  1.23/04 05/10/2017  09.26.43  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  19.42.41  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  16.55.27  by  Michael Scheer
*-- Author :    Michael Scheer   24/01/2017
      subroutine undumag_force_dipoles_nos(kdip,ifail)

      use undumagf90m
      use bpolyederf90m

      implicit none

      double precision :: eps=0.0001 !Meter
      double precision x,y,z,xm,xp,ym,yp,zm,zp,bxm,bym,bzm,bxp,byp,bzp,p(3),pn,
     &  dbxdx,dbxdy,dbxdz,
     &  dbydx,dbydy,dbydz,
     &  dbzdx,dbzdy,dbzdz,
     &  dfxdip,dfydip,dfzdip,
     &  dtxdip,dtydip,dtzdip,bx,by,bz,bpebc17

      integer kdip,idip,ifail,kmag,moth,imag

      ifail=0
      if (kdip.lt.1.or.kdip.gt.ndipoles) then
        ifail=1
        return
      endif

      x=dipoles(1,kdip)/1000.0d0
      y=dipoles(2,kdip)/1000.0d0
      z=dipoles(3,kdip)/1000.0d0

      p=dipoles(4,kdip)*dipoles(5:7,kdip)/1000.0d0

      xp=x+eps
      xm=x-eps
      yp=y+eps
      ym=y-eps
      zp=z+eps
      zm=z-eps

      dfxdip=0.0d0
      dfydip=0.0d0
      dfzdip=0.0d0

      dtxdip=0.0d0
      dtydip=0.0d0
      dtzdip=0.0d0

      if (iforcedip.lt.0) then

        do idip=1,ndipoles

          if (dipoles(9,idip).eq.dipoles(9,kdip)) cycle

          call undumag_dipole_field(idip,xp,y,z,bxp,byp,bzp,ifail)
          call undumag_dipole_field(idip,xm,y,z,bxm,bym,bzm,ifail)
          dbxdx=(bxp-bxm)/(xp-xm)
          dbydx=(byp-bym)/(xp-xm)
          dbzdx=(bzp-bzm)/(xp-xm)

          call undumag_dipole_field(idip,x,yp,z,bxp,byp,bzp,ifail)
          call undumag_dipole_field(idip,x,ym,z,bxm,bym,bzm,ifail)
          dbxdy=(bxp-bxm)/(yp-ym)
          dbydy=(byp-bym)/(yp-ym)
          dbzdy=(bzp-bzm)/(yp-ym)

          call undumag_dipole_field(idip,x,y,zp,bxp,byp,bzp,ifail)
          call undumag_dipole_field(idip,x,y,zm,bxm,bym,bzm,ifail)
          dbxdz=(bxp-bxm)/(zp-zm)
          dbydz=(byp-bym)/(zp-zm)
          dbzdz=(bzp-bzm)/(zp-zm)

          dfxdip=dfxdip+p(1)*dbxdx+p(2)*dbydx+p(3)*dbzdx
          dfydip=dfydip+p(1)*dbxdy+p(2)*dbydy+p(3)*dbzdy
          dfzdip=dfzdip+p(1)*dbxdz+p(2)*dbydz+p(3)*dbzdz

          call undumag_dipole_field(idip,x,y,z,bx,by,bz,ifail)

          dtxdip=dtxdip+p(2)*bz-p(3)*by
          dtydip=dtydip+p(3)*bx-p(1)*bz
          dtzdip=dtzdip+p(1)*by-p(2)*bx

        enddo

      else !iforcedip>0 {

c        kmag=dipoles(10,kdip)
c        bpebc17=bpebc(17,kmag)
c        bpebc(17,kmag)=-1

        do imag=1,nmag

          moth=bpebc(15,imag)
          if (moth.eq.kforcemag) cycle

          call undumag_bpolyeder1_sym(imag,xp,y,z,bxp,byp,bzp,ifail)
          call undumag_bpolyeder1_sym(imag,xm,y,z,bxm,bym,bzm,ifail)
          dbxdx=(bxp-bxm)/(xp-xm)
          dbydx=(byp-bym)/(xp-xm)
          dbzdx=(bzp-bzm)/(xp-xm)

          call undumag_bpolyeder1_sym(imag,x,yp,z,bxp,byp,bzp,ifail)
          call undumag_bpolyeder1_sym(imag,x,ym,z,bxm,bym,bzm,ifail)
          dbxdy=(bxp-bxm)/(yp-ym)
          dbydy=(byp-bym)/(yp-ym)
          dbzdy=(bzp-bzm)/(yp-ym)

          call undumag_bpolyeder1_sym(imag,x,y,zp,bxp,byp,bzp,ifail)
          call undumag_bpolyeder1_sym(imag,x,y,zm,bxm,bym,bzm,ifail)
          dbxdz=(bxp-bxm)/(zp-zm)
          dbydz=(byp-bym)/(zp-zm)
          dbzdz=(bzp-bzm)/(zp-zm)

          dfxdip=dfxdip+p(1)*dbxdx+p(2)*dbydx+p(3)*dbzdx
          dfydip=dfydip+p(1)*dbxdy+p(2)*dbydy+p(3)*dbzdy
          dfzdip=dfzdip+p(1)*dbxdz+p(2)*dbydz+p(3)*dbzdz

          call undumag_bpolyeder1_sym(imag,x,y,z,bx,by,bz,ifail)

          dtxdip=dtxdip+p(2)*bz-p(3)*by
          dtydip=dtydip+p(3)*bx-p(1)*bz
          dtzdip=dtzdip+p(1)*by-p(2)*bx

        enddo

c        bpebc(17,kmag)=bpebc17

      endif !(iforcedip.lt.0) then

      ! Factor 10 maybe due to H in Tesla and m to mm somewhere !??

      dfxdip=10.0d0*dfxdip
      dfydip=10.0d0*dfydip
      dfzdip=10.0d0*dfzdip

      dtxdip=10.0d0*dtxdip
      dtydip=10.0d0*dtydip
      dtzdip=10.0d0*dtzdip

      dipoles(11,kdip)=dfxdip
      dipoles(12,kdip)=dfydip
      dipoles(13,kdip)=dfzdip

      dipoles(14,kdip)=dtxdip
      dipoles(15,kdip)=dtydip
      dipoles(16,kdip)=dtzdip

      fxdip=fxdip+dfxdip
      fydip=fydip+dfydip
      fzdip=fzdip+dfzdip

      txdip=txdip+dtxdip
      tydip=tydip+dtydip
      tzdip=tzdip+dtzdip

      return
      end
