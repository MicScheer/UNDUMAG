*CMZ :  1.02/00 01/10/2014  14.10.39  by  Michael Scheer
*CMZ :  1.01/02 26/09/2014  13.40.07  by  Michael Scheer
*CMZ :  1.00/01 24/09/2014  13.22.59  by  Michael Scheer
*CMZ :  0.01/03 22/09/2014  21.17.48  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  19.54.46  by  Michael Scheer
*-- Author :    Michael Scheer   08/09/2014
      subroutine mshplt_view_log_axis(
     &  xorig, yorig, xylength, smin, smax, ang,
     &  chhe, ticheight, title,iticside,ilabside,reltitang,offtit,
     &  rellabang,offlab)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xorig, yorig, xylength, smin, smax, ang,chhe,ticheight
      real titang,offtit,offlab,sminl,smaxl,reltitang,rellabang,
     &  x(2),y(2),aLabSide,xdec,ydec,nexp,tango,chheo,alabang

      integer iticside,ilabside,j,l,idecmin,idecmax
      character(*) title

      integer ntic,nlab,ndec,i
      real xmin,xmax,ymin,ymax,sinang,cosang,dxy,tside,costit,sintit

      character(8) c8

      call mshplt_flush_buff

      write(lun_ps,'(a)')'% begin of mshplt_view_log_axis'

      tango=ang_ps
      chheo=chhe_ps

      if (iticside.lt.0) then
        tside=1.
      else if (iticside.gt.0) then
        tside=-1.
      else
        tside=0.
      endif

      if (ilabside.lt.0) then
        aLabSide=-1.
      else if (ilabside.gt.0) then
        aLabSide=0.5
      else
        aLabSide=0.
      endif

      if (smin.le.0.0) then
        sminl=-30.
      else
        sminl=alog10(smin)
      endif

      if (smax.le.0.0) then
        smaxl=30.
      else
        smaxl=alog10(smax)
      endif

      cosang=cos(ang/radtodeg_ps)
      sinang=sin(ang/radtodeg_ps)

c      wtov=1./(scalex_ps*cosang+scaley_ps*sinang)

      xmin=xorig
      xmax=xorig+cosang*xylength
      ymin=yorig
      ymax=yorig+sinang*xylength

      call mshplt_line_raw(xmin,ymin,xmax,ymax)

      costit=cos((ang+rellabang)/radtodeg_ps)
      sintit=sin((ang+rellabang)/radtodeg_ps)

      call mshplt_view_to_world(0.,0.,x(1),y(1))
      call mshplt_view_to_world(costit,sintit,x(2),y(2))
      alabang=atan2(y(2)-y(1),x(2)-x(1))*radtodeg_ps

      costit=cos((ang+reltitang)/radtodeg_ps)
      sintit=sin((ang+reltitang)/radtodeg_ps)

      call mshplt_view_to_world(0.,0.,x(1),y(1))
      call mshplt_view_to_world(costit,sintit,x(2),y(2))
      titang=atan2(y(2)-y(1),x(2)-x(1))*radtodeg_ps

      idecmin=int(sminl)
      idecmax=int(smaxl)

      if (idecmax.lt.30.and.10**idecmax.lt.smax) then
        idecmax=idecmax+1
      endif

      ndec=idecmax-idecmin

      nlab=ndec*9
      ntic=nlab

      dxy=xylength/ndec

      write(c8,'(i8)')max(abs(idecmin),abs(idecmax))
      do l=1,8
        if(c8(l:l).ne.' ') then
          nexp=8-l+1
          goto 1
        endif
      enddo

 1    continue

      call mshplt_set_text_angle(alabang)
      call mshplt_set_character_height(aLabHeight_ps)

      do i=1,ndec

        xdec=xorig+i*dxy*cosang
        ydec=yorig+i*dxy*sinang

        x(1)=xdec
        x(2)=x(1)+ticheight*tside*sinang/scalex_ps
        y(1)=ydec
        y(2)=y(1)-ticheight*tside*cosang/scaley_ps

        call mshplt_pline_raw(2,x,y)

        if (ilabside.ne.0.and.aLabHeight_ps.gt.0.0) then

          x(2)=x(2)
     &      -iLabSide*sign(1.,90.0-theta_ps)*sinang/scalex_ps*(offlab+(0.8*nexp*aLabHeight_ps))

          y(2)=ydec
     &      -cosang/scaley_ps*(offlab+(0.8*nexp*aLabHeight_ps))

          call mshplt_text_raw(x(2),y(2),"10")

          write(c8,'(i8)')idecmin+i
          do l=1,8
            if(c8(l:l).ne.' ') goto 11
          enddo
 11       continue

          call mshplt_text_upper(c8(l:8))

        endif

        do j=2,9
          x(1)=xorig+((i-1)*dxy+alog10(float(j))*dxy)*cosang
          x(2)=x(1)+ticheight/scalex_ps*sinang*tside*0.8
          y(1)=yorig+((i-1)*dxy+alog10(float(j))*dxy)*sinang
          y(2)=y(1)-ticheight/scaley_ps*cosang*tside*0.8
          call mshplt_pline_raw(2,x,y)
        enddo

      enddo !ndec

      call mshplt_set_character_height(chhe)

      call mshplt_set_text_angle(titang)

      if (iLabSide.ne.0.and.aLabHeight_ps.ne.0.0) then

        x(1)=xorig
     &    +0.5*xylength*cosang
     &    -len_trim(title)*chhe/4.*costit/scalex_ps
     &    +iLabSide*(offtit+0.8*nexp*aLabHeight_ps)*abs(sintit)/scalex_ps

        y(1)=yorig
     &    +0.5*xylength*sinang
     &    -len_trim(title)*chhe/4.*sintit/scaley_ps
     &    -(offtit+0.8*nexp*aLabHeight_ps)*costit/scaley_ps

      else
        goto 9999
      endif

      call mshplt_set_character_height(chhe)
      call mshplt_text_raw(x(1),y(1),title(1:len_trim(title)))
      call mshplt_set_character_height(chheo)
      call mshplt_set_text_angle(tango)

 9999 call mshplt_flush_buff
      write(lun_ps,'(a)')'% end of mshplt_log_axis'

      return
      end
