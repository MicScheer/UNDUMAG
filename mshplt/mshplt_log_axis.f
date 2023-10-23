*CMZ :          20/10/2023  20.40.14  by  Michael Scheer
*CMZ :  1.03/01 08/10/2014  12.21.58  by  Michael Scheer
*CMZ :  1.01/02 26/09/2014  11.35.12  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  16.31.07  by  Michael Scheer
*CMZ :  1.00/01 23/09/2014  20.14.56  by  Michael Scheer
*CMZ :  0.01/03 22/09/2014  21.17.48  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  19.54.46  by  Michael Scheer
*-- Author :    Michael Scheer   08/09/2014
      subroutine mshplt_log_axis(
     &  xorig, yorig, xylength, smin, smax, ang,
     &  chhe, ticheight, title,iticside,ilabside,titang,offtit,offlab)

      implicit none

*KEEP,MSHPLTINCL.
      include 'mshplt.cmn'
*KEND.

      real xorig, yorig, xylength, smin, smax, ang,chhe,ticheight
      real titang,offtit,offlab,sminl,smaxl,
     &  x(2),y(2),aLabSide,xdec,ydec,xv,yv,nexp,ango,chheo

      integer iticside,ilabside,j,l,idecmin,idecmax
      character(*) title

      integer ntic,nlab,ndec,i
      real xmin,xmax,ymin,ymax,sinang,cosang,dxy,tside,costit,sintit

      character(8) c8

      call mshplt_flush_buff

      write(lun_ps,'(a)')'% begin of mshplt_log_axis'

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
        aLabSide=0.25
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

      cosang=cos(ang*1.745329e-2)
      sinang=sin(ang*1.745329e-2)

      call  mshplt_world_to_view(xorig,yorig,xmin,ymin)

      call  mshplt_world_to_view(
     &  xorig+cosang*xylength,yorig+sinang*xylength,
     &  xmax,ymax)

      call mshplt_line_raw(xmin,ymin,xmax,ymax)

      idecmin=nint(sminl)
      if (sminl.ne.idecmin) then
        print*,'*** Warning in mshplt_log_axis: Routine must be called with integer power of 10'
      endif
      idecmax=nint(smaxl)
      if (smaxl.ne.idecmax) then
        print*,'*** Warning in mshplt_log_axis: Routine must be called with integer power of 10'
      endif

      if (idecmax.lt.30.and.10.0**idecmax.lt.smax.or.idecmax.eq.idecmin) then
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

      do i=1,ndec

        if (i.eq.1) then

          xdec=xorig
          ydec=yorig

          x(1)=xdec
          x(2)=x(1)+ticheight*tside*sinang
          y(1)=ydec
          y(2)=y(1)+ticheight*tside*cosang

          call  mshplt_world_to_view(x(1),y(1),x(1),y(1))
          call  mshplt_world_to_view(x(2),y(2),x(2),y(2))

          if (ilabside.ne.0.and.aLabHeight_ps.gt.0.0) then
            x(2)=xdec+aLabSide*sinang*(offlab+(1.+nexp)*aLabHeight_ps/2.)
     &        -aLabHeight_ps*cosang
            y(2)=ydec+aLabSide*offlab*cosang-aLabHeight_ps
     &        +aLabHeight_ps*cosang
            call  mshplt_world_to_view(x(2),y(2),xv,yv)
            call mshplt_set_character_height(aLabHeight_ps*1.2)
            call mshplt_text_raw(xv,yv,"10")

            write(c8,'(i8)')idecmin
            do l=1,8
              if(c8(l:l).ne.' ') goto 12
            enddo
 12         continue

            call mshplt_text_upper(c8(l:8))

          endif
        endif

        xdec=xorig+i*dxy*cosang
        ydec=yorig+i*dxy*sinang

        x(1)=xdec
        x(2)=x(1)+ticheight*tside*sinang
        y(1)=ydec
        y(2)=y(1)+ticheight*tside*cosang

        call  mshplt_world_to_view(x(1),y(1),x(1),y(1))
        call  mshplt_world_to_view(x(2),y(2),x(2),y(2))

        call mshplt_pline_raw(2,x,y)

        if (ilabside.ne.0.and.aLabHeight_ps.gt.0.0) then
          x(2)=xdec+aLabSide*sinang*(offlab+(1.+nexp)*aLabHeight_ps/2.)
     &      -aLabHeight_ps*cosang
          y(2)=ydec+aLabSide*offlab*cosang-aLabHeight_ps
     &      +aLabHeight_ps*cosang
          call  mshplt_world_to_view(x(2),y(2),xv,yv)
          call mshplt_set_character_height(aLabHeight_ps*1.2)
          call mshplt_text_raw(xv,yv,"10")

          write(c8,'(i8)')idecmin+i
          do l=1,8
            if(c8(l:l).ne.' ') goto 11
          enddo
 11       continue

          call mshplt_text_upper(c8(l:8))

        endif

        do j=2,9
          x(1)=xorig+((i-1)*dxy+alog10(float(j))*dxy)*cosang
          x(2)=x(1)+ticheight*sinang*tside*0.8
          y(1)=yorig+((i-1)*dxy+alog10(float(j))*dxy)*sinang
          y(2)=y(1)+ticheight*cosang*tside*0.8
          call  mshplt_world_to_view(x(1),y(1),x(1),y(1))
          call  mshplt_world_to_view(x(2),y(2),x(2),y(2))
          call mshplt_pline_raw(2,x,y)
        enddo
      enddo

      chheo=chhe_ps
      ango=ang_ps

      costit=cos((ang+titang)*1.745329e-2)
      sintit=sin((ang+titang)*1.745329e-2)

      call mshplt_set_text_angle(ang+titang)

      if (aLabSide.gt.0.0) then
        x(1)=xorig
     &    +0.5*xylength*costit
     &    -len_trim(title)*chhe/4.*costit
     &    +(6.+nexp)/2.*aLabSide*(offtit+aLabHeight_ps)*sintit
        y(1)=yorig
     &    +0.5*xylength*sintit
     &    -len_trim(title)*chhe/4.*sintit
     &    +(6.+nexp)/2.*aLabSide*(offtit+aLabHeight_ps)*costit
      else if (aLabSide.lt.0.0) then
        x(1)=xorig
     &    +0.5*xylength*costit
     &    +aLabSide*len_trim(title)*chhe/4.*costit
     &    +aLabSide*(offtit+aLabHeight_ps)*sintit
        y(1)=yorig
     &    +0.5*xylength*sintit
     &    +aLabSide*len_trim(title)*chhe/4.*sintit
     &    +aLabSide*(offtit+aLabHeight_ps)*costit
      else
        goto 9999
      endif

      call mshplt_world_to_view(x(1),y(1),xv,yv)
      call mshplt_set_character_height(chhe)
      call mshplt_text_raw(xv,yv,title(1:len_trim(title)))

 9999 continue

      call mshplt_set_character_height(chheo)
      call mshplt_set_text_angle(ango)

      call mshplt_flush_buff
      write(lun_ps,'(a)')'% end of mshplt_log_axis'

      return
      end
