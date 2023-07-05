*CMZ :          02/08/2018  18.13.30  by  Michael Scheer
*CMZ :  1.03/02 23/09/2016  13.46.02  by  Michael Scheer
*CMZ :  1.02/00 01/10/2014  14.43.01  by  Michael Scheer
*CMZ :  1.01/02 27/09/2014  14.55.40  by  Michael Scheer
*CMZ :  1.01/00 24/09/2014  14.41.50  by  Michael Scheer
*CMZ :  1.00/01 24/09/2014  11.26.19  by  Michael Scheer
*CMZ :  0.01/03 23/09/2014  14.12.14  by  Michael Scheer
*CMZ :  0.01/02 22/09/2014  19.04.48  by  Michael Scheer
*CMZ :  0.01/01 27/08/2014  08.54.49  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.47.40  by  Michael Scheer
*CMZ :  0.00/05 18/08/2014  13.15.26  by  Michael Scheer
*CMZ :  0.00/04 14/08/2014  12.58.41  by  Michael Scheer
*CMZ :  0.00/03 05/08/2014  15.57.52  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  14.42.14  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_axis_taylor(
     &  xmin,xmax,ymin,ymax,smin,smax,
     &  nlab,chlab,rlabsiz,xlabrel,ylaboff,xoffexp,yoffexp,rlabangrel,
     &  ntic,ticsiz,ticposrel,ticangrel,
     &  titsiz,titposrel,titoff,titangrel,chtit
     &  )

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      integer :: ical=0

      integer nlab,ntic,i,nbig,ic,ifirst,nexp,iprimitiv,k,nskip,iexpmode,
     &  nexpmax,kbig(100),ndig,nticmax,inv

      real xmin,xmax,ymin,ymax,axang,x(2),y(2),ango,chheo,
     &  rlabsiz(*),xlabrel(*),ylaboff(*),xoffexp,yoffexp,rlabangrel(*),axlen,
     &  titsiz,titangrel,ticsiz(*),ticposrel(*),ticangrel(*),cosang,sinang,
     &  titposrel,titoff,xlab(100),smin,smax,rmanti,rtotlabx,rtotlaby,
     &  costitang,sintitang,costicang,sinticang,coslabang,sinlabang
     &  ,ticsizo,dxabs,xlabinv(100)

      character(*) chtit,chlab(*)
      character(12) chreal
      character(3) chexp

      ical=ical+1

      ndig=100

      if (ymax.eq.ymin.and.xmax.eq.xmin) then
        print*,'*** Error in mshplt_axis_taylor: Zero length axis ***'
        return
      endif

      call mshplt_flush_buff
      write(lun_ps,'(a)')'% begin of mshplt_axis_taylor'

      ticsizo=ticsiz(1)

      ango=ang_ps
      chheo=chhe_ps
      axang=atan2((ymax-ymin)*scaley_ps,(xmax-xmin)*scalex_ps) !radian
      cosang=cos(axang)
      sinang=sin(axang)
      axang=axang/1.745329251994e-2 !degree, on paper
      axlen=sqrt(((ymax-ymin)*scaley_ps)**2+((xmax-xmin)*scalex_ps)**2) !paper

      ifirst=1
      kbig=0
      iprimitiv=0

      call mshplt_line_raw(xmin,ymin,xmax,ymax)

      if (nlab.lt.0) then

c nlab.lt.0 means automatic mode, where abs(nlab) is the garantied size of the
c considered arrays. Nlab is number of labels and ntic is number of all tics
c Most values are overwritten and returned, so they must not be constants.

        if (len(chlab(1)).lt.12) then
          write(lun_ps,*)
     &      '%*** Error in mshplt_axis_taylor: For automatic mode, the size of'
          write(lun_ps,*)
     &      '%*** must be at least 12, i.e. character(12)'
          write(6,*)
     &      '*** Error in mshplt_axis_taylor: For automatic mode, the size of'
          write(6,*)
     &      '*** must be at least 12, i.e. character(12)'
          goto 9999
        endif

        if (-nlab.gt.100) then
          write(6,*)
     &      '*** Error in mshplt_axis_taylor: Dimension of labels exceeded'
          write(lun_ps,*)
     &      '%*** Error in mshplt_axis_taylor: Dimension of labels exceeded'
          goto 9999
        endif

        nticmax=max(3,int(axlen))

c111     continue
        iprimitiv=1
c Problems with negative axis
        if (smax.lt.0.0d0) then
          call mshplt_axis_division(abs(smax),abs(smin),nticmax,nlab,xlabinv)
          do inv=1,nlab
            xlab(inv)=-xlabinv(nlab-inv+1)
            write(chlab(1),'(2pe12.3)')xlab(inv)
            read(chlab(1),*)xlab(inv)
          enddo
        else
          call mshplt_axis_division(smin,smax,nticmax,nlab,xlab)
          do inv=1,nlab
            write(chlab(1),'(2pe12.3)')xlab(inv)
            read(chlab(1),*)xlab(inv)
          enddo
        endif

        dxabs=abs(xlab(2)-xlab(1))
        iexpmode=0
        write(chlab(1),'(2pe12.3)')dxabs
        read(chlab(1),*)dxabs
        write(chlab(1),'(1pe12.5)')dxabs
        read(chlab(1)(10:12),'(i3)')nexpmax
        if (abs(xlab(nlab)-xlab(1)).gt.9999.
     &    .or.
     &    abs(xlab(nlab)-xlab(1)).lt.0.001) iexpmode=1

        if (iexpmode.eq.0) nexpmax=0

        ntic=nlab
        nbig=0

        if (nlab.eq.1) then
          ticposrel(1)=0.5
          xlabrel(1)=0.5
        else if (nlab.eq.2) then
          ticposrel(1)=0.25
          xlabrel(1)=0.25
          ticposrel(2)=0.75
          xlabrel(2)=0.75
        endif

        do i=1,ntic
          ticsiz(i)=ticsizo
          ticangrel(i)=ticangrel(1)
          rlabangrel(i)=rlabangrel(1)
          rlabsiz(i)=rlabsiz(1)
          if (iexpmode.eq.0) then
            write(chlab(i),'(g12.5)')xlab(i)
          else
            write(chlab(i),'(1pe12.5)')xlab(i)
          endif
        enddo

        if (nlab.ge.2) then

          do i=1,ntic
            ticposrel(i)=(xlab(i)-smin)/(smax-smin)
            xlabrel(i)=ticposrel(i)
          enddo

          write(chreal,'(2pe12.3)')xlab(2)-xlab(1)

          if (chreal(2:3).eq.'10'
     &        .or.
     &        chreal(2:3).eq.'25'
     &        .or.
     &        chreal(2:3).eq.'50'
     &        ) then

            iprimitiv=0

            do i=1,ntic
              chlab(i)=''
              ticsiz(i)=ticsizo
              ticangrel(i)=90.
              ticposrel(i)=(xlab(i)-smin)/(smax-smin)
              rlabangrel(i)=rlabangrel(1)
              rlabsiz(i)=rlabsiz(1)
              xlabrel(i)=ticposrel(i)
c              write(chreal,'(g12.5)')xlab(i)/10**nexpmax
            enddo !nlab

          endif
        endif !nlab > 2

c        if (iprimitiv.ne.0) then

c          write(chlab(i),'(g12.3)')xlab(i)

c        else

          do i=1,ntic

            if (xlab(i).eq.0.0) then
              chlab(i)='0.0'
              cycle
            endif

            write(chlab(i),'(f12.4)')xlab(i)/10.**nexpmax

            read(chlab(i)(1:8),*)rmanti
            nexp=0

            ifirst=0
            do ic=1,len_trim(chlab(i))
              if (chlab(i)(ic:ic).ne.' '.and.ifirst.eq.0) then
                ifirst=ic
                goto 123
              endif
            enddo
123         continue

            chlab(i)=chlab(i)(ifirst:len_trim(chlab(i)))

            do ic=len_trim(chlab(i)),1,-1
              if (chlab(i)(ic:ic).eq.'0') then
                chlab(i)(ic:ic)=''
              else
                goto 124
              endif
            enddo

124         continue

            if(chlab(i)(len_trim(chlab(i)):len_trim(chlab(i))).eq.'.')
     &        chlab(i)(len_trim(chlab(i)):len_trim(chlab(i)))=''

          enddo ! labels

c        endif !iprimitiv
      endif !nlab.lt.0, i.e. automatic mode

      do i=1,ntic

        costicang=cos((ticangrel(i)+axang)*1.745329251994e-2)
        sinticang=sin((ticangrel(i)+axang)*1.745329251994e-2)

        x(1)=xmin+ticposrel(i)*(xmax-xmin)
        y(1)=ymin+ticposrel(i)*(ymax-ymin)
        call mshplt_view_to_world(x(1),y(1),x(1),y(1))

        x(2)=x(1)+ticsiz(i)*costicang
        y(2)=y(1)+ticsiz(i)*sinticang

        call mshplt_world_to_view(x(1),y(1),x(1),y(1))
        call mshplt_world_to_view(x(2),y(2),x(2),y(2))
c20160923        if (x(1).le.xmax) then
          call mshplt_pline_raw(2,x,y)
c20160923        endif
      enddo

      rtotlabx=0.
      do i=1,ntic
        if (len_trim(chlab(i)).ne.0) then
          coslabang=cos((rlabangrel(i)+axang)*1.745329251994e-2)
          sinlabang=sin((rlabangrel(i)+axang)*1.745329251994e-2)
          rtotlabx=rtotlabx+len_trim(chlab(i))*rlabsiz(i)/2.*coslabang
          rtotlaby=rtotlaby+len_trim(chlab(i))*rlabsiz(i)/2.*sinlabang
        endif
      enddo

      if (sqrt(rtotlabx**2+rtotlaby**2).gt.axlen*0.5) then
c        nticmax=nticmax-1
c        goto 111
        if (nbig.ge.2) then
          do k=1,ntic
            if (kbig(k).eq.0) then
              chlab(k)=''
            endif
          enddo
        else !nbig
          if (ntic.gt.2.and.mod(ntic,2).eq.0) then
            nskip=int(sqrt(rtotlabx**2+rtotlaby**2)/axlen+0.5)*2
            if (ntic.gt.4) then
              do i=1,ntic,nskip+1
                do k=1,nskip
                  chlab(i-1+k)=''
                enddo
              enddo
            else
              chlab(2)=''
              chlab(4)=''
            endif
          else
            do i=2,ntic-1
              if (i.ne.ntic/2+1) chlab(i)=''
            enddo
          endif
        endif !nbig
      endif !to many letters

      do i=1,nlab
        if (rlabsiz(i).gt.0.) then

          coslabang=cos((rlabangrel(i)+axang)*1.745329251994e-2)
          sinlabang=sin((rlabangrel(i)+axang)*1.745329251994e-2)

          call mshplt_set_text_angle(axang+rlabangrel(i))
          call mshplt_set_character_height(rlabsiz(i))

          x(1)=xmin+xlabrel(i)*(xmax-xmin)
          y(1)=ymin+xlabrel(i)*(ymax-ymin)

          call mshplt_view_to_world(x(1),y(1),x(1),y(1))

          x(1)=x(1)+ylaboff(i)*sinang
     &      -len_trim(chlab(i))*rlabsiz(i)/4.*coslabang

          y(1)=y(1)-ylaboff(i)*cosang
     &      -sign(1.,90.-theta_ps)*rlabsiz(i)/2.*sinang-len_trim(chlab(i))*rlabsiz(i)/4.*sinlabang

          call mshplt_world_to_view(x(1),y(1),x(1),y(1))
          call mshplt_text_raw(x(1),y(1),chlab(i))

c20170602??          if (i.eq.nlab.and.iprimitiv.eq.0.and.iexpmode.ne.0) then
          if (i.eq.nlab.and.iexpmode.ne.0) then

            x(1)=xmax
            y(1)=ymax

            call mshplt_view_to_world(x(1),y(1),x(1),y(1))

            if (ylaboff(i).gt.0.) then
              x(1)=x(1)+xoffexp*cosang+yoffexp*sinang
              y(1)=y(1)+xoffexp*sinang-yoffexp*cosang
            else
              x(1)=x(1)+xoffexp*cosang-(yoffexp+3.*rlabsiz(i))*sinang
              y(1)=y(1)+xoffexp*sinang+yoffexp*cosang
            endif

            call mshplt_world_to_view(x(1),y(1),x(1),y(1))
            call mshplt_set_character_height(rlabsiz(i)*1.2)
            call mshplt_text_raw(x(1),y(1),'x 10')

            write(chexp,'(i3)')nexpmax

            if (chexp(1:2).eq.'  ') then
              call mshplt_text_upper(chexp(3:3))
            else if (chexp(1:1).eq.' ') then
              call mshplt_text_upper(chexp(2:3))
            else
              call mshplt_text_upper(chexp(1:3))
            endif

          endif
        endif
      enddo

c axis-title

      if (titsiz.gt.0.0) then

        costitang=cos((titangrel+axang)*1.745329251994e-2)
        sintitang=sin((titangrel+axang)*1.745329251994e-2)
        call mshplt_set_text_angle(titangrel+axang)
        call mshplt_set_character_height(titsiz)
        x(1)=xmin+titposrel*(xmax-xmin)
        y(1)=ymin+titposrel*(ymax-ymin)
        call mshplt_view_to_world(x(1),y(1),x(1),y(1))
        x(1)=x(1)+titoff*sintitang
     &  -len_trim(chtit)*titsiz/4.*costitang
        y(1)=y(1)-titoff*costitang
     &    -len_trim(chtit)*titsiz/4.*sintitang
        call mshplt_world_to_view(x(1),y(1),x(1),y(1))
        call mshplt_text_raw(x(1),y(1),chtit)

      endif

9999  continue

      call mshplt_set_character_height(chheo)
      call mshplt_set_text_angle(ango)
      call mshplt_flush_buff

      write(cline_ps,*)'% end of mshplt_axis_taylor'
      write(lun_ps,'(a)')cline_ps(2:len_trim(cline_ps))

      return
      end
