*CMZ :  0.00/01 26/04/2016  08.59.08  by  Michael Scheer
*-- Author :    Michael Scheer   26/04/2016
C**********************************************************************
      subroutine uradbmap(lunbmap,xin,yin,zin,bxout,byout,bzout,
     &  tolarence,interpol,istatus)
C**********************************************************************
c Author: Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de

c NO WARRANTY

*KEEP,gplhint.
!******************************************************************************
!
!      Copyright 2013 Helmholtz-Zentrum Berlin (HZB)
!      Hahn-Meitner-Platz 1
!      D-14109 Berlin
!      Germany
!
!      Author Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de
!
! -----------------------------------------------------------------------
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy (wave_gpl.txt) of the GNU General Public
!    License along with this program.
!    If not, see <http://www.gnu.org/licenses/>.
!
!    Dieses Programm ist Freie Software: Sie koennen es unter den Bedingungen
!    der GNU General Public License, wie von der Free Software Foundation,
!    Version 3 der Lizenz oder (nach Ihrer Option) jeder spaeteren
!    veroeffentlichten Version, weiterverbreiten und/oder modifizieren.
!
!    Dieses Programm wird in der Hoffnung, dass es nuetzlich sein wird, aber
!    OHNE JEDE GEWAEHRLEISTUNG, bereitgestellt; sogar ohne die implizite
!    Gewaehrleistung der MARKTFAEHIGKEIT oder EIGNUNG FueR EINEN BESTIMMTEN ZWECK.
!    Siehe die GNU General Public License fuer weitere Details.
!
!    Sie sollten eine Kopie (wave_gpl.txt) der GNU General Public License
!    zusammen mit diesem Programm erhalten haben. Wenn nicht,
!    siehe <http://www.gnu.org/licenses/>.
!
!******************************************************************************
*KEND.

c Calculates magentic field (bxout, byout, bzout) from field map urad.bmap
c at (x,y,z), need F90 compiler or higher

c Input:
c ------
c      integer: lunbmap: LUN to open data file urad.bmap
c      integer: interpol: <0: linear interpolation,
c                             quadratic interpolation else

c real*8, point (x,y,z) in meter, where field is calculated
c tolarence: If x or z are more then this out of range, the field is set to
c            zero, and istatus is set. In y this value lower by a factor of 10.

c Output:
c -------

c      istatus: 0: ok
c       999: too few data
c       1001: out of range in x
c       1002: out of range in y
c       1003: out of range in z

c real*8 (bxout,byout,bzout): mag. field in Tesla

c Format:
c ---------

c Header lines
c Comment lines start with '!', '%', or '#'.
c Lines starting with '@ scaling = ' contains six scaling
c factors for x,y,z,Bx,By,Bz
c All following lines are data lines, containing x,y,z,Bx,By,Bz,
c where x is the longitudinal and y the vertical coordinate,
c (Bx,By,Bz) the mag. field.
c z must run fastest, than y, than x!!

      implicit none

      double precision, dimension (:,:), allocatable :: bmappe

      double precision xin,yin,zin,x,y,z,bx,by,bz,
     &  tolarence,tolarencey,xold,yold,
     &  bxout,byout,bzout,
     &  bmxmin,bmxmax,bmymin,bmymax,bmzmin,bmzmax,
     &  bmbxmin,bmbxmax,bmbymin,bmbymax,bmbzmin,bmbzmax,
     &  scalex,scaley,scalez,
     &  scalebx,scaleby,scalebz,bmapdy,bmapdz,x1,x2,a3(3),x3(3),b3(3),
     &  b11,b12,b13,b21,b22,b23,b31,b32,b33,bb1,bb2,bb3,dy,dz

      double precision b111(3),b211(3), b121(3),b221(3)
      double precision b112(3),b212(3), b122(3),b222(3)
      double precision b112111(3),b212211(3), b122121(3),b222221(3)
      double precision blow(3),bhig(3),b(3),dxx,dyy,dzz,xx,yy,zz,y1,z1

      integer lunbmap,istatus,interpol

      integer ical,nx,ny,nz,last,ntot,ianf,
     &  kd,ix1,ix2,iy1,iy2,iz1,iz2,nyz,i,k,
     &  kx1,kx2,kx3,ky1,ky2,ky3,kz1,kz2,kz3

      character(256) cline

      save

      data ical/0/
      data scalex,scaley,scalez/1.0d0,1.0d0,1.0d0/
      data scalebx,scaleby,scalebz/1.0d0,1.0d0,1.0d0/


      if (ical.eq.0) then

        bmxmin= 1.0d99
        bmxmax=-1.0d99
        bmymin= 1.0d99
        bmymax=-1.0d99
        bmzmin= 1.0d99
        bmzmax=-1.0d99

        bmbxmin= 1.0d99
        bmbxmax=-1.0d99
        bmbymin= 1.0d99
        bmbymax=-1.0d99
        bmbzmin= 1.0d99
        bmbzmax=-1.0d99

        open(unit=lunbmap,file="urad.bmap",status='old')
        nx=-1
        ny=-1
        nz=0
 1      read(lunbmap,'(a)',end=9),cline
        last=len_trim(cline)
        if (last.le.1.or.
     &      cline(1:1).eq.'%'.or.
     &      cline(1:1).eq.'*'.or.
     &      cline(1:1).eq.'!'.or.
     &      cline(1:1).eq.'#'.or.
     &      cline(1:1).eq.'@'.or.
     &      cline(1:2).eq.' %'.or.
     &      cline(1:2).eq.' *'.or.
     &      cline(1:2).eq.' !'.or.
     &      cline(1:2).eq.' #'.or.
     &      cline(1:2).eq.' @'
     &      ) then
          ianf=index(cline,"scaling")
          if (ianf.gt.0) then
            ianf=index(cline,"=")
            read(cline(ianf+1:last),*)scalex,scaley,scalez,
     &        scalebx,scaleby,scalebz
          endif
        else
          ntot=ntot+1
          read(cline(1:last),*)x,y,z,bx,by,bz
          x=x*scalex
          y=y*scaley
          z=z*scalez
          bx=bx*scalebx
          by=by*scaleby
          bz=bz*scalebz
          if (x.lt.bmxmin) bmxmin=x
          if (x.gt.bmxmax) bmxmax=x
          if (y.lt.bmymin) bmymin=y
          if (y.gt.bmymax) bmymax=y
          if (z.lt.bmzmin) bmzmin=z
          if (z.gt.bmzmax) bmzmax=z
          if (bx.lt.bmbxmin) bmbxmin=bx
          if (bx.gt.bmbxmax) bmbxmax=bx
          if (by.lt.bmbymin) bmbymin=by
          if (by.gt.bmbymax) bmbymax=by
          if (bz.lt.bmbzmin) bmbzmin=bz
          if (bz.gt.bmbzmax) bmbzmax=bz
          if (ntot.eq.1) then
            xold=x
            yold=y
          endif
          if (nx.eq.-1) then
            if (ny.eq.-1) then
              if (y.eq.yold) then
                nz=nz+1
              else
                ny=nz+1
              endif
            else if (x.eq.xold) then
              ny=ny+1
            else
              ny=ny/nz
              nx=0
            endif !ny
          endif !nx
        endif !line type
        goto 1
 9      rewind(lunbmap)

        nx=ntot/(ny*nz)

        if (nx.lt.2.and.interpol.lt.0.or.nx.lt.3.and.interpol.ge.0) then
          istatus=999
          return
        else
          allocate(bmappe(6,ntot))
        endif

        ntot=0
 11     read(lunbmap,'(a)',end=99),cline

        last=len_trim(cline)

        if (last.le.1.or.
     &      cline(1:1).eq.'%'.or.
     &      cline(1:1).eq.'*'.or.
     &      cline(1:1).eq.'!'.or.
     &      cline(1:1).eq.'#'.or.
     &      cline(1:1).eq.'@'.or.
     &      cline(1:2).eq.' %'.or.
     &      cline(1:2).eq.' *'.or.
     &      cline(1:2).eq.' !'.or.
     &      cline(1:2).eq.' #'.or.
     &      cline(1:2).eq.' @'
     &    ) then
        else
          ntot=ntot+1
          read(cline(1:last),*) x,y,z,bx,by,bz
          x=x*scalex
          y=y*scaley
          z=z*scalez
          bx=bx*scalebx
          by=by*scaleby
          bz=bz*scalebz
          bmappe(1,ntot)=x
          bmappe(2,ntot)=y
          bmappe(3,ntot)=z
          bmappe(4,ntot)=bx
          bmappe(5,ntot)=by
          bmappe(6,ntot)=bz
        endif
        goto 11
 99     close(lunbmap)

c        step=(bmappe(1,nx)-bmappe(1,1))/(nx-1)
        tolarencey=tolarence/10.0d0
        bmapdy=(bmymax-bmymin)/(ny-1)
        bmapdz=(bmzmax-bmzmin)/(nz-1)

        ix1=1
        ix2=ntot
        nyz=ny*nz

        ical=1

      endif !ical

      x=xin
      y=yin
      z=zin

      IF (
     &    X.LT.BMXMIN-tolarence.OR.X.GT.BMXMAX+tolarence
     &    ) THEN
        istatus=1001
        BXout=0.0d0
        BYout=0.0d0
        BZout=0.0d0
        RETURN
      ENDIF	

      IF (
     &    Y.LT.BMYMIN-tolarencey.OR.Y.GT.BMYMAX+tolarencey
     &    ) THEN
        istatus=1002
        BXout=0.0d0
        BYout=0.0d0
        BZout=0.0d0
        RETURN
      ENDIF	

      IF (
     &    Z.LT.BMZMIN-tolarence.OR.Z.GT.BMZMAX+tolarence
     &    ) THEN
        istatus=1003
        BXout=0.0d0
        BYout=0.0d0
        BZout=0.0d0
        RETURN
      ENDIF	

      if (x.lt.bmxmin) then
        ix1=1
        ix2=2
      else if (x.gt.bmxmax) then
        ix1=nx-1
        ix2=nx
      else

        if (x.ge.bmappe(1,(ix1-1)*nyz+1)) then
c hunt up
          kd=1
111       ix2=min(ix1+kd,nx)
          if (x.gt.bmappe(1,nyz*(ix2-1)+1)) then
            kd=2*kd
            ix1=ix2
            goto 111
          endif
        else    !(x.ge.bmappe(1,ix1))
c hunt down
          kd=1
          ix2=ix1
22        ix1=max(ix2-kd,1)
          if (x.lt.bmappe(1,(ix1-1)*nyz+1)) then
            kd=2*kd
            ix2=ix1
            goto 22
          endif
        endif

1111    if (ix2-ix1.gt.1) then
          k=(ix2+ix1)/2
          if(bmappe(1,(k-1)*nyz+1).gt.x)then
            ix2=k
          else
            ix1=k
          endif
          goto 1111
        endif
      endif

      x1=bmappe(1,(ix1-1)*nyz+1)
      x2=bmappe(1,(ix2-1)*nyz+1)
      dxx=(x-x1)/(x2-x1)

      if (y.lt.bmymin) then
        iy1=1
      else
        iy1=(y-bmymin)/bmapdy
        iy1=max(1,iy1)
      endif

      iy2=iy1+1

      if (iy2.gt.ny) then
        iy2=ny
        iy1=iy2-1
      endif

      if (z.lt.bmzmin) then
        iz1=1
      else
        iz1=(z-bmzmin)/bmapdz
        iz1=max(1,iz1)
      endif
      iz2=iz1+1
      if (iz2.gt.nz) then
        iz2=nz
        iz1=iz2-1
      endif

      if (interpol.lt.0) then

        dyy=(y-(bmymin+(iy1-1)*bmapdy))/bmapdy
        dzz=(z-(bmzmin+(iz1-1)*bmapdz))/bmapdz

        do i=1,3

          b111(i)=bmappe(3+i,iz1+(iy1-1)*nz+(ix1-1)*nyz)
          b211(i)=bmappe(3+i,iz2+(iy1-1)*nz+(ix1-1)*nyz)
          b121(i)=bmappe(3+i,iz1+(iy2-1)*nz+(ix1-1)*nyz)
          b221(i)=bmappe(3+i,iz2+(iy2-1)*nz+(ix1-1)*nyz)
          b112(i)=bmappe(3+i,iz1+(iy1-1)*nz+(ix2-1)*nyz)
          b212(i)=bmappe(3+i,iz2+(iy1-1)*nz+(ix2-1)*nyz)
          b122(i)=bmappe(3+i,iz1+(iy2-1)*nz+(ix2-1)*nyz)
          b222(i)=bmappe(3+i,iz2+(iy2-1)*nz+(ix2-1)*nyz)

          b112111(i)=b111(i)+(b112(i)-b111(i))*dxx
          b122121(i)=b121(i)+(b122(i)-b121(i))*dxx
          b212211(i)=b211(i)+(b212(i)-b211(i))*dxx
          b222221(i)=b221(i)+(b222(i)-b221(i))*dxx

          blow(i)=b112111(i)+(b212211(i)-b112111(i))*dzz
          bhig(i)=b122121(i)+(b222221(i)-b122121(i))*dzz

          b(i)=blow(i)+(bhig(i)-blow(i))*dyy

        enddo

      else

        if (ix1.gt.1) then
          kx1=ix1-1
          kx2=ix1
          kx3=ix1+1
        else
          kx1=ix1
          kx2=ix1+1
          kx3=ix1+2
        endif

        if (iy1.gt.1) then
          ky1=iy1-1
          ky2=iy1
          ky3=iy1+1
        else
          ky1=iy1
          ky2=iy1+1
          ky3=iy1+2
        endif

        if (iz1.gt.1) then
          kz1=iz1-1
          kz2=iz1
          kz3=iz1+1
        else
          kz1=iz1
          kz2=iz1+1
          kz3=iz1+2
        endif

        x3(1)=0.0d0

        x1=bmappe(1,kz1+(ky1-1)*nz+(kx1-1)*nyz)
        xx=x-x1

        y1=bmappe(2,kz1+(ky1-1)*nz+(kx1-1)*nyz)
        yy=y-y1
        dy=bmappe(2,kz1+(ky2-1)*nz+(kx1-1)*nyz)-y1

        z1=bmappe(3,kz1+(ky1-1)*nz+(kx1-1)*nyz)
        zz=z-z1
        dz=bmappe(3,kz2+(ky1-1)*nz+(kx1-1)*nyz)-z1

        do i=1,3

          x3(2)=bmappe(1,kz1+(ky1-1)*nz+(kx2-1)*nyz)-x1
          x3(3)=bmappe(1,kz1+(ky1-1)*nz+(kx3-1)*nyz)-x1

          b3(1)=bmappe(3+i,kz1+(ky1-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz1+(ky1-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz1+(ky1-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b11=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz1+(ky2-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz1+(ky2-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz1+(ky2-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b21=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz1+(ky3-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz1+(ky3-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz1+(ky3-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b31=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz2+(ky1-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz2+(ky1-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz2+(ky1-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b12=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz2+(ky2-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz2+(ky2-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz2+(ky2-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b22=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz2+(ky3-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz2+(ky3-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz2+(ky3-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b32=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz3+(ky1-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz3+(ky1-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz3+(ky1-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b13=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz3+(ky2-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz3+(ky2-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz3+(ky2-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b23=a3(1)+(a3(2)+a3(3)*xx)*xx

          b3(1)=bmappe(3+i,kz3+(ky3-1)*nz+(kx1-1)*nyz)
          b3(2)=bmappe(3+i,kz3+(ky3-1)*nz+(kx2-1)*nyz)
          b3(3)=bmappe(3+i,kz3+(ky3-1)*nz+(kx3-1)*nyz)
          call parabel_short(x3,b3,a3)
          b33=a3(1)+(a3(2)+a3(3)*xx)*xx

          x3(2)=dy
          x3(3)=x3(2)+dy

          b3(1)=b11
          b3(2)=b21
          b3(3)=b31
          call parabel_short(x3,b3,a3)
          bb1=a3(1)+(a3(2)+a3(3)*yy)*yy

          b3(1)=b12
          b3(2)=b22
          b3(3)=b32
          call parabel_short(x3,b3,a3)
          bb2=a3(1)+(a3(2)+a3(3)*yy)*yy

          b3(1)=b13
          b3(2)=b23
          b3(3)=b33
          call parabel_short(x3,b3,a3)
          bb3=a3(1)+(a3(2)+a3(3)*yy)*yy

          x3(2)=dz
          x3(3)=x3(2)+dz
          b3(1)=bb1
          b3(2)=bb2
          b3(3)=bb3
          call parabel_short(x3,b3,a3)
          b(i)=a3(1)+(a3(2)+a3(3)*zz)*zz

        enddo

      endif !interpol

      bxout=b(1)
      byout=b(2)
      bzout=b(3)

      return
      end
