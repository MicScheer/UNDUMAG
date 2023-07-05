*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.25/00 07/03/2018  10.31.03  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2018
      subroutine undumag_bwirenum(wire,xin,yin,zin,bxout,byout,bzout)

c Units: A, T and m

      use commandlinef90m

      implicit none

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      double precision xin,yin,zin,bxout,byout,bzout,
     &  rx,ry,rz,dxl,dyl,dzl,xl,yl,zl,dbx,dby,dbz,xc,yc,zc,r,
     &  bx,by,bz,wire(8),r3,curr,tiny

      data tiny/1.0d-6/

      integer iseg,nseg

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      curr=wire(1)
      nseg=nint(wire(2))

      xl=wire(6)-wire(3)
      yl=wire(7)-wire(4)
      zl=wire(8)-wire(5)

      dxl=xl/nseg
      dyl=yl/nseg
      dzl=zl/nseg

      xc=wire(3)-dxl/2.0d0
      yc=wire(4)-dyl/2.0d0
      zc=wire(5)-dzl/2.0d0

      bx=0.0d0
      by=0.0d0
      bz=0.0d0

      do iseg=1,nseg

        xc=xc+dxl
        yc=yc+dyl
        zc=zc+dzl

        rx=xin-xc
        ry=yin-yc
        rz=zin-zc

        r=sqrt(rx**2+ry**2+rz**2)
        r3=r**3

        if (r.lt.tiny) then
          write(lun6,*)'*** Error in undumag_bwirenum: Too close to wire'
          write(lun6,*)'*** Point: sngl(xin)*1000.,sngl(yin)*1000.,sngl(zin)*1000.'
          stop
        endif

        dbx=dyl*rz-dzl*ry
        dby=dzl*rx-dxl*rz
        dbz=dxl*ry-dyl*rx

        bx=bx+dbx/r3
        by=by+dby/r3
        bz=bz+dbz/r3

      enddo !nseg

      bxout=bxout+bx*curr*rmu04pi1
      byout=byout+by*curr*rmu04pi1
      bzout=bzout+bz*curr*rmu04pi1

      return
      end
