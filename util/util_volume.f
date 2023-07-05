*CMZ :  2.04/03 03/03/2023  15.00.22  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  18.38.39  by  Michael Scheer
*-- Author :    Michael Scheer   26/02/2023
      subroutine util_volume(n,xin,yin,zin,tiny,v,kfail)

      implicit none

      double precision :: tiny

      double precision xin(*),yin(*),zin(*),v,gc(3),
     &  p1(3),p2(3),p3(3),vnor(3),dist,a,rotmat(3,3),r(3),vrot(3)

      integer l,n,kfail,nhull,nedge,nface,iover,i,ipoi,iface,npoi,kfacelast,k
      double precision, dimension(:), allocatable :: x,y,z,hull,xr,yr,zr
      integer, dimension(:,:), allocatable :: kedge
      integer, dimension(:), allocatable :: kface,khull

      ! khull(n)
      ! kedge(4,2*n-2)
      ! kface((n+1)*n)

      allocate(x(n),y(n),z(n),hull(n),khull(n),xr(n),yr(n),zr(n),
     &  kedge(4,2*n-2),
     &  kface((n+1)*n)
     &  )

      x(1:n)=xin(1:n)
      y(1:n)=yin(1:n)
      z(1:n)=zin(1:n)

      call util_convex_hull_3d(
     &  n,x,y,z,khull,kedge,kface,nhull,nedge,nface,kfacelast,tiny,kfail)

      gc=0.0d0

      do i=1,nhull
        k=khull(i)
        gc=gc+[x(k),y(k),z(k)]
      enddo
      gc=gc/dble(n)

      v=0.0d0

      l=0
      do iface=1,nface
        npoi=kface(l+1)
        ipoi=kface(l+2)
        p1=[x(ipoi),y(ipoi),z(ipoi)]
        ipoi=kface(l+3)
        p2=[x(ipoi),y(ipoi),z(ipoi)]
        ipoi=kface(l+4)
        p3=[x(ipoi),y(ipoi),z(ipoi)]
        call util_plane(p1,p2,p3,gc,vnor,dist,iover,kfail)
        if (kfail.ne.0) then
          v=0.0d0
          goto 9999
        endif
        call util_matrix_to_rot_vec_to_z(vnor,rotmat,kfail)
        if (kfail.ne.0) then
          v=0.0d0
          goto 9999
        endif
        call util_mat_mul_vec_3x3(rotmat,vnor,vrot)
        do ipoi=1,npoi
          k=kface(l+1+ipoi)
          r(1)=x(k)
          r(2)=y(k)
          r(3)=z(k)
          call util_mat_mul_vec_3x3(rotmat,r,vrot)
          xr(ipoi)=vrot(1)
          yr(ipoi)=vrot(2)
          zr(ipoi)=vrot(3)
        enddo
        call util_area(npoi,xr,yr,tiny,a,kfail)
        if (kfail.ne.0) then
          v=0.0d0
          goto 9999
        endif
        v=v+a*abs(dist)/3.0d0
        l=l+npoi+1
      enddo

c      allocate(x(n),y(n),z(n),hull(n),kedge(4,2*n),kface(5*n),khull(n))
9999  continue

      deallocate(x,y,z,xr,yr,zr,hull,kedge,kface,khull)

      return
      end
