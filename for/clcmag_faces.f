*CMZ :  2.05/02 31/10/2023  13.43.09  by  Michael Scheer
*CMZ :  2.04/17 12/09/2023  14.03.08  by  Michael Scheer
*CMZ :  2.04/13 03/09/2023  11.20.18  by  Michael Scheer
*CMZ :  2.04/09 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  12.58.25  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  12.43.30  by  Michael Scheer
*CMZ :  2.04/06 04/08/2023  11.32.02  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  19.30.13  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  20.28.35  by  Michael Scheer
*CMZ :  2.04/00 09/12/2022  11.08.58  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_faces

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      double precision p(3,3),wnorm(3),wcen(3)
      integer imag,k,npoi,l,ipoi,iface,nf
      character(128) ctype


      do imag=1,nmag_t+nspecmag_t

        ctype=t_magnets(imag)%ctype
        if (ctype.eq.'Cylinder') cycle
        nf=t_magnets(imag)%nface
        allocate(t_magnets(imag)%fcen(3,nf),t_magnets(imag)%fnorm(3,nf))
        k=0
        do iface=1,nf
          k=k+1
          t_magnets(imag)%lface(iface)=k
          npoi=t_magnets(imag)%kface(k)
          wcen=0.0d0
          do l=1,npoi
            k=k+1
            ipoi=t_magnets(imag)%kface(k)
            if (l.le.3) then
              p(1:3,l)=[t_magnets(imag)%xhull(ipoi),t_magnets(imag)%yhull(ipoi),t_magnets(imag)%zhull(ipoi)]
            endif
            wcen=wcen+[t_magnets(imag)%xhull(ipoi),t_magnets(imag)%yhull(ipoi),t_magnets(imag)%zhull(ipoi)]
          enddo
          call util_vcross(p(:,2)-p(:,1),p(:,3)-p(:,2),wnorm)
          wnorm=wnorm/norm2(wnorm)
          t_magnets(imag)%fnorm(:,iface)=wnorm
          wcen=wcen/dble(npoi)
          t_magnets(imag)%fcen(:,iface)=wcen
        enddo !iface
      enddo !imag

      return
      end
