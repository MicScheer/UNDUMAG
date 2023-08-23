*CMZ :  2.04/10 23/08/2023  13.17.43  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  12.57.46  by  Michael Scheer
*CMZ :  2.04/06 29/03/2023  15.13.19  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/00 13/01/2023  11.36.00  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_br_inhom_init

      use magnets_structure

      implicit none

      double precision undumag_variable_getval,coef

      integer imag,i,ix,iy,iz,ipos(2,10),istat,nwords

      character(512) c512

      do imag=1,nmag_t+nspecmag_t

        do i=1,t_magnets(imag)%IsInhom

          c512=t_magnets(imag)%cinhom(i)
          call util_string_split(c512,10,nwords,ipos,istat)

          ix=nint(undumag_variable_getval(c512(ipos(1,2):ipos(2,2))))
          iy=nint(undumag_variable_getval(c512(ipos(1,3):ipos(2,3))))
          iz=nint(undumag_variable_getval(c512(ipos(1,4):ipos(2,4))))
          coef=undumag_variable_getval(c512(ipos(1,5):ipos(2,5)))

          write(t_magnets(imag)%cinhom(i),*) c512(ipos(1,1):ipos(2,1))," ",
     &      ix,iy,iz,coef

        enddo
      enddo !imag=1,nmag_t+nspecmag_t

      return
      end
