*CMZ :  2.04/00 13/01/2023  19.36.46  by  Michael Scheer
*CMZ :  2.02/01 27/04/2021  10.19.42  by  Michael Scheer
*CMZ :  2.02/00 18/08/2020  10.39.47  by  Michael Scheer
*CMZ :  2.01/08 28/07/2020  11.52.35  by  Michael Scheer
*CMZ :  2.01/05 21/07/2020  12.59.07  by  Michael Scheer
*CMZ :  1.25/00 24/01/2018  15.15.53  by  Michael Scheer
*CMZ :  1.17/08 30/05/2017  14.55.39  by  Michael Scheer
*CMZ :  1.11/01 09/01/2017  13.51.01  by  Michael Scheer
*CMZ :  1.11/00 06/12/2016  19.35.14  by  Michael Scheer
*CMZ :  1.10/02 25/11/2016  12.01.14  by  Michael Scheer
*CMZ :  1.10/00 11/11/2016  12.55.12  by  Michael Scheer
*CMZ :  1.02/00 22/08/2016  09.11.46  by  Michael Scheer
*CMZ :  0.00/10 13/07/2016  09.36.23  by  Michael Scheer
*CMZ :  0.00/09 30/06/2016  15.12.31  by  Michael Scheer
*CMZ :  0.00/05 10/06/2016  15.58.48  by  Michael Scheer
*CMZ :  0.00/04 11/05/2016  11.22.46  by  Michael Scheer
*-- Author :    Michael Scheer   10/05/2016
      subroutine undumag_sort_cpar(nparp,npar,cpar,par)

      implicit none

      integer :: lmin=1000000,lmax=0,nparp,l,ll

      double precision par(nparp),parcop(nparp)

      integer npar,i,kpar(nparp)

      character(128) cpar(nparp),cparcop(nparp), cword

      ! sort cpar by length

      parcop(:npar) = par(:npar)
      cparcop(:npar) = cpar(:npar)

      do i=1,npar
        cword=cparcop(i)
        ll=len_trim(cword)
        lmax=max(lmax,ll)
        lmin=min(lmin,ll)
      enddo

      kpar=0
      do l=lmax,lmin,-1
        do i=1,npar
          cword=cparcop(i)
          ll=len_trim(cword)
          if (ll.eq.l) then
            kpar=kpar+1
            cpar(kpar) = cparcop(i)
            par(kpar) = parcop(i)
          endif
        enddo
      enddo

      return
      end
