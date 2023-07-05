*CMZ :  1.17/07 22/05/2017  08.48.22  by  Michael Scheer
*-- Author :    Michael Scheer   21/05/2017
      subroutine undumag_cornpoles_fit(nmag,npoimax,cornpoles,ifail)

      implicit none

      integer nmag,npoimax,ifail,npoi,narg,nfun,ndimpoi

      double precision cornpoles(4,npoimax,nmag)
      double precision, dimension (:), allocatable :: param
      double precision, dimension (:,:), allocatable :: fundata,a,t

      ifail=0

      ndimpoi=npoimax/8+8

c      call util_linar_fit(ifail,3,3,ndimpoi,npoi,narg,nfun,a,t,fundata)

      return
      end
