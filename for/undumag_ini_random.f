*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*-- Author :    Michael Scheer   19/10/2021
      subroutine undumag_ini_random

      use undumagf90m
      use commandlinef90m
      use bpolyederf90m

      implicit none

*KEEP,random.
      integer*8 irancalls
      integer, parameter :: irnsize=64
      integer irnseed(irnsize),irnmode,irnseedi(irnsize)
      common /randomc/ irancalls,irnseed,irnmode,irnseedi

      namelist /randomn/ irnmode,irnseed
*KEND.

      integer lunio,i,k

      if (irnmode.eq.1.or.irnmode.eq.2) then
        kundurun=0
        open(newunit=lunio,file="undumag.run",form='formatted',recl=512)
        read(lunio,*,end=9)kundurun
 9      kundurun=kundurun+1
        close(lunio)
        if (irnmode.eq.2) irnseed(12)=irnseed(12)+kundurun
        call util_random_set_seed(irnsize,irnseed)
      else if (irnmode<0) then
        open(newunit=lunio,file='undumag.seeds',status='old')
        read(lunio,*) k
        do i=1,irnsize
          read(lunio,*)k,irnseed(k)
        enddo
        close(lunio)
        call util_random_set_seed(irnsize,irnseed)
      else
        call util_random_init(irnsize,irnseed)
      endif

      randoxa=abs(randox)
      randoya=abs(randoy)
      randoza=abs(randoz)
      randox10=randoxa/10.0d0
      randoy10=randoya/10.0d0
      randoz10=randoza/10.0d0

      return
      end
