*CMZ :  4.01/00 13/03/2023  13.10.22  by  Michael Scheer
*CMZ :  3.07/01 21/03/2019  22.15.30  by  Michael Scheer
*CMZ :  3.05/03 17/05/2018  15.19.40  by  Michael Scheer
*-- Author : Michael Scheer
      module souintmod

      double precision, dimension (:), save, allocatable :: ampzmax
      integer, dimension (:), save, allocatable :: kobs

c!$OMP THREADPRIVATE(ampzmax,kobs)

      end module souintmod
