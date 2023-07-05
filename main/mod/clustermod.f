*CMZ :  4.01/00 13/03/2023  14.35.41  by  Michael Scheer
*CMZ :  4.00/06 05/12/2019  12.26.54  by  Michael Scheer
*CMZ :  3.07/01 28/03/2019  14.02.49  by  Michael Scheer
*CMZ :  3.02/00 10/09/2014  12.02.37  by  Michael Scheer
*CMZ :  2.66/09 24/03/2010  17.24.22  by  Michael Scheer
*CMZ :  2.66/07 10/03/2010  09.23.19  by  Michael Scheer
*CMZ :  2.66/06 24/11/2009  08.26.46  by  Michael Scheer
*CMZ :  2.66/04 16/11/2009  14.01.34  by  Michael Scheer
*CMZ :  2.66/03 11/11/2009  13.04.10  by  Michael Scheer
*-- Author :    Michael Scheer   03/11/2009
      module clustermod

      double precision wpspecnor,afspecnor

      integer icluster,kpid,master,iclubun
      character(256) wppath

      namelist/cluster/
     &  icluster, iclubun,
     &  wppath

      end module
