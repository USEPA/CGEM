      Subroutine AdvNeighbors(f,my_im,jm,nsl,nf,myid,numprocs)
 
!--------------------------------------------------------------------------
! Exchange guard cells for f, need 0 and im+2 
!------------------------------------------------------------------------

      IMPLICIT NONE
      include "mpif.h"

      real, intent(inout) :: f(0:(my_im+1),jm,nsl,nf)
      integer, intent(in) :: my_im,jm,nsl,nf,myid,numprocs
      integer tag
      integer mpierr,i,j,k,ii,status(MPI_STATUS_SIZE)
      real g(1,jm,nsl,nf)


      if(mod(myid,2).eq.0) then  !For even numbered processors
!Sending first array to processor below
        if(myid.ne.0) then
         i = 1
         tag = 1
         do j=1,jm
         do k=1,nsl
         do ii=1,nf
         g(1,j,k,ii) = f(i,j,k,ii)
         enddo
         enddo
         enddo
         call MPI_SEND(g,jm*nsl*nf,MPI_REAL,myid-1,tag,MPI_COMM_WORLD,mpierr)
       endif

       if(myid.ne.(numprocs-1)) then
         i = my_im + 1 
         tag = 1
         call MPI_RECV(g,jm*nsl*nf,MPI_REAL,myid+1,tag,MPI_COMM_WORLD,status,mpierr)
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         f(i,j,k,ii) = g(1,j,k,ii)
        enddo
        enddo
        enddo
       endif

!Sending last array to processor above
        if(myid.ne.(numprocs-1)) then
         i=my_im 
        tag = 0
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         g(1,j,k,ii) = f(i,j,k,ii)
        enddo
        enddo
        enddo
         call MPI_SEND(g,jm*nsl*nf,MPI_REAL,myid+1,tag,MPI_COMM_WORLD,mpierr)
       endif

       if(myid.ne.0) then
         i = 0
         tag = 0 
         call MPI_RECV(g,jm*nsl*nf,MPI_REAL,myid-1,tag,MPI_COMM_WORLD,status,mpierr)
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         f(i,j,k,ii) = g(1,j,k,ii)
        enddo
        enddo
        enddo
       endif

      else  !Odd numbered processors

!Sending first array to processor below 
       if(myid.ne.(numprocs-1)) then
         i = my_im + 1
         tag = 1
         call MPI_RECV(g,jm*nsl*nf,MPI_REAL,myid+1,tag,MPI_COMM_WORLD,status,mpierr)
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         f(i,j,k,ii) = g(1,j,k,ii)
        enddo
        enddo
        enddo
       endif

        if(myid.ne.0) then
         i=1
         tag = 1
         do j=1,jm
         do k=1,nsl
         do ii=1,nf
         g(1,j,k,ii) = f(i,j,k,ii)
         enddo
         enddo
         enddo
         call MPI_SEND(g,jm*nsl*nf,MPI_REAL,myid-1,tag,MPI_COMM_WORLD,mpierr)
       endif


!Sending last array to processor above 
       if(myid.ne.0) then
         i = 0
         tag = 0
         call MPI_RECV(g,jm*nsl*nf,MPI_REAL,myid-1,tag,MPI_COMM_WORLD,status,mpierr)
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         f(i,j,k,ii) = g(1,j,k,ii)
        enddo
        enddo
        enddo
       endif

        if(myid.ne.(numprocs-1)) then
         i = my_im 
        tag = 0
        do j=1,jm
         do k=1,nsl
         do ii=1,nf
         g(1,j,k,ii) = f(i,j,k,ii)
        enddo
        enddo
        enddo
         call MPI_SEND(g,jm*nsl*nf,MPI_REAL,myid+1,tag,MPI_COMM_WORLD,mpierr)
       endif
 
      endif

      RETURN
      END Subroutine AdvNeighbors 
