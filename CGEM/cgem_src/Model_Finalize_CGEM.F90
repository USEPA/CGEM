       Subroutine Model_Finalize_CGEM(myid,numprocs)

       USE OUTPUT_NETCDF_CGEM

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

#ifdef DEBUG
       write(6,*) "myid,numprocs"
#endif
!       if(myid.eq.0) then
         CALL CLOSE_FILE()
!       endif

       return

       End Subroutine Model_Finalize_CGEM
