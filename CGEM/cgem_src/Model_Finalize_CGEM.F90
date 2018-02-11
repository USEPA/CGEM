       Subroutine Model_Finalize_CGEM(myid,numprocs)

       USE OUTPUT_NETCDF_CGEM

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

       if(myid.eq.0) CALL CLOSE_FILE()

       return

       End Subroutine Model_Finalize_CGEM
