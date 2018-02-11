       Subroutine Model_Finalize_GD(myid,numprocs)

       USE OUTPUT_NETCDF_GD

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

       if(myid.eq.0) CALL CLOSE_FILE()

       return

       End Subroutine Model_Finalize_GD
