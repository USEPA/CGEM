       Subroutine Model_Finalize_GD(myid,numprocs)

       USE OUTPUT_NETCDF_GD

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

       CALL CLOSE_FILE()

       return

       End Subroutine Model_Finalize_GD
