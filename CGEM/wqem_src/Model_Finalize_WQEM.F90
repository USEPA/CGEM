       Subroutine Model_Finalize_WQEM(myid,numprocs)

       USE OUTPUT_NETCDF_WQEM

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

       IF (.FALSE.) WRITE(6,*) "myid, numprocs = ", myid, numprocs

       CALL CLOSE_FILE()

       return

       End Subroutine Model_Finalize_WQEM
