module serial

!Dummy variables for running without MPI

      integer :: MPI_COMM_WORLD !=0
      integer :: MPI_CHARACTER !=0
      integer :: MPI_INTEGER !=0
      integer :: MPI_REAL !=0
      integer :: MPI_DOUBLE_PRECISION !=0
      integer :: MPI_LOGICAL !=0
      !real*8 :: MPI_WTIME
      integer, parameter :: MPI_OFFSET_KIND=4
      integer :: MPI_COMM_SELF
 
end module
