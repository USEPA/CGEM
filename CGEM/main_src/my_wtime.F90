function my_wtime()
include 'mpif.h'
real*8 my_wtime 
my_wtime = MPI_WTIME()
return
end function 
