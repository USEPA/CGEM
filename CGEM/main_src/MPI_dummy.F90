subroutine MPI_INIT(mpierr)
integer mpierr
!Dummy subroutine
end

subroutine MPI_COMM_RANK(comm,pid,mpierr)
integer comm, pid, mpierr
!Dummy subroutine
END 

subroutine MPI_COMM_SIZE(comm,pn,mpierr)
integer comm, pn, mpierr
!Dummy subroutine
end 

function MPI_WTIME() result(out_sec)
implicit none
!Calculate run time in seconds 
!(call twice and subtract)
real in_sec
double precision out_sec
call cpu_time(in_sec)     !cpu time returns a real
out_sec=real(in_sec,8) !MPI_WTIME returns a double
return
end 

subroutine MPI_BCAST(object,length,data_type,pid,comm,mpierr)
integer length, data_type, pid, comm, mpierr
external object
!Dummy subroutine
end

subroutine MPI_FINALIZE(mpierr)
integer mpierr
!Dummy subroutine
end

subroutine MPI_BARRIER(comm)
integer comm
!Dummy subroutine
end

subroutine MPI_INFO_CREATE(info,err)
integer info, err
!Dummy subroutine
end

subroutine MPI_INFO_SET(info,bsize,num,err)
integer info, bsize, num, err 
!Dummy subroutine
end

subroutine MPI_INFO_FREE(info,err)
integer info, err 
!Dummy subroutine
end



