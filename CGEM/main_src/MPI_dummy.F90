
subroutine MPI_INIT(mierr)
integer mierr
!Dummy subroutine
return
end subroutine

subroutine MPI_COMM_RANK(comm,pid,mierr)
integer comm, pid, mierr
!Dummy subroutine
return
end subroutine 

subroutine MPI_COMM_SIZE(comm,pn,mierr)
integer comm, pn, mierr
!Dummy subroutine
return
end subroutine 

function MY_WTIME() result(out_sec)
implicit none
!Calculate run time in seconds 
!(call twice and subtract)
real in_sec
double precision out_sec
call cpu_time(in_sec)     !cpu time returns a real
out_sec=real(in_sec,8) !MPI_WTIME returns a double
return
end function


subroutine BCAST_INT(object,length,data_type,pid,comm,mierr)
integer length, data_type, pid, comm, mierr
integer object
!Dummy subroutine
return
end subroutine

subroutine BCAST_REAL(object,length,data_type,pid,comm,mierr)
integer length, data_type, pid, comm, mierr
real object
!Dummy subroutine
return
end subroutine

subroutine BCAST_CHAR(a,b,c,d,e,f)
integer b,c,d,e,f
character a
end subroutine

subroutine BCAST_vINT(a,b,c,d,e,f)
integer b,c,d,e,f
integer :: a(b)
end subroutine

subroutine BCAST_vREAL(a,b,c,d,e,f)
integer b,c,d,e,f
real :: a(b)
end subroutine

subroutine BCAST_vCHAR(a,b,c,d,e,f)
integer b,c,d,e,f
character :: a(b)
end subroutine

subroutine BCAST_v2INT(a,b,c,d,e,f)
integer b,c,d,e,f
integer :: a(b,1)
end subroutine

subroutine BCAST_v2REAL(a,b,c,d,e,f)
integer b,c,d,e,f
real :: a(b,1)
end subroutine

subroutine BCAST_v3INT(a,b,c,d,e,f)
integer b,c,d,e,f
integer :: a(b,1,1)
end subroutine

subroutine BCAST_v3REAL(a,b,c,d,e,f)
integer b,c,d,e,f
real :: a(b,1,1)
end subroutine


subroutine MPI_FINALIZE(mierr)
integer mierr
!Dummy subroutine
return
end subroutine

subroutine MPI_BARRIER(comm)
integer comm
!Dummy subroutine
return
end subroutine

subroutine MPI_INFO_CREATE(info,err)
integer info, err
!Dummy subroutine
return
end subroutine

subroutine MPI_INFO_SET(info,bsize,num,err)
integer info, bsize, num, err 
!Dummy subroutine
return
end subroutine

subroutine MPI_INFO_FREE(info,err)
integer info, err 
!Dummy subroutine
return
end subroutine

