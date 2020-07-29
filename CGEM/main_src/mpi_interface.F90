module mpi_interface

interface MPI_BCAST

subroutine BCAST_INT(a,b,c,d,e,f) 
integer :: a 
integer b,c,d,e,f
end subroutine

subroutine BCAST_REAL(a,b,c,d,e,f)
real :: a 
integer b,c,d,e,f 
end subroutine

subroutine BCAST_CHAR(a,b,c,d,e,f)
integer b,c,d,e,f
character :: a
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

subroutine BCAST_INT8(a,b,c,d,e,f) 
integer(kind=8) :: a 
integer b,c,d,e,f
end subroutine

subroutine BCAST_LOGICAL(a,b,c,d,e,f) 
logical :: a 
integer b,c,d,e,f
end subroutine

end interface


end module
