module xnetcdf
!Parallel netCDF routines
include 'mpif.h'

contains

function ncdf_create(mpicomm,cname,inumber,info,ncid) result(res)
integer,intent(in) :: mpicomm,inumber,info
integer,intent(out) :: ncid
character(LEN=*), intent(in) :: cname
integer :: res
res=nfmpi_create(mpicomm,cname,inumber,info,ncid)
return
end

function ncdf_enddef(id) result(res)
integer,intent(in) :: id
integer :: res
res=nfmpi_enddef(id)
return
end

function ncdf_put_var_real(id,rvar,r) result(res)
integer,intent(in) :: id,rvar
real, intent(inout) :: r(*)
integer :: res
res=nfmpi_put_var_real(id,rvar,r)
return
end

function ncdf_put_vara_real(id,ivar,starts,counts,var,request) result(res)
integer,intent(in) :: id,ivar
integer(KIND=MPI_OFFSET_KIND),intent(in), dimension(*) :: starts, counts
real, intent(inout) :: var(*)
integer, intent(out) :: request
integer :: res
res=nfmpi_iput_vara_real(id,ivar,starts,counts,var,request)
return
end

function ncdf_put_vara_double(id,ivar,starts,counts,var,request) result(res)
!( FILE_ID, TIME_VAR, STARTS1, COUNTS1, SECONDS, REQUESTS(1))

integer,intent(in) :: id,ivar
integer(KIND=MPI_OFFSET_KIND),intent(in), dimension(*) :: starts, counts
double precision, intent(inout) :: var(*)
integer, intent(out) :: request
integer :: res
res=nfmpi_iput_vara_double(id,ivar,starts,counts,var,request)
return
end


function ncdf_inq_varid(id,cname,ivar) result(res)
integer,intent(in) :: id,ivar
character(LEN=*), intent(in) :: cname
integer :: res
res=nfmpi_inq_varid(id,cname,ivar)
return
end

function ncdf_close(id) result(res)
integer :: res
res=nfmpi_close(id)
return
end

function ncdf_open(mpicomm,cname,inumber,info,ncid) result(res)
integer,intent(in) :: mpicomm,inumber,info
integer,intent(out) :: ncid
character(LEN=*), intent(in) :: cname
integer :: res
res=nfmpi_open(mpicomm,cname,inumber,info,ncid)
return
end

function ncdf_sync(id) result(res)
integer,intent(in) :: id
integer :: res
res=nfmpi_sync(id)
return
end

function ncdf_wait_all(id,request_count,requests,statuses) result(res)
integer,intent(in) :: id,request_count,requests(request_count)
integer,intent(out) :: statuses(request_count)
integer :: res
res=nfmpi_wait_all(id,request_count,requests,statuses)
return
end

function ncdf_begin_indep_data(id) result(res)
integer,intent(in) :: id
integer :: res
res=nfmpi_begin_indep_data(id)
return
end


end module xnetcdf
