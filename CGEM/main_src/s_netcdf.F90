module xnetcdf
!Serial netCDF routines
use serial
include 'netcdf.inc'

contains

function ncdf_create(mpicomm,cname,inumber,info,ncid) result(res)
integer,intent(in) :: inumber,mpicomm,info
integer,intent(out) :: ncid
character(LEN=*), intent(in) :: cname
integer :: res
res=nf_create(cname,inumber,ncid)
return
end

function ncdf_enddef(id) result(res)
integer,intent(in) :: id
integer :: res
res=nf_enddef(id)
return
end

function ncdf_put_var_real(id,rvar,r) result(res)
integer,intent(in) :: id,rvar
real, intent(in) :: r(*)
integer :: res
res=nf_put_var_real(id,rvar,r)
return
end

function ncdf_put_vara_real(id,ivar,starts,counts,var,request) result(res)
integer,intent(in) :: id,ivar,request
integer,intent(in), dimension(*) :: starts, counts
real, intent(in) :: var(*)
integer :: res
res=nf_put_vara_real(id,ivar,starts,counts,var)
return
end

function ncdf_put_vara_double(id,ivar,starts,counts,var,request) result(res)
integer,intent(in) :: id,ivar,request
integer,intent(in), dimension(*) :: starts, counts
double precision, intent(in) :: var(*)
integer :: res
res=nf_put_vara_double(id,ivar,starts,counts,var)
return
end


function ncdf_inq_varid(id,cname,ivar) result(res)
integer,intent(in) :: id,ivar
character(LEN=*), intent(in) :: cname
integer :: res
res=nf_inq_varid(id,cname,ivar)
return
end

function ncdf_close(id) result(res)
integer,intent(in) :: id
integer :: res
res=nf_close(id)
return
end

function ncdf_open(mpicomm,cname,inumber,info,ncid) result(res)
integer,intent(in) :: mpicomm,inumber,info
integer,intent(out) :: ncid
character(LEN=*), intent(in) :: cname
integer :: res
res=nf_open(cname,inumber,ncid)
return
end

function ncdf_sync(id) result(res)
integer,intent(in) :: id
integer :: res
res=nf_sync(id)
return
end

function ncdf_wait_all(id,request_count,requests,statuses) result(res)
integer,intent(in) :: id,request_count,requests(request_count)
integer,intent(out) :: statuses(request_count)
integer :: res
statuses=0
res=0
return
end

function ncdf_begin_indep_data(id) result(res)
integer,intent(in) :: id
integer :: res
res=0 
return
end


end module xnetcdf
