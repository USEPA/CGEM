! gfortran -m64 -Wall -fsecond-underscore -g -static test_netcdf.f90 -o test_netcdf -Ilib/Linux.x86_64 -Llib/Linux.x86_64 lib/Linux.x86_64/libnetcdf.a
! gfortran -m64 -Wall -fsecond-underscore -g -static test_netcdf.f90 -o test_netcdf -Ilib/Win64 -Llib/Win64 lib/Win64/libnetcdf.a
! ./test_netcdf
! ncdump example.nc

program test_netcdf
!!use netcdf
  implicit none
  include 'netcdf.inc'
  integer ncid, dimid, varid, status
  integer starts(1), counts(1)
  real array(2)
  array(1) = 1.1
  array(2) = 2.2
  starts(1) = 1
  counts(1) = 2
  status = nf_create('example.nc', NF_CLOBBER, ncid)
  print *, status
  status = nf_def_dim(ncid, 'size', 2, dimid)
  print *, status
  status = nf_def_var(ncid, 'data', NF_FLOAT, 1, dimid, varid)
  print *, status
  status = nf_enddef(ncid)
  print *, status
  status = nf_put_vara_real(ncid, varid, starts, counts, array)
  print *, status
  status = nf_close(ncid)
  print *, status
end program test_netcdf
