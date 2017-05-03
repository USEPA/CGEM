module fill_value

!This is to generate NaN or other fill value for netCDF
!which=1 is for -9999.
!all others will be NaN
 
IMPLICIT NONE

contains

function fill(which)               RESULT(fv)

real fv
real x
integer, intent(in) :: which !What type of fill value?

if(which.eq.1) then

 fv = -9999.

else

 x=-1
 fv=sqrt(x)

 if(isnan(fv) .eqv. .FALSE.)then
 write(6,*) "didn't get NaN, fv=",fv 
 stop
 endif

endif


return

end function

end module fill_value
