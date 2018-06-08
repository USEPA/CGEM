Module DailyRad 

IMPLICIT NONE

save

    real, dimension(:,:,:), allocatable :: aDailyRad ! previous day's irradiance per layer
    real, dimension(:,:,:), allocatable :: aRadSum   ! Add up current day's irradiance
 
contains

Subroutine DailyRad_allocate

USE Model_dim

IMPLICIT NONE

    allocate (aDailyRad(myim,jm,km)) ! previous day's irradiance per layer
    allocate (aRadSum(myim,jm,km))   ! Add up current day's irradiance
 
return

END Subroutine DailyRad_allocate 


END MODULE DailyRad

