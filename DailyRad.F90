Module DailyRad 

IMPLICIT NONE

    real, dimension(:,:,:), allocatable :: aDailyRad ! previous day's irradiance per layer
    real, dimension(:,:,:), allocatable :: aRadSum   ! Add up current day's irradiance
 
END MODULE DailyRad 
