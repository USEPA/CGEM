Subroutine DailyRad_allocate

USE DailyRad
USE Model_dim

IMPLICIT NONE

    allocate (aDailyRad(im,jm,nsl)) ! previous day's irradiance per layer
    allocate (aRadSum(im,jm,nsl))   ! Add up current day's irradiance
 
END Subroutine DailyRad_allocate 
