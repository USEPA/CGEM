!----------------------------------------------------------------------
      FUNCTION calc_solar_zenith(lat, lon, rhr, julianDay, leapyr )    &      
      &        RESULT(sunangle) 
      
!-----------------------------------------------------------------------
!     Original MATLAB code Written by: Brad Penta/NRL
!
!     Translated into FORTRAN and 
!     Revised by                     : Barry Herchenroder/EMVL, April 2010
!                                                               June 2011
!------------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Calculate the solar zenith angle (radians) given the latitude 
!  (lat, degrees) longitude (lon, degrees) of a point in the Gulf of 
!  Mexico (GOM) as well as the Julian day of the year (julianDay),
!  the hour in that day (ihr-integer), and whether julianDay is in
!  a leap year (leapyr == .TRUE.) or a non leap year (leapyr == .FALSE.)
!------------------------------------------------------------------------

  !--------------------------------------------------------------------------
  ! Definitions
  !
  ! sunangle     -> angle of the sun in radians, computed from latitude,
  !                 longitude, Julian day, and time--This is the zenith 
  !
  ! Compute the sun zenith angle in radians for a given Julian day (julianDay),
  ! GMT (ihr;in hours), latitude (lat), and longitude (lon).
  !
  ! leapyr      -> a logical parameter input thru the subroutine interface.
  !                 If true, then julianDay is in a leap year (366 days 
  !                 long). If false, then julianDay is in a non leap year 
  !                 (365 days long).      
  !  
  ! Sun angle of pi/2 radians (90 deg) = sun at or below horizon i.e. dark
  ! Sun angle of 0 radians (0 deg)     = sun directly overhead.
!------------------------------------------------------------------------------

  IMPLICIT NONE 
   
!-------------------------------------------------
! Declare variables in the interface
!------------------------------------------------- 
  REAL   , INTENT(IN)    :: lat          ! Latitude (deg); lat > 0 means
                                         !                 North of Equator
  
  REAL   , INTENT(IN)    :: lon          ! Longitude (deg E, 0 <= lon < 360); 
  
  REAL   , INTENT(IN)    :: rhr          ! decimal Hour of Julian Day  

  INTEGER, INTENT(IN)    :: julianDay    ! Julian Day GMT
  
  LOGICAL, INTENT(IN)    :: leapyr       ! IF true,  julianDay is in a leap
                                         !           year (366 days)
					 ! IF false, julianDay is in a non
                                         !           leap year (365 days)    
!-------------------------------------------------
! Local variables
!------------------------------------------------- 
  REAL   :: costmp
  REAL   :: sdec
  REAL   :: rad
  REAL   :: rlat 
  REAL   :: rsdec   
  REAL   :: rthez 
  REAL   :: sunangle  ! returned as RESULT 
  REAL   :: tc  
  REAL   :: thez
  REAL   :: xha
  REAL   :: Yr_length

!---------------------------------------------------
!  Compute solar declination angle
  rad = 180.0/ (4.0 * atan(1.0))             ! rad = radian to degrees 
                                             !       conversion factor
  IF(leapyr .eqv..FALSE.) THEN
      Yr_length = 365.0
  ELSE
      Yr_length = 366.0  
  ENDIF        

  thez = 360.0*((FLOAT(julianDay-1)) )/Yr_length ! thez = theta zero orbital 
                                                 !        position (degs)
  rthez = thez/rad;  ! in radians

!  sdec = solar declination angle in degrees
   sdec = 0.396372                                                    &
   &    -22.91327 *cos(rthez)     + 4.02543*sin(rthez)                & 
   &    - 0.387205*cos(2.0*rthez) + 0.051967*sin(2.0*rthez)           &
   &    - 0.154527*cos(3.0*rthez) + 0.084798*sin(3.0*rthez)           
   rsdec = sdec/rad; ! in radians

!  Time correction (tc) for solar hour angle
   tc = 0.004297                                                      &
   &  + 0.107029*cos(rthez)     - 1.837877*sin(rthez)                 &
   &  - 0.837378*cos(2.0*rthez) - 2.342824*sin(2.0*rthez)

!  xha = solar hour angle in degrees
   xha = (rhr-12.0 )*15.0 + lon + tc    
     
   IF (xha > 180.0) THEN
      xha = xha - 360.0
   ENDIF 
   IF (xha > -180.0) THEN
      xha = xha + 360.0
   ENDIF 

!  Sun zenith angle sunangle. 
! Cosine(sunangle) == costmp
! rsdec            == sun declination angle
! xha              == local hour angle  

   rlat = lat/rad
   costmp = sin(rlat)*sin(rsdec)                                      & 
   &      + cos(rlat)*cos(rsdec) * cos(xha/rad)

! The next block if is for failsafe purposes
   IF (abs(costmp) > 1.0) THEN
       IF (costmp > 0.0) THEN
           costmp = 1.0
       ELSE
           costmp = -1.0
       ENDIF 
   ENDIF 

   sunangle = acos(costmp)  ! solar zenith angle in radians. Note that
                            ! 2.0*atan(1.0)= Pi/2 radians= 90 deg

   IF (sunangle > (2.0*atan(1.0))) THEN
       sunangle = 2.0*atan(1.0);
   ENDIF 
 
   RETURN  
   END FUNCTION calc_solar_zenith   
