!----------------------------------------------------------------------
      Subroutine getSolar(                                             &
      &    lon       , lat    ,                                        & 
      &    iYr       ,                                                 &
      &    iMon      , iDay   ,                                        &    
      &    iHr       , iMin   ,                                        &     
      &    iSec      , Rad     )        
!----------------------------------------------------------------------
!     Written by  ::  D.S.Ko/NRL
!     Modified by ::  Barry Herchenroder/EMVL, April   2010
!                                              July    2011
!                                              Nov-Dec 2011
!
!     Calculate visible solar radiation irradiance
!                        using just the solar zenith and ignore
!                        the effects of clouds and atmospheric
!                        absorption and scattering.
! ---------------------------------------------------------------------
     USE Model_dim

     IMPLICIT NONE
     
!----------------------
! Interface variables:
!----------------------
      real   , intent(in)  :: lon(im)  ! longitude (deg E) at center of cell 
      real   , intent(in)  :: lat(jm)  ! latitude (deg N) at center of cell 
      integer, intent(in)  :: iYr      ! Year that Time_8 corresponds to
      integer, intent(in)  :: iMon     ! Month that Time_8 corresponds to 
      integer, intent(in)  :: iDay     ! Day that Time_8 corresponds to
      integer, intent(in)  :: iHr      ! Hour that Time_8 corresponds to
      integer, intent(in)  :: iMin     ! Minute that Time_8 corresponds to
      integer, intent(in)  :: iSec     ! Second that Time_8 corresponds to
      real   , intent(out) :: Rad(im,jm) ! Solar Radiation 
                                                                               
!-----------------------
! Local variables 
!-----------------------
      real, parameter :: cv        = 2.77e14 ! multiplicative factor used
                                                 ! to convert from watts/m2 
                                                 ! to photons/cm2/sec
                                                 ! Morel and Smith (1974)
      real(kind=8), parameter :: OneD60_8   =  1.0_8/60.0_8
      real(kind=8), parameter :: OneD3600_8 =  1.0_8/3600.0_8              
      real :: calc_solar_zenith
      integer  :: i,j  
      integer  :: jul_day
      integer  :: JY
      integer  :: mdate_julian
      real                :: rhr     ! decimal hr in the Julian Day 
      real(kind=8)        :: rhr_8   ! decimal hr in the Julian Day       
      real                :: solconst      
      real                :: Z
      LOGICAL :: leapyr  

!------------------------------------------------------------------------
! Temporarily, we will make the radiation only a function
! of time based on the zenth value and an approximate solar constant
! of 1200 watts/m2. We will use the function calc_solar_zenith to
! generate the solar zenith and the mdate_julian to generate a julian
! date/time  that approximates the GMT date/time that we need.
!-----------------------------------------------------------------------
	 rhr_8 = real(iHr,kind=8)                                        &
	 &     + OneD60_8*(real(iMin,kind=8) + OneD3600_8*real(iSec,kind=8)) 
         rhr   = real(rhr_8,kind=4)

         if (JY(iYr) == 1) then 
             leapyr = .FALSE.  ! iYr is not a leap-year
	 else 
	     leapyr = .TRUE.   ! iYr is a leap-year
	 endif 
	 
	 jul_day = mdate_julian(iMon,iDay,leapyr )
         
! Note in next line that 1200 is the average clear-sky solar constant
! in watts/m^2     
	 solconst = 1200.00 * cv  ! in photons/cm2/sec

!-----------------------------------	 
         do i = 1,im
         do j = 1,jm
           Z        = calc_solar_zenith(lat(j),lon(i),rhr,jul_day,leapyr) !in rad
           Rad(i,j) = solconst *AMAX1( COS(Z), 0.0)    ! COS(Z)<= 0 means night 
         enddo
         enddo
!-----------------------------------
        RETURN
           
      END Subroutine getSolar
