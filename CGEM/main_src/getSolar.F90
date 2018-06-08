!----------------------------------------------------------------------
      Subroutine getSolar( TC_8, lon, lat, Rad) 
!----------------------------------------------------------------------
!     Written by  ::  D.S.Ko/NRL
!
!     Calculate visible solar radiation irradiance
!                        using just the solar zenith and ignore
!                        the effects of clouds and atmospheric
!                        absorption and scattering.
! ---------------------------------------------------------------------
     USE Model_dim
     USE DATE_TIME

     IMPLICIT NONE
     
!----------------------
! Interface variables:
!----------------------
      real   , intent(in)  :: lon(im,jm)  ! longitude (deg E) at center of cell 
      real   , intent(in)  :: lat(im,jm)  ! latitude (deg N) at center of cell 
      integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0
      real   , intent(out) :: Rad(im,jm) ! Solar Radiation
      integer  :: iYr      ! Year that Time_8 corresponds to
      integer  :: iMon     ! Month that Time_8 corresponds to
      integer  :: iDay     ! Day that Time_8 corresponds to
      integer  :: iHr      ! Hour that Time_8 corresponds to
      integer  :: iMin     ! Minute that Time_8 corresponds to
      integer  :: iSec     ! Second that Time_8 corresponds to
 
!-----------------------
! Local variables 
!-----------------------
      real, parameter :: cv        = 2.77e14 ! multiplicative factor used
                                                 ! to convert from watts/m2 
                                                 ! to photons/cm2/sec
                                                 ! Morel and Smith (1974)
      real, parameter :: OneD60   =  1./60.
      real, parameter :: OneD3600 =  1./3600.              
      integer  :: i,j  
      integer  :: jday
      real                :: rhr         ! decimal hr in the Julian Day 
      real                :: Z   ! solar zenith angle 
      real calc_solar_zenith
      real                :: solconst      

         ! Note in next line that 1200 is the average clear-sky solar constant
         ! in watts/m^2
         solconst = 1200.00 * cv  ! in photons/cm2/sec
!-----------------------------------------------------------------------

         !Calculate Yr/Mon/Day from Model time in seconds	 
         call date_timestamp(iYr0,TC_8,iYr,iMon,iDay,iHr,iMin,iSec)

         !Hours in day
         rhr = real(iHr,4) + real(iMin,4)*OneD60 + real(iSec,4)*OneD3600

         !Day of the year
         jday = JDAY_IN_YEAR(iYr, iMon, iDay)

         do i = 1,im
         do j = 1,jm
           Z =  calc_solar_zenith(lat(i,j),lon(i,j),rhr,jday) !in rad
           Rad(i,j) = solconst * AMAX1( COS(Z), 0.0)    ! COS(suna)<= 0 means night 
         enddo
         enddo


        RETURN
           
      END Subroutine getSolar
