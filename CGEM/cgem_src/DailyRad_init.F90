! This routine initializes the previous day's irradiance for use by the
! Cloern Chl:C algorithm.
Subroutine DailyRad_init(TC_8, lat, lon, d, d_sfc, A_k, CDOM_k, &
           & OM1A_k, OM1Z_k, OM1R_k, OM1BC_k, aDailyRad_k, nz)

  USE Model_dim ! For iYr0, etc.
  USE DATE_TIME ! For SECONDS_PER_DAY, TOTAL_SECONDS, DATE_TIMESTAMP
  use INPUT_VARS
  use INPUT_VARS_CGEM, ONLY:Qc,CChla
  use conversions

  implicit none

  ! Input variables
  integer(kind=8), intent(in) :: TC_8 ! model time in seconds after iYr0.
  real, intent(in) :: lat, lon ! latitude and longitude of current grid cell
  real, intent(in) :: d(km)   ! depth from surface to bottom of cell 
  real, intent(in) :: d_sfc(km)   ! depth cell center to surface 
  real, intent(in) :: A_k(nospA, km) !phytoplankton density in cells/m3
  real, intent(in) :: CDOM_k(km)
  real, intent(in) :: OM1A_k(km)
  real, intent(in) :: OM1Z_k(km)
  real, intent(in) :: OM1R_k(km)
  real, intent(in) :: OM1BC_k(km)
  integer, intent(in) :: nz !Number of Layers

  ! Output variables
  real, intent(out) :: aDailyRad_k(km)


  ! Local variables
  integer :: iYr
  integer :: iMon
  integer :: iDay
  integer :: iSec
  integer :: iHr, iMin ! Not used but needed for call
  integer :: k, isp
  logical :: leapyr
  integer :: jul_day
  integer :: JY ! function
  integer :: mdate_julian
  real :: rhr ! decimal hour with fraction thereof
  real :: bottom_depth(km)
  real :: Zenith,  calc_solar_zenith !Solar Zenith Angle
  real :: SfcRad ! solar irradiance just below the surface
  ! The avg. clear-sky solar energy is 1200 W/m2. Convert it to photons/cm2/sec
  real, parameter :: solconst = 1200.00 * 2.77e14  ! Morel and Smith (1974) 
  real, parameter :: RADCONV = 1./6.0221413*1.e-19 ! Convert quanta/cm2/s to mol quanta/m2/s
                                             ! mol/m2/s = quanta/cm2/s * 1 mol/Avogadro# * 10,000cm2/m2
                                             !          =  (1/6.022e23) * 1.0e4 = (1/6.022)e-23 * 1.0e4
                                             !          = (1/6.0221413)e-19
  real :: Chla_tot_k(km)! Total Chl-a concentration (mg/m3) 
  real :: aIOPpar(km), aRadBot ! Unused but needed for call
  real :: aRadMid(km) ! Holds desired output from Call_IOP_Par
  real :: aRadSum(km)

  ! Use fixed C:Chla to estimate chlorophyll a concentration
  do k = 1, nz
     Chla_tot_k(k) = 0.0
     do isp = 1, nospA
        Chla_tot_k(k) =  Chla_tot_k(k)  + A_k(isp,k) * Qc(isp) * 12. * (1./CChla(isp))
     enddo 
     ! Init for later
     aRadSum(k) = 0.0
     bottom_depth(k) = d(k) ! Depth from Surface to bottom of cell
  enddo 

  ! Convert time counter into date so we can get the
  ! day to calculate the correct solar path.

  CALL DATE_TIMESTAMP( iYr0, TC_8, iYr, iMon, iDay, iHr, iMin, iSec )

  if (JY(iYr) == 1) then
     leapyr = .FALSE.
  else 
     leapyr = .TRUE.
  endif 

  ! Assume that the day is from midnight to midnight
  jul_day = mdate_julian(iMon, iDay, leapyr)

  do iSec = 1, iSDay, dT
     rhr = REAL(iSec) / REAL(3600)
     Zenith = calc_solar_zenith(lat,lon,rhr,jul_day,leapyr)
     SfcRad = solconst * AMAX1( COS(Zenith), 0.0)    ! COS(Z)<= 0 means night

     if(SfcRad .gt. 0.) then
        Call Call_IOP_PAR(SfcRad, Zenith, CDOM_k, Chla_tot_k, &
             & OM1A_k, OM1Z_k, OM1R_k, OM1BC_k, bottom_depth, nz, d_sfc, aIOPpar, &
             & aRadBot, aRadMid)
        ! Add to running total
        aRadSum(:) = aRadSum(:) + aRadMid(:)
     endif
  enddo

  ! Copy result to return variable
  ! Need to convert from quanta/cm2/s to average mol quanta/m2/d
  aDailyRad_k(:) = aRadSum(:) * RADCONV * dT

END Subroutine DailyRad_init
