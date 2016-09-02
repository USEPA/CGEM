!======================================================================     
    Subroutine GEM_EPA( lat, lon, TC_8, f, d, d_sfc, dz, Rad, PARbot_ij, T, S, pH, &
     & wsm, fm, CBODW, istep, iout, istep_out)       
!======================================================================
     USE Model_dim
     USE INPUT_VARS
     USE Which_Flux
     USE DATE_TIME
     USE OUTPUT_NETCDF
     USE CGEM_vars 
     USE Calc_Chla
     USE MOD_EPACOM_GEM_UTILITIES
     USE mvars
     USE STOICH_VARS
     USE MASS_BALANCE
     USE DailyRad

      IMPLICIT NONE

!---------------------------------------------
! Interface variables
!---------------------------------------------------------------------
    real,    intent(in)  :: lat(jm), lon(im)    ! Latitude and longitude
    integer(kind=8), intent(in) :: TC_8         ! Model time (seconds from beginning of Jan 1, 2002)
    real, intent(inout)  :: f(im,jm,nsl,nf) ! Array that holds the nf state variables
    real,    intent(in)  :: Rad(im,jm)  ! Irradiance just below sea level (quanta/cm2/sec)
    real,    intent(in)  :: T(im,jm,nsl), S(im,jm,nsl)  ! Temperature (Celsius) and salinity 
    real,    intent(in)  :: d(im,jm,nsl), d_sfc(im,jm,nsl)  ! Total Depth (m), depth from surface to cell center (m)
    real,    intent(in)  :: dz(im,jm,nsl)  ! Cell thickness (m)
    integer, intent(in)  :: istep     ! Current time step
    integer, intent(in)  :: iout      ! Output time-interval in timesteps
    integer, intent(in)  :: istep_out      ! Current output step 
    integer, intent(in)  :: fm(im,jm)             ! Land(0)/sea(1) mask
    real,    intent(in)  :: wsm(im,jm)            ! Shelf(0)/open ocean(1) mask
    real,    intent(out) :: PARbot_ij(im,jm)     ! Irradiance at sea bottom (quanta/cm2/sec)
    real,    intent(out) :: CBODW(im,jm)      ! Chemical Biological Oxygen Demand in the Water 
!---------------------------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------
    real ::  ff(im,jm,nsl,nf)        ! Holds the nf state vectors
    integer        ::  i, j, k, isp, isz ! Loop indicies, isp/isz is for phytoplankton/zooplankton species
    integer, save  ::  init  = 1         ! Declare some variables only at first subroutine call
    integer        ::  Is_Day            ! Switch for day/night for phytoplankton nutrient uptake only, Is_Day=0 means night
!------------------------------------ 
! Variables to hold netCDF output
    real    :: PAR_percent_ijk(im,jm,nsl) ! Percent Irradiance at mid cell (quanta/cm2/sec)
    real    :: PARdepth_ijk(im,jm,nsl)    ! Percent Irradiance at mid cell (quanta/cm2/sec) 
    real    :: uN_ijk(im,jm,nsl,nospA)    ! Nitrogen Limited growth rate (1/d)
    real    :: uP_ijk(im,jm,nsl,nospA)    ! Phosphorus limited growth rate (1/d)
    real    :: uE_ijk(im,jm,nsl,nospA)    ! Light limited growth rate (1/d)
    real    :: uSi_ijk(im,jm,nsl,nospA)   ! Silica limited growth rate (1/d)
    real    :: uA_ijk(im,jm,nsl,nospA)    ! Specific growth rate (1/d)
    real    :: Chla_tot_ijk(im,jm,nsl)    ! Total Chl-a concentration from all phytoplankton (mg/m3)
    real    :: Chl_C_ijk(im,jm,nsl,nospA)    ! Chl:C from each phytoplankton species
!-------------------------------------------------------------------------
! Phytoplankton parameters
 !Phytoplankton uptake and growth
    real, dimension(nospA,nsl) :: A_k      ! Phytoplankton number density (cells/m3)
    real    :: Agrow                       ! Phytoplankton growth (cells/m3/d)
    real, dimension(nospA,nsl) :: Agrow_k  ! Phytoplankton growth (cells/m3/d)
    real    :: uA                 ! Specific growth rate (1/d)
    real    :: uA_k(nsl,nospA)    ! Specific growth rate (1/d)
    real    :: uN_k(nsl,nospA)    ! Nitrogen Limited growth rate (1/d)
    real    :: uP_k(nsl,nospA)    ! Phosphorus limited growth rate (1/d)
    real    :: uE_k(nsl,nospA)    ! Light limited growth rate (1/d)
    real    :: uSi_k(nsl,nospA)   ! Silica limited growth rate (1/d)
    real    :: f_Qn(nospA)        ! Quota model for N
    real    :: f_Qp(nospA)        ! Quota model for P
    real    :: Qn,Qn_k(nospA,nsl) ! Phytoplankton Nitrogen Quota (mmol-N/cell)
    real    :: Qp,Qp_k(nospA,nsl) ! Phytoplankton Phosphorus Quota (mmol-P/cell)
    real    :: vN    ! Phytoplankton uptake rate of Nitrogen (mmol-N/cell/d)
    real    :: vP    ! Phytoplankton uptake rate of Phosphorus (mmol-P/cell/d)
    real    :: vSi   ! Phytoplankton uptake rate of Silica (mmol-Si/cell/d)
    real    :: AupN  ! Total Phytoplankton uptake of Nitrogen (mmol-N/m3/d)
    real    :: AupP  ! Total Phytoplankton uptake of Phosphorus (mmol-P/m3/d)
    real    :: AupSi ! Total Phytoplankton uptake of Silica (mmol-Si/m3/d)
    integer :: RLN   ! Rate Limiting Nutrient of N, P, and Si
 !Monod equations for phytoplankton
    real, dimension(nospA)    :: monodN  !Monod term in nitrogen uptake
    real, dimension(nospA)    :: monodP  !Monod term in phosphorus uptake
    real, dimension(nospA)    :: monodSi !Monod term in Si uptake
    real    :: Ntotal   ! Total N (mmol/m3)
 !Phytoplankton nutrient loss
    real, dimension(nospA)    :: Amort ! Dead phytoplankton (cells/m3/day)
    real, dimension(nospA)    :: AexudN_A    ! Phytoplankton group exudation (mmol-N/cell/d)
    real, dimension(nospA)    :: AexudP_A    ! Phytoplankton group exudation (mmol-P/cell/d) 
    real    :: AexudN          ! Sum of Exudation of N from all phytoplankton groups (mmol-N/m3/d)
    real    :: AexudP          ! Sum of Exudation of P from all phytoplankton groups (mmol-P/m3/d)
    real    :: Aresp           ! Total respiration from a phytoplankton group (cells/m3/d)
    real    :: Aresp_k(nospA,nsl) ! Total respiration from a phytoplankton group (cells/m3/d)
    real    :: ArespC          ! Phytoplankton equivalent carbon loss from respiration (mmol-C/m3/d)
!------------------------------------------------------------------
! Zooplankton parameters
 !Zooplankton uptake and growth
    real, dimension(nospZ)       :: optNP    ! Optimal nutrient ratio for zooplankton
    real, dimension(nospZ)       :: Z        ! Zooplankton
    real, dimension(nospZ)       :: Zgrow    ! Zooplankton growth (indv./m3/d)
    real, dimension(nospA,nospZ) :: Zgrazvol ! Grazing rate in units of biovolume (um3/m3/d)
    real, dimension(nospA,nospZ) :: ZgrazA   ! Zooplankton grazing of phytoplankton (cells/m3/d)
    real, dimension(nospA)       :: ZgrazA_tot ! Total zooplankton grazing of phytoplankton (cells/m3/d)
    real, dimension(nospZ)       :: ZgrazN   ! Zooplankton grazing uptake of Nitrogen (mmol-N/m3/d)
    real, dimension(nospZ)       :: ZgrazP   ! Zooplankton grazing uptake of Phosphorus (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZgrazC   ! Zooplankton grazing uptake of Carbon (mmol-C/m3/d)
    real, dimension(nospZ)       :: ZinN     ! Zooplankton ingestion of Nitrogen (mmol-N/m3/d)
    real, dimension(nospZ)       :: ZinP     ! Zooplankton ingestion of Phosphorus (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZinC     ! Zooplankton ingestion of Carbon (mmol-C/m3/d)
 !Monod equations for zooplankton ingestion of phytoplankton
    real, dimension(nospA,nospZ) :: monodZ   ! Monod term for zooplankton grazing
    real                         :: Abiovol  ! Algae biovolume vector (um3/m3)
    real, dimension(nospA,nospZ) :: top_A    ! Monod numerator value for phytoplankton group
    real, dimension(nospA,nospZ) :: bottom_A ! Monod Denominator value for phytoplankton group
    real, dimension(nospZ)       :: bottom   ! Sum of Monod Denominator value for all phytoplankton groups
 !Zooplankton nutrient loss
    real, dimension(nospZ)       :: Zresp    ! Zooplankton respiration (individuals/m3/d)
    real                         :: ZrespC   ! Carbon loss from zooplankton respiration (mmol-C/m3/day)
    real, dimension(nospZ)       :: ZunC     ! Unassimilated ingested Carbon (mmol-C/m3/d)
    real, dimension(nospZ)       :: ZunN     ! Unassimilated ingested Nitrogen (mmol-N/m3/d)
    real, dimension(nospZ)       :: ZunP     ! Unassimilated ingested Phosphorus (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZunSi    ! Unassimilated ingested Silica (mmol-Si/m3/d)
    real, dimension(nospZ)       :: Zmort    ! Dead zooplankton (individuals/m3/d)
    real :: ZmortC(nospZ), ZmortC_tot        ! Carbon released from dead zooplankton (mmol-C/m3/d)
    real :: ZmortN(nospZ), ZmortN_tot        ! Nitrogen released from dead zooplankton (mmol-N/m3/d)
    real :: ZmortP(nospZ), ZmortP_tot        ! Phosphorus released from dead zooplankton (mmol-P/m3/d)
    real :: ZslopC(nospZ), ZslopC_tot        ! Carbon lost to sloppy feeding (mmol-C/m3/d)
    real :: ZslopN(nospZ), ZslopN_tot        ! Nitrogen lost to sloppy feeding (mmol-N/m3/d)
    real :: ZslopP(nospZ), ZslopP_tot        ! Phosphorus lost to sloppy feeding (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZexN     ! Excretion from zooplankton (mmol-N/m3/d)
    real, dimension(nospZ)       :: ZexP     ! Excretion from zooplankton (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZegC     ! Egestion from zooplankton (mmol-C/m3/d)
    real, dimension(nospZ)       :: ZegN     ! Egestion from zooplankton (mmol-N/m3/d)
    real, dimension(nospZ)       :: ZegP     ! Egestion from zooplankton (mmol-P/m3/d)
    real, dimension(nospZ)       :: ZegSi    ! Egestion from zooplankton (mmol-Si/m3/d)
    real :: OM1_Ratio, OM2_Ratio             ! Separates sloppy feeding into OM1 and OM2
!---------------------------------------------------------------------- 
! Time variables  
    real, parameter :: SDay       = 86400.0   ! Seconds in a day
    real, parameter :: one_d_365  = 1.0/365.0 ! Convert 1/yr to 1/day
    real, parameter :: OneD60     = 1.0/60.0  ! Convert 1/min to 1/sec
    real, save      :: dTd           ! Timestep in days 
    integer, save   :: StepsPerDay   ! Time steps per day
    real            :: HrTC          ! Decimal hour of day
    integer         :: iYrTC, iMonTC, iDayTC, iHrTC, iMinTC, iSecTC !Time variables
    integer         :: JY            ! Function determines whether argument is a leap year
    integer         :: julianDay     ! Holds Julian Day
    integer         :: mdate_julian  ! Function calculates Julian Day
    logical         :: leapyr        ! Logical, is leap year 
!-----------------------------------------------------------------------
! Organic Matter Calculations
   ! Variables to calculate stoichiometry C:N:P ratios
    real    :: OM1_CA, OM1_NA, OM1_PA    ! OM from dead phytoplankton
    real    :: OM2_CA, OM2_NA, OM2_PA   
    real    :: OM1_CZ, OM1_NZ, OM1_PZ    ! OM from zooplankton
    real    :: OM2_CZ, OM2_NZ, OM2_PZ
    real stoich_x1A, stoich_y1A, stoich_z1A ! Stoichiometry for OM1_A
    real stoich_x2A, stoich_y2A, stoich_z2A ! Stoichiometry for OM2_A
    real stoich_x1Z, stoich_y1Z, stoich_z1Z ! Stoichiometry for OM1_Z
    real stoich_x2Z, stoich_y2Z, stoich_z2Z ! Stoichiometry for OM2_Z
!---------------------------------------------------------------------------
! reaction and Nitrification subroutine variables
    real    :: NO3, NH4, O2, DIC, Si, PO4               ! Nutrient input to subroutines
    real    :: OM1_A, OM1_Z, OM1_R, OM2_A, OM2_Z, OM2_R ! OM inputs to subroutines
    real    :: OM1_BC, OM2_BC                           ! OM inputs to subroutines
    real    :: R_11                                     ! Nitrification term
    real    :: RNO3, RNO3_A, RNO3_Z, RNO3_R, RNO3_BC ! Remineralization terms for NO3
    real    :: RNH4, RNH4_A, RNH4_Z, RNH4_R, RNH4_BC ! Remineralization terms for NH4
    real    :: ROM1_A, ROM1_Z, ROM1_R, ROM1_BC       ! Remineralization terms for POC
    real    :: ROM2_A, ROM2_Z, ROM2_R, ROM2_BC       ! Remineralization terms for DOC
    real    :: RO2, RO2_A, RO2_Z, RO2_R, RO2_BC      ! Remineralization terms for O2
    real    :: RPO4, RPO4_A, RPO4_Z, RPO4_R, RPO4_BC ! Remineralization terms for PO4
    real    :: RDIC, RDIC_A, RDIC_Z, RDIC_R, RDIC_BC ! Remineralization terms for DIC
    real    :: RSi, RSi_A, RSi_Z, RSi_R, RSi_BC      ! Remineralization terms for Si
    real    :: RALK, RALK_A, RALK_Z, RALK_R, RALK_BC ! Remineralization terms for ALK
    real    :: RN2, RN2_A, RN2_Z, RN2_R, RN2_BC ! Remineralization terms for N2 
    real, dimension(10) :: RC   ! Array that returns remineralization terms for OM
!---------------------------------------------------------
! Variables needed for light routine and calc_Agrow
    real    :: SunZenithAtm       ! Solar beam zenith angle
    real    :: calc_solar_zenith  ! Function, calculates solar beam zenith angle
    real    :: Katt               ! Attenuation coefficient for Irradiance model 2 
    real    :: tmpexp             ! Intermediate calculation
    real    :: PARbotkm1          ! Irradiance at bottom of layer k-1 (quanta/cm2/s)
    real    :: PARtopk            ! Irradiance at top of layer k (quanta/cm2/s)
    real    :: PARsurf            ! Irradiance just below the sea surface (quanta/cm2/s) 
    real    :: PARbot             ! Irradiance at sea floor (quanta/cm2/s)
    real    :: PARdepth_k(nsl)    ! Irradiance at center of layer k (quanta/cm2/s)
    real    :: PAR_percent_k(nsl) ! Percent irradiance at center of layer k (quanta/cm2/s)
    real       :: aDailyRad_k(nsl), aRadSum_k(nsl)
    real, parameter :: RADCONV = 1./6.0221413*1.e-19 ! Convert quanta/cm2/s to mol/m2/s:
                                               !  = quanta/cm2/s * 1 mol/Avogadro# * 10,000cm2/m2
                                               !  = (1/6.022e23) * 1.0e4 = (1./6.022)e-23 * 1.0e4
                                               !  = (1./6.0221413)*1.e-19
    real, dimension(nsl) :: Chla_tot_k  ! Total amount of Chl-a in all the
                                        !  phytoplankton species (mg/m3) per cell
    real, dimension(nospA,nsl) :: Chl_C_k     ! Chl:C
    real, dimension(nsl) :: OM1A_k, OM1Z_k, OM1SPM_k, OM1BC_k !POC in g/m3
    real, dimension(nsl) :: CDOM_k    ! CDOM, ppb
    real, dimension(nsl) :: N_k       ! Nitrogen, mmol/m3
    real, dimension(nsl) :: P_k       ! Phosphorus, mmol/m3
    real, dimension(nsl) :: Si_k      ! Silica, mmol/m3
    real, dimension(nsl) :: S_k, T_k  ! Salinity and Temperature(Celsius)
    real, parameter :: C_cf  = 12.0E-3    ! C conversion factor (mmol-C/m3 to g-C/m3) 
!-----------------------------------------------------------------------
! Other variables 
    real :: PrimProd                     ! Primary production (photosynthesis)
    real, dimension(nospA+nospZ) :: Tadj ! Temperature adjustment factor
    real :: Q10_T                        ! Temperature adjustment Q10 relation
!------------------------------------------------------------------    
! SAVE KGs for instant remineralization
    real, save :: KG1_save, KG2_save
!------------------------------------------------------------------
!Output vars for alkalinity subroutine:
    real, intent(out) :: pH(im,jm,nsl)
    real :: ph_calc(1), pco2_calc(1), fco2(1), co2(1), hco3(1), co3(1), omegaa(1), omegac(1), betad_calc(1) 
    real :: rhosw(1), p(1), tempis(1)
    real :: patm(1) = 1.
    real :: m_alk(1), m_dic(1), m_si(1), m_po4(1)
!MASS BALANCE
    real :: DIFF_A, SUM_DA

   if(init.eq.1) then  

!Initialize netCDF output variables
  RN2_ijk = -9999.
  Chl_C_ijk = -9999.
  Chla_tot_ijk = -9999.
  Parbot_ij = -9999.
  PAR_percent_ijk = -9999.
  uN_ijk = -9999.
  up_ijk = -9999.
  uE_ijk = -9999.
  uA_ijk = -9999.
  uSi_ijk = -9999.

      !Initial Stoichiometry of POC and DOC:
       s_x1A= Stoich_x1A_init
       s_x2A= Stoich_x2A_init
       s_y1A= Stoich_y1A_init
       s_y2A= Stoich_y2A_init
       s_z1A= Stoich_z1A_init
       s_z2A= Stoich_z2A_init
       s_x1Z= Stoich_x1Z_init
       s_x2Z= Stoich_x2Z_init
       s_y1Z= Stoich_y1Z_init
       s_y2Z= Stoich_y2Z_init
       s_z1Z= Stoich_z1Z_init
       s_z2Z= Stoich_z2Z_init


  do j = 1,jm
    do i = 1,im 
      if(fm(i,j).eq.0) then
       s_x1A(i,j,:) = -9999.
       s_x2A(i,j,:)=  -9999.
       s_y1A(i,j,:)=  -9999.
       s_y2A(i,j,:)=  -9999.
       s_z1A(i,j,:)=  -9999.
       s_z2A(i,j,:)=  -9999.
       s_x1Z(i,j,:)=  -9999.
       s_x2Z(i,j,:)=  -9999.
       s_y1Z(i,j,:)=  -9999.
       s_y2Z(i,j,:)=  -9999.
       s_z1Z(i,j,:)=  -9999.
       s_z2Z(i,j,:)=  -9999.
      endif  
      if(wsm(i,j).eq.0.) then
       s_x1A(i,j,nsl) = -9999.
       s_x2A(i,j,nsl)=  -9999.
       s_y1A(i,j,nsl)=  -9999.
       s_y2A(i,j,nsl)=  -9999.
       s_z1A(i,j,nsl)=  -9999.
       s_z2A(i,j,nsl)=  -9999.
       s_x1Z(i,j,nsl)=  -9999.
       s_x2Z(i,j,nsl)=  -9999.
       s_y1Z(i,j,nsl)=  -9999.
       s_y2Z(i,j,nsl)=  -9999.
       s_z1Z(i,j,nsl)=  -9999.
       s_z2Z(i,j,nsl)=  -9999.
      endif
  enddo  
  enddo 


       dTd = dT/SDay           ! Timestep length in units of days
       StepsPerDay = SDay / dT ! Time steps in a day

       ! Initialize previous day's irradiance for Chl:C calculation
       ! These duplicated lines execute only once for init
       do j = 1,jm
          do i = 1,im 
             if(fm(i,j).eq.1) then
                do k = 1, nz
                   do isp = 1, nospA          
                      A_k(isp,k) = f(i,j,k,isp) ! Phytoplankton in group isp, cells/m3
                   enddo 
                   CDOM_k(k)  = f(i,j,k,iCDOM)  ! CDOM is in ppb
                                 ! Convert mmol/m3 to g carbon/m3; CF_SPM is river specific
                                 ! and converts river OM to riverine SPM
                   OM1SPM_k(k) = (f(i,j,k,iOM1_R) * C_cf) / CF_SPM 
                   OM1Z_k(k)   = f(i,j,k,iOM1_Z)  * C_cf   ! Convert mmol/m3 to g carbon/m3
                   OM1A_k(k)   = f(i,j,k,iOM1_A)  * C_cf   ! Convert mmol/m3 to g carbon/m3
                   OM1BC_k(k)  = f(i,j,k,iOM1_BC) * C_cf   ! Convert mmol/m3 to g carbon/m3
                enddo 
                call DailyRad_init(TC_8, lat(j), lon(i), d(i,j,:), d_sfc(i,j,:), A_k, &
                     & CDOM_k, OM1A_k, OM1Z_k, OM1SPM_k, OM1BC_k, aDailyRad_k)
                aDailyRad(i,j,:) = aDailyRad_k(:)
             endif    
          enddo     
       enddo     

    init=0

     KG1_save = KG1
     KG2_save = KG2

   endif  ! endif(init.eq.1) ------------------------------------

       optNP = ZQn/ZQp    ! Optimal nutrient ratio for zooplankton

!-----------------------------------------------------------------
!   Begin main ij loop for the biogeochemistry 
!   calculations at time-level istep
!-----------------------------------------------------------------
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
 !---------------------------------------------------------
 ! Calculate and convert variables needed for light routine
 !---------------------------------------------------------
      do k = 1, nz
   !Get algae counts and Nitrogen/phosphorus quotas
             do isp = 1, nospA          
               A_k(isp,k) = f(i,j,k,isp) ! Phytoplankton in group isp, cells/m3
               Qn_k(isp,k) = f(i,j,k,iQn(1)-1+isp)
               Qp_k(isp,k) = f(i,j,k,iQp(1)-1+isp)
             enddo 
         ! Save Temperature (celsius) and Salinity in columns
           T_k(k)       = T(i,j,k)
           S_k(k)     = S(i,j,k)
         ! Silica is mmol Si/m3
           N_k(k)     = f(i,j,k,iNO3)+f(i,j,k,iNH4)
           P_k(k)     = f(i,j,k,iPO4)
           Si_k(k)     = f(i,j,k,iSi)
         ! CDOM is in ppb
           CDOM_k(k)  = f(i,j,k,iCDOM)
         ! Below is mmol/m3 Organic Matter from rivers converted to equivalent g carbon/m3
           OM1SPM_k(k) = f(i,j,k,iOM1_R) * C_cf 
         ! There is 1.8% Organic Matter in SPM originating from the rivers.
           OM1SPM_k(k) = OM1SPM_k(k)/0.018
         ! Below is mmol/m3 Organic Matter from fecal pellets converted to equivalent g carbon/m3
           OM1Z_k(k) = f(i,j,k,iOM1_Z) * C_cf 
         ! Below is mmol/m3 Organic Matter from dead phytoplankton converted to equivalent g carbon/m3
           OM1A_k(k)  = f(i,j,k,iOM1_A) * C_cf 
         ! Below is mmol/m3 Organic Matter from initial and boundary conditions converted to equivalent g carbon/m3
           OM1BC_k(k)  = f(i,j,k,iOM1_BC) * C_cf 
           aDailyRad_k(k) = aDailyRad(i,j,k)
      enddo ! End of the "DO k = 1, nz" block DO loop

!----------------------------------------------------------------
!
! Get chlorophyll-a quantity per layer
! 
!----------------------------------------------------------------
      select case (Which_chlaC)
      case (1) ! Use regression
         Chla_tot_k = Chla_Regression(A_k)

      case (2) ! Use Cloern Chl:C ratio
         Chla_tot_k = Chla_Cloern(A_k, Qn_k, Qp_k, N_k, P_k, Si_k, T_k, aDailyRad_k,Chl_C_k)

      case default
         WRITE(6, "(/'The inputfile specified invalid setting: Which_chlaC = ', I2/)") Which_chlaC
         WRITE(6, "('Which_chlaC determines the method for calculating the quantity of chlorophyll-a.')")
         WRITE(6, "('Please set Which_chlaC to one of these values:'/)")
         WRITE(6, "('1:')")
         WRITE(6, "('  Use the regression expression.'/)")
         WRITE(6, "('2:')")
         WRITE(6, "('  Use the Cloern Chl:C ratio.'/)")
         WRITE(6, "('Run aborted.'/)")

         STOP
      end select ! Which_chlaC


!----------------------------------------------------------------------
! Execute the desired atmospheric light model.  To calculate PARsurf,
! the effect amount of downward spectrally integrated irradiance 
! just below the sea surface.  'Rad' is just above sea surface. 
!----------------------------------------------------------------------

 ! First calculate the Julian(GMT) model year (iYrTC), month (iMonTC), 
 ! day (iDayTC), hour (iHrTC), minute (iMinTC), and second (iSecTC) 
 ! associated with the midpoint of the present timestep istep TC_8

      CALL DATE_TIMESTAMP( iYr0, TC_8, &
                           iYrTC, iMonTC, iDayTC, iHrTC, iMinTC, iSecTC )

 ! Calc HrTC, the decimal hour of day
       HrTC = iHrTC + OneD60*iMinTC + OneD60*iSecTC

 ! Now calculate whether or not the year iYrTC is a leap year
      if(JY(iYrTC) == 1) then
         leapyr = .FALSE.   ! iYrTC is not a leap year
      else 
         leapyr = .TRUE.    ! iYrTC is     a leap year
      endif 

 ! Now calculate the Julian Day associated with model time TC_8
      julianDay = mdate_julian(iMonTC,iDayTC,leapyr)

 ! Now calculate SunZenithAtm, the solar beam zenith angle in radians
 ! for a given GMT Julian day, hour, latitude and and longitude
     SunZenithAtm = calc_solar_zenith(lat(j), lon(i),  HrTC, julianDay, leapyr)

 !--Begin Calculate atmospheric model --------------------------------
         ! Rad(i,j) is short wave generated by NRL is used,
         ! and is multiplied by SWtoPAR: ratio of PAR to
         ! shortwave radiation (hardcoded 4/30/14 to 0.43).
         ! Hardcoded to 0.47 on 2/11/16, Re: Tsubo and Walker, 2005
                    PARsurf = 0.47 * Rad(i,j)
 !--End Calculate atmospheric model ---------------------------------------------

!----------------------------------------------------------------------------
! Execute the desired underwater light model to calculate the 1-D radiation
! arrays PARdepth_k, PARbot_ij and PAR_percent_k radiation arrays for
! vertical grid column (i,j).
!
! PARdepth_k(k) is the downward irradiance (photons/cm2/sec) at the middle
!                    of cell(i,j,k).
!
! PARbot is the downward irradiance (photons/cm2/sec) at the sea bottom
!
! PAR_percent_k(k)    is the % of incoming irradiance PARsurf that PARdepth_k(k)
!                 represents. PARsurf is the downward irradiance
!                 (photons/cm2/sec) just below the sea surface.
!-------------------------------------------------------------------------

         select case (Which_irradiance)

                 !--------------------------------------------
         case (1)! Upgraded form of the underwater light model
                 ! developed by Brad Penta of NRL is used
                 !--------------------------------------------

                if(PARsurf.gt.0.) then
                 call Call_IOP_PAR(                                    &
                 & PARsurf    , SunZenithAtm,                          &
                 & CDOM_k     , Chla_tot_k,                            &
                 & OM1A_k     , OM1Z_k,                                &
                 & OM1SPM_k   , OM1BC_k, d(i,j,:),                       &
                 & nz         , d_sfc(i,j,:),                          &
                 & PAR_percent_k,                                      &
                 & PARbot     , PARdepth_k                         )
                else
                 PAR_percent_k=0.
                 Parbot=0.
                 PARdepth_k=0.
                endif

               ! Save Output Arrays for netCDF
                 PAR_percent_ijk(i,j,1:nz) = PAR_percent_k(1:nz)
                 PARdepth_ijk(i,j,1:nz) = PARdepth_k(1:nz)
                 PARbot_ij(i,j) = Parbot

                 !-------------------------------------------------
         case (2)! Upgraded form of the original underwater light
                 ! model of Pete Eldridge is used. Now accounts for
                 ! light attenuation in each k layer rather than for the whole
                 ! mixed layer as in the Eldridge & Roelke(2010) code 
                 !-------------------------------------------------

                 PAR_percent_k    = 0.0
                 PARbot = 0.0
                 PARdepth_k = 0.0

             if(PARsurf /= 0.0) then 

                 PARbotkm1 = PARsurf             ! initialize at top of
                                                 ! column i,j., i.e.
                                                 ! at bottom of layer "zero".

                 do k = 1, nz
                   !Calculate attenuation coefficient
                     Katt    = Kw                                                  &
                     &       + Kchla   * Chla_tot_k(k)                             &
                     &       + Kspm * (OM1SPM_k(k)+OM1A_k(k)+OM1Z_k(k)+OM1BC_k(k)) &
                     &       + Kcdom* CDOM_k(k)                                    &
                     &       + (((0.0022*S_k(k))-0.158)*S_k(k)+3.03)

                     PARtopk = PARbotkm1         ! irradiance at top of
                                                 ! layer k is same as
                                                 ! irradiance at bottom
                                                 ! of layer km1
                     tmpexp  = exp(-0.5*Katt*dz(i,j,k))

                     PARdepth_k(k) = PARtopk * tmpexp    ! irradiance at middle of layer k

                     PARbot  = PARdepth_k(k) * tmpexp    ! irradiance at bottom of layer k

                     PARbotkm1 = PARbot         ! reinitialize for next top layer

                     PAR_percent_k(k)       = 100.0*PARdepth_k(k)/PARsurf ! Percent of total irradiance
                                                                    ! at middle of bottom k layer 

                 enddo 

             endif     !"End of "if(PARsurf /= 0.0)" 

                   ! Save array values for netCDF
                     PAR_percent_ijk(i,j,1:nz) = PAR_percent_k(1:nz)
                     PARdepth_ijk(i,j,1:nz) = PARdepth_k(1:nz)
                     PARbot_ij(i,j) = Parbot

         case default
           WRITE(6, "(' ')")
           WRITE(6, "('You have chosen Which_irradiance = ', I2)") Which_irradiance
           WRITE(6, "('which is not supported. ')")
           WRITE(6, "('Only Which_irradiance = 1 and 2 are supported where: ')")
           WRITE(6, "(' ')")
           WRITE(6, "('w_light_m = 1: ')")
           WRITE(6, "('Underwater light model developed by Brad Penta of NRL ')")
           WRITE(6, "(' ')")
           WRITE(6, "('w_light_m = 2: ')")
           WRITE(6, "('Upgraded form of the original underwater light ')")
           WRITE(6, "('model of Pete Eldridge is used. ')")
           WRITE(6, "(' ')")
           WRITE(6, "('Abort Run.')")
           WRITE(6, "(' ')")
           STOP
         end select
!---------------------End Underwater Light Model-----------------------------------

! Update running total of current day's irradiance
         aRadSum_k(:) = aRadSum_k(:) + PARdepth_k(:)
         if (MOD(istep, StepsPerDay) .eq. 0) then ! If last time step of day
            ! Convert summed irradiance from quanta/cm2/s to mol quanta/m2/s
            ! and store for next day's processing
            aDailyRad_k(:) = aRadSum_k(:) * RADCONV * dT
            aRadSum_k(:) = 0.0
         endif ! if (MOD(istep, StepsPerDay) .eq. 0)

         aDailyRad(i,j,:) = aDailyRad_k(:)
         aRadSum(i,j,:) = aRadSum_k(:)

!-------------------------------------------------------------------------
! call subroutine calc_Agrow to execute the desired phytoplankton 
! growth model to calculate the one-D array Agrow_k.
!
! Agrow_k(k) is the growth-rate  for
! vertical grid column (i,j) at cell (i,j,k).
!-----------------------------------------------------------------------
       call calc_Agrow(  PARdepth_k,    T_k,                    &
     & Qn_k   , Qp_k, Si_k , A_k     , Agrow_k,                  &
     & uA_k, Aresp_k, uN_k, uP_k, uE_k, uSi_k  )

! Save arrays to output to netCDF
     do k = 1,nz
      do isp = 1,nospA
        uN_ijk(i,j,k,isp) = uN_k(k,isp)
        uP_ijk(i,j,k,isp) = uP_k(k,isp)
        uE_ijk(i,j,k,isp) = uE_k(k,isp)
        uA_ijk(i,j,k,isp) = uA_k(k,isp)
        uSi_ijk(i,j,k,isp) = uSi_k(k,isp)
        Chl_C_ijk(i,j,k,isp) = Chl_C_k(isp,k)
       enddo
        Chla_tot_ijk(i,j,k) = Chla_tot_k(k)
      enddo
!------end phytoplankton growth model-------------------------


!------------------------------------------
! Begin main k loop within the main ij loop
!------------------------------------------
      do k = 1, nz      
!---------------------------------------------------------------------	   
  !Initialize variables
!  Zooplankton groups
   Z(:)         = f(i,j,k,iZ(:))

          do isp = 1, nospA
             Abiovol       = A_k(isp,k)*volcell(isp) 
             top_A(isp,:)    = AMAX1((Abiovol-Athresh(isp))*ediblevector(:,isp),0.0)
             bottom_A(isp,:) = Abiovol * ediblevector(:,isp)
          enddo

         do isz = 1, nospZ
          bottom(isz) = SUM(bottom_A(:,isz))   ! sum over isp
         enddo

          do isp = 1, nospA
	     monodZ(isp,:)  = top_A(isp,:)/(ZKa(:) + bottom(:))
	  enddo 


!--------------------------------------------------------------------------
! Initialize counters to zero that are used to accumulate variable values
! over the nospA phytoplankton groups and the nospZ zooplankton groups.
!--------------------------------------------------------------------------
      PrimProd  = 0.0
      ArespC    = 0.0
      AexudN    = 0.0
      AexudP    = 0.0 
      AupN      = 0.0
      AupP      = 0.0
      AupSi     = 0.0
      ZgrazC    = 0.0
      ZgrazN    = 0.0
      ZgrazP    = 0.0

!---------------------------------------------------------------
! Set the scalar variables that are common to all three reaction
! subroutines
!----------------------------------------------------------------
        O2        = f(i,j,k,iO2)
        NO3       = f(i,j,k,iNO3)
        NH4       = f(i,j,k,iNH4)
        Si        = f(i,j,k,iSi)
        PO4       = f(i,j,k,iPO4)
        DIC       = f(i,j,k,iDIC)
        OM1_A     = f(i,j,k,iOM1_A)
        OM2_A     = f(i,j,k,iOM2_A)
        OM1_Z    = f(i,j,k,iOM1_Z)
        OM2_Z    = f(i,j,k,iOM2_Z)
        OM1_R    = f(i,j,k,iOM1_R)
        OM2_R    = f(i,j,k,iOM2_R)
        OM1_BC    = f(i,j,k,iOM1_BC)
        OM2_BC    = f(i,j,k,iOM2_BC)

!--------------------------------------
! Call temperature and growth functions
!-----------------------------------------
      call func_T(T_k(k), Tadj)
      call func_Qs(Qn_k(:,k), Qp_k(:,k), f_Qn, f_Qp)

!Nutrients only taken up during the day:
     Is_Day = 1
     if(RAD(i,j).eq.0) Is_Day = 0

! ----------------------------------------------------------------------
      do isp = 1, nospA      
! ----------------------------------------------------------------------   
       Aresp   = Aresp_k(isp,k)    
       uA      = uA_k(k,isp)    
       Agrow   = Agrow_k(isp,k) 
       Qn      = Qn_k(isp,k) 
       Qp      = Qp_k(isp,k)      

!---------------------------------------------------------------------      
! Note that the expressions for PrimProd, ArespC, AexudN, AexudP are 
! sums over the isp phytoplankton groups. When the isp loop is complete,
! PrimProd, ArespC. AexudN, and AexudP will represent totals for
! all the nospA phytoplankton groups. 
!--------------------------------------------------------------------
      PrimProd       = PrimProd + Agrow*Qc(isp)    ! Phytoplankton 
                                                   ! primary production
                                                   ! (mmol-C/m3/d)	

      ArespC  = ArespC + Aresp*Qc(isp)             ! Phytoplankton respiration 
						   ! equivalent carbon loss
						   ! (mmol-C/m3/d)	
      
      AexudN_A(isp) = Aresp*Qn/A_k(isp,k)   ! Phytoplankton group exudation (mmol-N/cell/d)
      AexudP_A(isp) = Aresp*Qp/A_k(isp,k)   ! Phytoplankton group exudation (mmol-P/cell/d)
      AexudN = AexudN + Aresp*Qn            ! Total Phytoplankton exudation (mmol-N/m3/d)
      AexudP = AexudP + Aresp*Qp            ! Total Phytoplankton exudation (mmol-P/m3/d)
      
!-------------------------------------------------------------------------
! Calculate dead phytoplankton, particulate and dissolved  
!-------------------------------------------------------------------------      
     Amort(isp)     = A_k(isp,k) * mA(isp)    ! dead phytoplankton (cells/m3/day)

   !Monod Equations
     Ntotal       = NO3 + NH4 
     monodN(isp)  = Ntotal/(Ntotal+Kn(isp))
     monodP(isp)  = PO4/(PO4+Kp(isp))
     monodSi(isp) = Si/(Si+Ksi(isp))

!------------------------------------------------------------------------     
! Nutrient limited uptake:
! Find Rate Limiting Nutrient RLN for N, P, and Si:
! N==1, P==2, Si==3
   if(Ntotal.le.PO4.and.Ntotal.le.Si) then
     RLN = 1
   elseif (PO4.le.Ntotal.and.PO4.le.Si) then
     RLN = 2
   else
     RLN = 3
   endif

!------------------------------------------------------------------------
   if(Is_Day.eq.0) then  !Nutrient uptake only takes place during the day
        vN = 0
        vP = 0
        vSi = 0

   else

      if(RLN.eq.1) then
         vN = Q10_T(T_k(k),vmaxN(isp))*monodN(isp)*f_Qn(isp)

         vP = Q10_T(T_k(k),vmaxP(isp))*monodP(isp)*f_Qp(isp)&
     &      *( Ntotal/(Ntotal+aN(isp)*Kn(isp)) )

         vSi = Q10_T(T_k(k),vmaxSi(isp))*monodSi(isp)        &
     &      *( Ntotal/(Ntotal+aN(isp)*Kn(isp)) )

      elseif(RLN.eq.2) then

         vN = Q10_T(T_k(k),vmaxN(isp))*monodN(isp)*f_Qn(isp)&
     &      *( PO4/(PO4+aN(isp)*Kp(isp)) )

         vP = Q10_T(T_k(k),vmaxP(isp))*monodP(isp)*f_Qp(isp)

         vSi = Q10_T(T_k(k),vmaxSi(isp))*monodSi(isp)       &
     &      *( PO4/(PO4+aN(isp)*Kp(isp)) )

      else

         vN = Q10_T(T_k(k),vmaxN(isp))*monodN(isp)*f_Qn(isp)&
     &      *( Si/(Si+aN(isp)*Ksi(isp)) )

         vP = Q10_T(T_k(k),vmaxP(isp))*monodP(isp)*f_Qp(isp)&
     &      *( Si/(Si+aN(isp)*Ksi(isp)) )

         vSi = Q10_T(T_k(k),vmaxSi(isp))*monodSi(isp)

      endif

   endif !Endif Is_Day.eq.0

      
!--------------------------------------------------------------      
! When isp loop is done, AupN and AupP are totals for all nospA 
! phytoplankton groups in cell k          
!---------------------------------------------------------------   
      AupN = AupN + A_k(isp,k)*vN     ! Phytoplankton uptake of Nitrogen (mmol-N/m3/d)
      AupP = AupP + A_k(isp,k)*vP     ! Phytoplankton uptake of Phosphorus (mmol-P/m3/d) 
      AupSi = AupSi + A_k(isp,k)*vSi  ! Phytoplankton uptake of Silica (mmol-Si/m3/d)
    
!-----------------------------------------------------------------------
! Note that Zumax(1)*monodZ(isp,1) is volume of type isp phytoplankton
! eaten per day by type 1 zooplankton. Therefore
!      Zumax(1)*monodZ(isp,1)/volcell(isp)
! is the number of type isp phytoplankton eaten per day by type 1 
! zooplankton. An analogous statement holds for type 2 zooplankton
!-----------------------------------------------------------------------
      Zgrazvol(isp,:)     = Z(:)*Zumax(:)*monodZ(isp,:)   ! Grazing of phytoplankton by biovolume (um3/m3/d)
      ZgrazA(isp,:)       = Zgrazvol(isp,:)/volcell(isp)  ! Grazing of phytoplankton (cells/m3/d)
      ZgrazA_tot(isp) = SUM( ZgrazA(isp,:) ) 

!----------------------------------------------------------------------
! When the isp loop is finished, ZgrazC, ZgrazN, and ZgrazP will be total
! carbon, nitrogen, and phosphorous uptake of zooplankton from grazing
! all phytoplankton groups
!---------------------------------------------------------------------
      ZgrazC(:) = ZgrazC(:) + ZgrazA(isp,:) * Qc(isp)     ! Carbon uptake from grazing (mmol-C/m3/day)
      ZgrazN(:) = ZgrazN(:) + ZgrazA(isp,:) * Qn          ! Nitrogen uptake from grazing( mmol-N/m3/day)
      ZgrazP(:) = ZgrazP(:) + ZgrazA(isp,:) * Qp          ! Phosphorus uptake from grazing (mmol-P/m3/day)


!---------------------------------------------------------
!-A; Phytoplankton number density (cells/m3);
!---------------------------------------------------------
      ff(i,j,k,iA(isp)) = AMAX1(f(i,j,k,iA(isp))                              &
      & + ( Agrow - Aresp - ZgrazA_tot(isp) - Amort(isp) )*dTd,1.)

!----------------------------------------------------------------------
!-Qn: Phytoplankton Nitrogen Quota (mmol-N/cell)
!----------------------------------------------------------------------
      Qn = f(i,j,k,iQn(isp))                              &
    &               + (vN - Qn*uA - AexudN_A(isp))*dTd

! Enforce minima, also enforce maxima if not equal Droop (Which_quota=1)
      if(Which_quota.eq.1) then
           Qn = AMAX1(Qn,QminN(isp))
      else
           Qn = AMIN1(AMAX1(Qn,QminN(isp)),QmaxN(isp))
      endif

      ff(i,j,k,iQn(1)-1+isp) = Qn
 
!----------------------------------------------------------------------
!-Qp: Phytoplankton Phosphorus Quota (mmol-P/cell)
!----------------------------------------------------------------------
      Qp = f(i,j,k,iQp(isp))                              &
       &               + (vP - Qp*uA - AexudP_A(isp))*dTd

! Enforce minima, also enforce maxima if not equal Droop (Which_quota=1)
      if(Which_quota.eq.1) then
           Qp = AMAX1(Qp,QminP(isp))
      else
           Qp = AMIN1(AMAX1(Qp,QminP(isp)),QmaxP(isp))
      endif

      ff(i,j,k,iQp(1)-1+isp) = Qp      

!----------------------------------------------------------------------- 
      enddo  ! END OF do isp = 1, nospA 

!-------------------------------------------------------------------
! Now calculate the total ingested ZinC, ZinN, and ZinP of C, N, and P
!-------------------------------------------------------------------
      ZslopC(:)  = Zslop(:)*ZgrazC(:)                      ! Sloppy feeding (mmol-C/m3/d)
      ZslopC_tot = SUM(ZslopC)                          ! Total Sloppy feeding (mmol-C/m3/d)
      ZunC(:)    = (1.0_8-Zeffic(:))*(ZgrazC(:)-ZslopC(:)) ! Unassimilated (mmol-C/m3/d)
      ZinC(:)    = ZgrazC(:) - ZslopC(:) - ZunC(:)         ! Ingested (mmol-C/m3/d)

      ZslopN(:)  = Zslop(:)*ZgrazN(:)                      ! Sloppy feeding (mmol-N/m3/d)
      ZslopN_tot = SUM(ZslopN)                             ! Total Sloppy feeding (mmol-N/m3/d) 
      ZunN(:)    = (1.0_8-Zeffic(:))*(ZgrazN(:)-ZslopN(:)) ! Unassimilated (mmol-N/m3/d)
      ZinN(:)    = ZgrazN(:) - ZslopN(:) - ZunN(:)         ! Ingested (mmol-N/m3/d)

      ZslopP(:)  = Zslop(:)*ZgrazP(:)                      ! Sloppy feeding (mmol-P/m3/d)
      ZslopP_tot = SUM(ZslopP)                             ! Total Sloppy feeding (mmol-P/m3/d)
      ZunP(:)    = (1.0_8-Zeffic(:))*(ZgrazP(:)-ZslopP(:)) ! Unassimilated (mmol-P/m3/d)
      ZinP(:)    = ZgrazP(:) - ZslopP(:) - ZunP(:)         ! Ingested (mmol-P/m3/d)
!-------------------------------------------------


!------------------------------------         
! Liebigs Law for zooplankton group isz 
!------------------------------------
  do isz=1,nospZ

     if (ZinN(isz) .gt. optNP(isz)*ZinP(isz)) then  
        Zgrow(isz)= ZinP(isz)/ZQp(isz)                   ! P-limited growth (indv./m3/d) 
        ZegN(isz) = ZinN(isz) - ZinP(isz)*optNP(isz)       ! P-limited N excretion (mmol-N/m3/d) 
                                                   ! determined by subtracting N-equivalent of ZinP
        ZegC(isz) = ZinC(isz) - ZinP(isz)/ZQp(isz)*ZQc(isz)  ! P-limited C excretion (mmol-C/m3/d)
        ZegP(isz) = 0.                        
      else
        Zgrow(isz)= ZinN(isz)/ZQn(isz)                   ! N-limited growth (indv./m3/d)
        ZegP(isz) = ZinP(isz) - ZinN(isz)/optNP(isz)       ! N-limited P excretion (mmol-P/m3/d)    
                                                   ! determined by subtracting P-equivalent of ZinN
        ZegC(isz) = ZinC(isz) - ZinN(isz)/ZQn(isz)*ZQc(isz)  ! N-limited C excretion (mmol-C/m3/d)
        ZegN(isz) = 0.
      endif

  enddo

!------------------------------------------------

!-----------------------------------------------------
! ZegC should not be negative 
  do isz=1,nospZ
      if(ZegC(isz).lt.0.) then
          ZegC(isz) = 0.
          !ZegN(isz) = 0.
          !ZegP(isz) = 0.
      endif
  enddo

! Egestion and unassimilated for Si set equivalent to that of N
      ZegSi = ZegN
      ZunSi = ZunN


! Zooplankton respiration based on growth and basal metabolism, both modified by a temperature adjustment factor 
      Zresp(:) = (Zgrow(:)*Zrespg(:) + Z(:)*Zrespb(:)) !Zooplankton respiration (indv./m3/d)

      ZrespC   = SUM(Zresp*ZQc)                                       !Total Carbon loss from respiration (mmol-C/m3/d)

                                                ! Excretion
      ZexN(:)   = Zresp(:)*ZQn(:)               ! (mmol-N/m3/d)
      ZexP(:)   = Zresp(:)*ZQp(:)               ! (mmol-P/m3/d)

                                                ! Mortality
     Zmort(:)       = Zm(:) * Z(:) * Z(:)       ! (indv./m3/d)
     ZmortC(:)      = Zmort(:)*ZQc(:)           ! (mmol-C/m3/d)
     ZmortC_tot     = SUM(ZmortC)
     ZmortN(:)      = Zmort(:)*ZQn(:)           ! (mmol-N/m3/d)
     ZmortN_tot     = SUM(ZmortN)
     ZmortP(:)      = Zmort(:)*ZQp(:)           ! (mmol-P/m3/d)
     ZmortP_tot     = SUM(ZmortP)
!-------------------------------------------------------------------------

!---------------------------------------------------------
!-G; Zooplankton number density (individuals/m3);
!---------------------------------------------------------
      ff(i,j,k,iZ(:))  = AMAX1( f(i,j,k,iZ(:))                         &
      &      + (Zgrow(:) - Zresp(:) - Zmort(:))*dTd, 1.)
!------------------------------------------------------------------------

!-----------------------------------------------------------
! Remineralization - reactions
!---------------------------------------------------------------
       ! Instant Remineralization, if on bottom of shelf, redefine KG's
       if(k.eq.nz.and.Which_fluxes(iInRemin).eq.1.and.wsm(i,j).eq.0.) then
           KG1 = KG_bot
           KG2 = KG_bot
       endif
!------------------------------------------------------------
! Nitrification
!--------------------------------------------------------------
        call Nitrification( O2, NH4, KO2, KNH4, nitmax, T_k(k), R_11 )

!------------------------------------------------------------
! Carbon Chemistry
!--------------------------------------------------------------
!!! MOCSY alkalinity expressions:
        m_alk = f(i,j,k,iALK)/1000.
        m_dic = f(i,j,k,iDIC)/1000.
        m_si  = f(i,j,k,iSi)/1000.
        m_po4 = f(i,j,k,iPO4)/1000.
        call vars(ph_calc, pco2_calc, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD_calc, rhoSW, p, tempis,&
         &    T(i,j,k), S(i,j,k), m_alk, m_dic, m_si, m_po4, patm, d_sfc(i,j,k), lat(j), 1, &
         &    'mol/m3', 'Tinsitu', 'm ', 'u74', 'l  ', 'pf ', 'Pzero  ')
        pH(i,j,k) = ph_calc(1)

!------------------------------------------------------------
! Particulate and Dissolved dead phytoplankton, rate of remineralization
!--------------------------------------------------------------
        call reaction( OM1_A, OM2_A, O2, NO3, KG1, KG2, KO2, KstarO2, KNO3,               &
     &  s_x1A(i,j,k), s_y1A(i,j,k), s_z1A(i,j,k), s_x2A(i,j,k), s_y2A(i,j,k), s_z2A(i,j,k), T_k(k), RC )

        RC        = one_d_365 * RC  !Change units from /year to /day

        ROM1_A     = RC(1)          ! units are /m3/day
        ROM2_A     = RC(2)
        RO2_A      = RC(3)
        RNO3_A     = RC(4)
        RPO4_A     = RC(5)
        RDIC_A     = RC(6)
        RNH4_A     = RC(7)
        RSi_A      = RC(8)
        RALK_A     = RC(9)
        RN2_A      = RC(10)
!------------------------------------------------------------
! Particulate and Dissolved fecal pellets, rate of remineralization
!--------------------------------------------------------------
        call reaction( OM1_Z, OM2_Z, O2, NO3, KG1, KG2, KO2, KstarO2, KNO3,               &
     &  s_x1Z(i,j,k), s_y1Z(i,j,k), s_z1Z(i,j,k), s_x2Z(i,j,k), s_y2Z(i,j,k), s_z2Z(i,j,k), T_k(k), RC )
        RC       = one_d_365 * RC   !Change units from /year to /day

        ROM1_Z     = RC(1)         ! units are /m3/day
        ROM2_Z     = RC(2)
        RO2_Z      = RC(3)
        RNO3_Z     = RC(4)
        RPO4_Z     = RC(5)
        RDIC_Z     = RC(6)
        RNH4_Z     = RC(7)
        RSi_Z      = RC(8)
        RALK_Z     = RC(9)
        RN2_Z      = RC(10)

!------------------------------------------------------------
! Particulate and Dissolved riverine OM, rate of remineralization 
!------------------------------------------------------------
        call reaction( OM1_R, OM2_R, O2, NO3, KG1_R, KG2_R, KO2, KstarO2, KNO3,               &
     &  stoich_x1R, stoich_y1R, stoich_z1R, stoich_x2R, stoich_y2R, stoich_z2R, T_k(k), RC )

        RC       = one_d_365 * RC   !Change units from /year to /day

        ROM1_R     = RC(1)         ! units are /m3/day
        ROM2_R     = RC(2)
        RO2_R      = RC(3)
        RNO3_R     = RC(4)
        RPO4_R     = RC(5)
        RDIC_R     = RC(6)
        RNH4_R     = RC(7)
        RSi_R      = RC(8)
        RALK_R     = RC(9)
        RN2_R      = RC(10)

!------------------------------------------------------------
! Particulate and Dissolved initial and boundary OM, rate of remineralization
!------------------------------------------------------------
        call reaction( OM1_BC, OM2_BC, O2, NO3, KG1_BC, KG2_BC, KO2, KstarO2, KNO3,               &
     &  stoich_x1BC, stoich_y1BC, stoich_z1BC, stoich_x2BC, stoich_y2BC, stoich_z2BC, T_k(k), RC )

        RC       = one_d_365 * RC   !Change units from /year to /day

        ROM1_BC     = RC(1)         ! units are /m3/day
        ROM2_BC     = RC(2)
        RO2_BC      = RC(3)
        RNO3_BC     = RC(4)
        RPO4_BC     = RC(5)
        RDIC_BC     = RC(6)
        RNH4_BC     = RC(7)
        RSi_BC      = RC(8)
        RALK_BC     = RC(9)
        RN2_BC      = RC(10)

       ! Instant Remineralization, change KG's back to original
       if(k.eq.nz.and.Which_fluxes(iInRemin).eq.1.and.wsm(i,j).eq.0.) then
           KG1 = KG1_save
           KG2 = KG2_save
       endif

!--------------------------------------------------------------------
! Sum remineralization terms from dead phytoplankton, fecal pellets, and riverine particulate
  RO2   = RO2_A  + RO2_Z  + RO2_R  + RO2_BC - 2.*R_11  ! (mmol-O2/m3/d)
  RNO3  = RNO3_A + RNO3_Z + RNO3_R + RNO3_BC + R_11    ! (mmol-NO3/m3/d)
  RNH4  = RNH4_A + RNH4_Z + RNH4_R + RNH4_BC - R_11    ! (mmol-NH4/m3/d)
  RPO4  = RPO4_A + RPO4_Z + RPO4_R + RPO4_BC           ! (mmol-PO4/m3/d)
  RDIC  = RDIC_A + RDIC_Z + RDIC_R + RDIC_BC           ! (mmol-DIC/m3/d)
  RSi   = RSi_A  + RSi_Z  + RSi_R  + RSi_BC            ! (mmol-Si/m3/d)
  RALK  = RALK_A + RALK_Z + RALK_R + RALK_BC - 2.*R_11 ! (mmol-HCO3/m3/d)
  RN2   = RN2_A + RN2_Z + RN2_R + RN2_BC         ! (mmol-N2/m3/d)
       !Save for netCDF
       RN2_ijk(i,j,k) = RN2_ijk(i,j,k) + (2*RN2)*dTd

! Save RO2 as CBODW
  CBODW(i,j) = RO2 !The last time this happens, k=nz, so will be the bottom
!--------------------------------------------------------------------


!---------------------------------------------------------------------
! Stoichiometry - calculate C:N:P ratios for Remineralization equations
!---------------------------------------------------------------------
!-- Organic Matter from dead phytoplankton --------------------------
OM1_CA = 0.
OM2_CA = 0.
OM1_NA = 0.
OM2_NA = 0.
OM1_PA = 0.
OM2_PA = 0.

do isp=1,nospA
 if ( uN_k(k,isp) .lt. uP_k(k,isp)  ) then
!Particulate
   OM1_CA = OM1_CA + Amort(isp)*(Qn_k(isp,k)-QminN(isp))/Qn_k(isp,k)*Qc(isp)
   OM1_NA = OM1_NA + Amort(isp)*(Qn_k(isp,k)-QminN(isp))
   OM1_PA = OM1_PA + Amort(isp)*(Qn_k(isp,k)-QminN(isp))/Qn_k(isp,k)*Qp_k(isp,k)
!Dissolved
   OM2_CA = OM2_CA + Amort(isp)*QminN(isp)/Qn_k(isp,k)*Qc(isp)
   OM2_NA = OM2_NA + Amort(isp)*QminN(isp)
   OM2_PA = OM2_PA + Amort(isp)*QminN(isp)/Qn_k(isp,k)*Qp_k(isp,k)
 else
!Particulate
   OM1_CA = OM1_CA + Amort(isp)*(Qp_k(isp,k)-QminP(isp))/Qp_k(isp,k)*Qc(isp)
   OM1_NA = OM1_NA + Amort(isp)*(Qp_k(isp,k)-QminP(isp))/Qp_k(isp,k)*Qn_k(isp,k)
   OM1_PA = OM1_PA + Amort(isp)*(Qp_k(isp,k)-QminP(isp))
!Dissolved
   OM2_CA = OM2_CA + Amort(isp)*QminP(isp)/Qp_k(isp,k)*Qc(isp)
   OM2_NA = OM2_NA + Amort(isp)*QminP(isp)/Qp_k(isp,k)*Qn_k(isp,k)
   OM2_PA = OM2_PA + Amort(isp)*QminP(isp)
 endif
enddo

                                             ! Dissolved


   !This calculates the cumulative stoichiometry ratios for OM1_A
   if(OM1_CA.ne.0) then
    stoich_x1A = (OM1_CA*dTd + OM1_A) / (OM1_PA*dTd + (1/s_x1A(i,j,k))*OM1_A) ! C/P
    stoich_y1A = (OM1_NA*dTd + (s_y1A(i,j,k)/s_x1A(i,j,k))*OM1_A) / (OM1_PA*dTd + (1/s_x1A(i,j,k))*OM1_A) !N/P
    stoich_z1A = 1.
   else
    stoich_x1A = s_x1A(i,j,k)
    stoich_y1A = s_y1A(i,j,k)
    stoich_z1A = 1.
   endif
   !Save for later timesteps and for netCDF files
    s_x1A(i,j,k) = stoich_x1A
    s_y1A(i,j,k) = stoich_y1A
    s_z1A(i,j,k) = stoich_z1A

   !This calculates the cumulative stoichiometry ratios for OM2_A
   if(OM2_CA.ne.0) then
    stoich_x2A = (OM2_CA*dTd + OM2_A) / (OM2_PA*dTd + (1/s_x2A(i,j,k))*OM2_A) ! C/P
    stoich_y2A = (OM2_NA*dTd + (s_y2A(i,j,k)/s_x2A(i,j,k))*OM2_A) / (OM2_PA*dTd + (1/s_x2A(i,j,k))*OM2_A) !N/P
    stoich_z2A = 1.
   else
    stoich_x2A = s_x2A(i,j,k)
    stoich_y2A = s_y2A(i,j,k)
    stoich_z2A = 1.
   endif
   !Save for later timesteps and for netCDF files
    s_x2A(i,j,k) = stoich_x2A
    s_y2A(i,j,k) = stoich_y2A
    s_z2A(i,j,k) = stoich_z2A

!-- Organic Matter from fecal pellets ---------------------------------
    OM1_Ratio = SUM( (Qn_k(:,k)-QminN)/Qn_k(:,k)*A_k(:,k))/SUM(A_k(:,k))
    OM2_Ratio = SUM( (QminN/Qn_k(:,k))*A_k(:,k))/SUM(A_k(:,k))

    if(nospZ.eq.1) then 
                                                                  ! Particulate
     OM1_CZ  = .5*(ZegC(1) + ZunC(1) + ZmortC_tot) + OM1_Ratio*ZslopC_tot !  (mmol-C/m3/d)
     OM1_NZ  = .5*(ZegN(1) + ZunN(1) + ZmortN_tot) + OM1_Ratio*ZslopN_tot !  (mmol-N/m3/d)
     OM1_PZ  = .5*(ZegP(1) + ZunP(1) + ZmortP_tot) + OM1_Ratio*ZslopP_tot !  (mmol-P/m3/d)
                                                                  ! Dissolved
     OM2_CZ  = .5*(ZegC(1) + ZunC(1) + ZmortC_tot) + OM2_Ratio*ZslopC_tot              !  (mmol-C/m3/d)
     OM2_NZ  = .5*(ZegN(1) + ZunN(1) + ZmortN_tot) + OM2_Ratio*ZslopN_tot              !  (mmol-N/m3/d)
     OM2_PZ  = .5*(ZegP(1) + ZunP(1) + ZmortP_tot) + OM2_Ratio*ZslopP_tot              !  (mmol-P/m3/d)

    else if(nospZ.eq.2) then
                                                                  ! Particulate
     OM1_CZ  = ZegC(1) + ZunC(1) + ZmortC_tot + OM1_Ratio*ZslopC_tot !  (mmol-C/m3/d)
     OM1_NZ  = ZegN(1) + ZunN(1) + ZmortN_tot + OM1_Ratio*ZslopN_tot !  (mmol-N/m3/d)
     OM1_PZ  = ZegP(1) + ZunP(1) + ZmortP_tot + OM1_Ratio*ZslopP_tot !  (mmol-P/m3/d)
                                                                  ! Dissolved
     OM2_CZ  = ZegC(2) + ZunC(2) + OM2_Ratio*ZslopC_tot              !  (mmol-C/m3/d)
     OM2_NZ  = ZegN(2) + ZunN(2) + OM2_Ratio*ZslopN_tot              !  (mmol-N/m3/d)
     OM2_PZ  = ZegP(2) + ZunP(2) + OM2_Ratio*ZslopP_tot              !  (mmol-P/m3/d)

    else 

                                                                  ! Particulate
     OM1_CZ  = ZegC(1) + ZunC(1) + ZmortC_tot + OM1_Ratio*ZslopC_tot !  (mmol-C/m3/d)
     OM1_NZ  = ZegN(1) + ZunN(1) + ZmortN_tot + OM1_Ratio*ZslopN_tot !  (mmol-N/m3/d)
     OM1_PZ  = ZegP(1) + ZunP(1) + ZmortP_tot + OM1_Ratio*ZslopP_tot !  (mmol-P/m3/d)
                                                                  ! Dissolved
     OM2_CZ  = SUM(ZegC(2:nospZ)) + SUM(ZunC(2:nospZ)) + OM2_Ratio*ZslopC_tot              !  (mmol-C/m3/d)
     OM2_NZ  = SUM(ZegN(2:nospZ)) + SUM(ZunN(2:nospZ)) + OM2_Ratio*ZslopN_tot              !  (mmol-N/m3/d)
     OM2_PZ  = SUM(ZegP(2:nospZ)) + SUM(ZunP(2:nospZ)) + OM2_Ratio*ZslopP_tot              !  (mmol-P/m3/d)

    endif

   !This calculates the cumulative stoichiometry ratios for OM1_Z
   if(OM1_CZ.ne.0) then
    stoich_x1Z = (OM1_CZ*dTd + OM1_Z) / (OM1_PZ*dTd + (1./s_x1Z(i,j,k))*OM1_Z) ! C/P
    stoich_y1Z = (OM1_NZ*dTd + (s_y1Z(i,j,k)/s_x1Z(i,j,k))*OM1_Z) / (OM1_PZ*dTd + (1./s_x1Z(i,j,k))*OM1_Z) !N/P
    stoich_z1Z = 1.
   else
    stoich_x1Z = s_x1Z(i,j,k)
    stoich_y1Z = s_y1Z(i,j,k)
    stoich_z1Z = 1.
   endif
   !Save for later timesteps and for netCDF files
    s_x1Z(i,j,k) = stoich_x1Z
    s_y1Z(i,j,k) = stoich_y1Z
    s_z1Z(i,j,k) = stoich_z1Z

   !This calculates the cumulative stoichiometry ratios for OM2_Z
   if(OM2_CZ.ne.0) then
    stoich_x2Z = (OM2_CZ*dTd + OM2_Z) / (OM2_PZ*dTd + (1./s_x2Z(i,j,k))*OM2_Z) !  C/P
    stoich_y2Z = (OM2_NZ*dTd + (s_y2Z(i,j,k)/s_x2Z(i,j,k))*OM2_Z) / (OM2_PZ*dTd + (1./s_x2Z(i,j,k))*OM2_Z) !N/P
    stoich_z2Z = 1.
   else
    stoich_x2Z = s_x2Z(i,j,k)
    stoich_y2Z = s_y2Z(i,j,k)
    stoich_z2Z = 1.
   endif
   !Save for later timesteps and for netCDF files
    s_x2Z(i,j,k) = stoich_x2Z
    s_y2Z(i,j,k) = stoich_y2Z
    s_z2Z(i,j,k) = stoich_z2Z
!------------------------------------------------------------------------
 
!-------------------------------
!-NO3; (mmol-N/m3)
!-------------------------------     
       ff(i,j,k,iNO3) = AMAX1(f(i,j,k,iNO3)                            &
       &  + ( RNO3 - AupN*NO3/(NO3+NH4) )*dTd, 0.0 )              

!--------------------------------
!-NH4; Ammonium (mmol-N/m3)
!--------------------------------
       ff(i,j,k,iNH4) = AMAX1(f(i,j,k,iNH4)                            &
       & + ( RNH4 - AupN*NH4/(NO3+NH4) + 2.*AexudN + SUM(ZexN)  )*dTd, 0.0)          

!----------------------------
!-Silica: (mmol-Si/m3)
!----------------------------
       ff(i,j,k,iSi) =  AMAX1(f(i,j,k,iSi)                             &
       & + ( RSi - AupSi + SUM(ZegSi) + SUM(ZunSi) )*dTd, 0.0)

!---------------------------------------------
!-PO4: Phosphate (mmol-P/m3)
!--------------------------------------
      ff(i,j,k,iPO4) = AMAX1(f(i,j,k,iPO4)                             &
      & + ( RPO4 - AupP + 2.*AexudP + SUM(ZexP)  )*dTd, 0.0)

!---------------------------------------------------------
!-DIC: Dissolved Inorganic Carbon (mmol-C/m3)
!---------------------------------------------------------
       ff(i,j,k,iDIC) = AMAX1(f(i,j,k,iDIC)                            &
       &  + ( RDIC - PrimProd + ArespC  + ZrespC )*dTd, 0.0)  
 
!-----------------------------------------------------------------------      
!-O2: Oxygen (mmol O2 m-3) 
!-------------------------------------------------------------------      
       ff(i,j,k,iO2)  = AMAX1(f(i,j,k,iO2)                             &  
       &  + ( PrimProd - ArespC + RO2 - ZrespC)*dTd, 0.0)

!-----------------------------------------
!-OM1_A: (mmol-C/m3-- Dead Phytoplankton Particulate)
!-----------------------------------------
       ff(i,j,k,iOM1_A) = AMAX1(f(i,j,k,iOM1_A)                       &
       &   + ( (ROM1_A) + OM1_CA )*dTd, 0.0)       

!----------------------------------------------------------------------
!---------------------------------------------
!-OM2_A: (mmol-C/m3-- Dead Phytoplankton Dissolved)
!---------------------------------------------
       ff(i,j,k,iOM2_A) = AMAX1(f(i,j,k,iOM2_A)                       &
       &   + (ROM2_A + OM2_CA )*dTd, 0.0)              

!----------------------------------------------------------------------
!------------------------------------------
!-OM1_Z:(mmol-C/m3--G particulate)
!------------------------------------------
       ff(i,j,k,iOM1_Z) = AMAX1(f(i,j,k,iOM1_Z)                     &
       &   +( ROM1_Z + OM1_CZ)*dTd, 0.0)              

!---------------------------------------------------------------------
!-----------------------------------------------
!-OM2_Z:(mmol-C/m3--G dissolved)
!-----------------------------------------------
       ff(i,j,k,iOM2_Z) = AMAX1(f(i,j,k,iOM2_Z)                      &
       &   + ( ROM2_Z + OM2_CZ )*dTd, 0.0)              

!---------------------------------------------------------------------
!-------------------------------------------
!-OM1_R: (mmol-C/m3--SPM particulate)
!-------------------------------------------
       ff(i,j,k,iOM1_R) = AMAX1(f(i,j,k,iOM1_R)                      &
       &   + ( ROM1_R )*dTd, 0.0)              

!---------------------------------------------------------------------
!------------------------------------------------
!-OM2_R: (mmol-C/m3--SPM dissolved)
!------------------------------------------------
       ff(i,j,k,iOM2_R) =  AMAX1(f(i,j,k,iOM2_R)                     &
       &   + ( ROM2_R )*dTd, 0.0)       

!---------------------------------------------------------------------
!-------------------------------------------
!-OM1_BC: (mmol-C/m3--initial and boundary condition OM particulate)
!-------------------------------------------
       ff(i,j,k,iOM1_BC) = AMAX1(f(i,j,k,iOM1_BC)                      &
       &   + ( ROM1_BC )*dTd, 0.0)

!---------------------------------------------------------------------
!------------------------------------------------
!-OM2_BC: (mmol-C/m3--initial and boundary condition OM dissolved)
!------------------------------------------------
       ff(i,j,k,iOM2_BC) =  AMAX1(f(i,j,k,iOM2_BC)                     &
       &   + ( ROM2_BC )*dTd, 0.0)

!---------------------------------------------------------------------
!----------------------------
!-CDOM: (ppb) 
!----------------------------
       ff(i,j,k,iCDOM) =  AMAX1(f(i,j,k,iCDOM)*(1.0 - KGcdom*dTd), 0.0)  

!---------------------------------------------------------------------
!----------------------------
!-ALK: (mmol-HCO3/m3)
!----------------------------
       ff(i,j,k,iALK) =  AMAX1(f(i,j,k,iALK) +                 &
      & (RALK + AupN*NO3/(NO3+NH4) - AupN*NH4/(NO3+NH4) + AupP + 4.8*AupP)*dTd, 0.0) 
                      
!--------------------------------------------------------------------
        enddo   ! end of  "do k = 1, nz" 

       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
! ----------------------------------------------------------------------

!update f for the current timestep
         do k = 1,nz
         do j = 1,jm
         do i = 1,im 
             if(fm(i,j).eq.1) then
                f(i,j,k,:) = ff(i,j,k,:)
             endif
         enddo
         enddo
         enddo
!-- End Main GEM Calculations ---------------------------------------------------

!-- Call "Extra" variables for netCDF --------------------------------------------------------
!--------------------------------------------------------
  ! -- do initialization of first timestep:
      if (   istep .eq. 1 ) then
                 CALL WRITE_EXTRA_DATA( im, jm, nsl, EXTRA_VARIABLES, nospA, 0, &
                                     PARdepth_ijk, &
                                  PAR_percent_ijk, &
                                        uN_ijk, &
                                        uP_ijk, &
                                        uE_ijk, &
                                        uA_ijk, &
                                     Chla_tot_ijk, &
                                            s_x1A, &
                                            s_y1A, &
                                            s_x2A, &
                                            s_y2A, &
                                            s_x1Z, &
                                            s_y1Z, &
                                            s_x2Z, &
                                            s_y2Z, &
                                       uSi_ijk,    &
                                       Chl_C_ijk,  &
                                              pH,  &
                                         RN2_ijk)
     endif  !end of "if (mod(istep,iout).eq.0)" block if

  ! --- dump output when istep is a multiple of iout
      if (  mod( istep, iout ) .eq. 0 ) then
                 CALL WRITE_EXTRA_DATA( im, jm, nsl, EXTRA_VARIABLES, nospA, istep_out+1, &
                                     PARdepth_ijk, &
                                  PAR_percent_ijk, &
                                        uN_ijk, &
                                        uP_ijk, &
                                        uE_ijk, &
                                        uA_ijk, &
                                     Chla_tot_ijk, &
                                            s_x1A, &
                                            s_y1A, &
                                            s_x2A, &
                                            s_y2A, &
                                            s_x1Z, &
                                            s_y1Z, &
                                            s_x2Z, &
                                            s_y2Z, &
                                       uSi_ijk,    &
                                       Chl_C_ijk,  &
                                              pH,  &
                                          RN2_ijk) 
     endif  !end of "if (mod(istep,iout).eq.0)" block if


   return
   END Subroutine GEM_EPA 
!---------------------------------------------------------------------- 
