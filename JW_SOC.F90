      Subroutine JW_SOC(O2Flux,NH4Flux,PO4Flux,CBODW_in,PHY_in,I_in,DO2_in,T,tau,i,j)

      USE Model_dim,   ONLY:nospA
      USE INPUT_VARS,  ONLY:Qc,dT
      USE Conversions, ONLY:SDay,convert_I
      USE JWMod 

      IMPLICIT NONE

      real, intent(out) :: O2Flux       !Change in O2, (mg O2/L/d)
      real, intent(out) :: NH4Flux      !Change in NH4, (mg NH4/L/d)
      real, intent(out) :: PO4Flux      !Change in PO4, (mg PO4/L/d)
      real, intent(in) :: CBODW_in      !CBOD in water column (bottom sigma layer), (mmol O2/m3)
                                        !  RO2(decay term) in CGEM
      real, intent(in) :: PHY_in(nospA) !Phytoplankton concentration (bottom sigma layer), (cells/m3)
      real, intent(in) :: I_in          !Irradiance (bottom sigma layer), (quanta/cm2/s) 
      real, intent(in) :: DO2_in        !Dissolved Oxygen concentration (bottom sigma layer), (mmol O2/m3)
      real, intent(in) :: T             !Temperature (bottom sigma layer), (C)
      real, intent(in) :: tau           !Bottom shear stress, (Pa)
      integer, intent(in) :: i, j     !Indices
      real, parameter :: v_CBODW   = 0.5   !Organic matter settling velocity, (m/d)
      real, parameter :: f_dCBODW  = 0.50  !Fraction of dissolved CBODW, (unitless)
      real, parameter :: v_PHY     = 0.5   !Phytoplankton settling velocity, (m/d)
      real, parameter :: theta_bp  = 1.080 !Temperature coefficient for benthic photosynthesis, (unitless)
      real, parameter :: kSOC      = 0.003 !CBODS decomposition rate, (1/d)
      real, parameter :: theta_SOC = 1.080 !Temperature coefficient for SOC, (unitless)
      real, parameter :: f_asoc    = 0.65  !Ratio of aerobic respiration to total sediment respiration, (unitless)
      real, parameter :: K_SOC     = 0.5   !Half saturation constant for O2 limitation of SOC, (mg O2/L)
      real, parameter :: ktau      = 1.01  !Bottom stress coefficient, (unitless)
      real, parameter :: tau_ce    = 0.11  !Critical shear stress for settleable sediment resuspension, (Pa)
      real, parameter :: kCBODS    = 0.001 !CBODS burial rate, (1/d)
      real, parameter :: f_dnf     = 0.12
      real :: CBODW, PHY(nospA), PHY_tot, Irr, DO2  !Variables converted to subroutine units
      real :: dCBODS, CBODS_sed_burial, SOC, Bnth_Photosyn, PHY_sett, CBODW_sett !Calculations
      integer, save :: init = 0

      if(init.eq.0) then
      !Initialize CBODS
       CBODS = 0.
       init = 1
      endif

      !Unit Conversions
      CBODW = CBODW_in * 32./1000.  !Convert to (mg O2/L)
      PHY = PHY_in * Qc * 12./1000. !Convert to (mg C/L)
      PHY_tot = SUM(PHY)            !Total from all phytoplankton groups (mg C/L)
      Irr = I_in * convert_I          !Convert to (mol photons/m2/d)
      DO2 = DO2_in * 32./1000.      !Convert to (mg O2/L)

      !Everything here in units of mg,L

! CBODS sediment burial
      CBODS_sed_burial = kCBODS * CBODS(i,j)
! Sediment oxygen consumption
      SOC = kSOC * theta_SOC**(T-20.) * f_asoc * CBODS(i,j) * (DO2 / (K_SOC + DO2) ) &
     &      * ktau**AMAX1((tau-tau_ce),0.0)       
! Benthic Photosynthesis
      Bnth_Photosyn = 0.53 * theta_bp**(T-20.) * (32./12.) * (132./1000.) * Irr**1.45 
! Phytoplankton settling
      PHY_sett = (32./12.) * v_PHY * PHY_tot
! CBODW settling
      CBODW_sett = v_CBODW * (1-f_dCBODW) * CBODW
! Differential equation for CBODS:
      dCBODS = CBODW_sett + PHY_sett + Bnth_Photosyn - SOC - CBODS_sed_burial
      CBODS(i,j) = CBODS(i,j) + dCBODS*dT/SDay

! Benthic O2 flux:
      O2Flux = Bnth_Photosyn - SOC
! Benthic NH4 flux:
      NH4Flux = ((1.-f_dnf)*(16./106.)*(14./32.) - (f_dnf*(84.8/106.)*(14./32.))) &
     & * SOC 
! Benthic PO4 flux:
      PO4Flux = (1./106.)*(30.97/32.) * SOC

! Convert back to mmol/m3:
      O2Flux = O2Flux * 1000./32.
      NH4Flux = NH4Flux * 1000./14. 
      PO4Flux = PO4Flux * 1000./30.97


      RETURN

      END Subroutine JW_SOC 
