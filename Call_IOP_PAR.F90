   Subroutine Call_IOP_PAR(                                            &
		 & PARsurf     , sun_zenith,                           &
                 & CDOM_k      , totChl    ,                           &
                 & OM1_A       , OM1_Z     ,                           &
                 & OM1_R       , OM1_BC    ,                           &
                 & bottom_depth, numdepths ,                           &
	         & d_sfc       , PAR_percent,                          &
		 & PARbot      , PARdepth                            )   
   
!-----------------------------------------------------------------------
! Code is based on Brad Penta's code
!---------------------------------------------------------------------
  USE Model_dim
  USE LIGHT_VARS 

  IMPLICIT NONE 

!----------------------------------------------------------------------------
  real   , intent(in)    :: PARsurf      ! Irradiance just below sea surface
 
  real   , intent(in)    :: sun_zenith   ! Angle of the sun

  real   , intent(in)    :: totChl(nsl)  ! total Chl-a (mg/m3)

  real   , intent(in)    :: CDOM_k(nsl)  ! CDOM (ppb) 

  real   , intent(in)    :: OM1_A(nsl)   ! Concentration of particulate
                                         ! dead phytoplankton (g/m3)

  real   , intent(in)    :: OM1_Z(nsl)   ! Concentration of particulate
                                         ! fecal pellets (g/m3)

  real   , intent(in)    :: OM1_R(nsl)   ! Concentration of particulate
                                         ! river generated SPM (g/m3)

  real   , intent(in)    :: OM1_BC(nsl)  ! Concentration of particulate
                                         ! initial and boundary condition generated SPM (g/m3)

  real   , intent(in)    :: bottom_depth(nsl)
                                         ! depths(k) is the depth
                                         ! at the Bottom of layer k.
                                         ! it is assumed that the top
                                         ! of layer k=1 has a depth
                                         ! of zero.

  real   , intent(in)    :: d_sfc(nsl)   ! depth at center of cell k from surface

  integer, intent(in)    :: numdepths    ! Total number of layers in water
                                         ! column

  real  , intent(out)    :: PAR_percent(nsl) ! PAR_percent(k) is the % of
                                             ! incoming visible
                                             ! irradiance, PARsurf.

  real  , intent(out)    :: PARbot           ! PARbot is Par,the
                                             ! visible irradiance,
                                             ! at the sea bottom 
                                             ! (quanta/cm**2/sec)

  real  , intent(out)    :: PARdepth(nsl)    ! PARdepth(k) is Par, the
                                             ! visible irradiance
                                             ! at the middle of layer k.
                                             ! (quanta/cm**2/sec)

!----------------------------------------------------------------
! Calculate absorption (490 nm) components: seawater, chl, SPM from rivers, CDOM,
! detritus (dead cells), fecal pellets ...
      real Chla_tot, CDOM_tot, OM1A_tot, OM1Z_tot, OM1R_tot, OM1BC_tot, CDOM(nsl)
      real a490_mid, aSw_mid, aChl490_mid, aCDOM490_mid, bbChl490_mid, bb490_mid
      real a490_bot, aSw_bot, aChl490_bot, aCDOM490_bot, bbChl490_bot, bb490_bot
      real aOM1A490_mid, aOM1Z490_mid, aOM1R490_mid, aOM1BC490_mid
      real aOM1A490_bot, aOM1Z490_bot, aOM1R490_bot, aOM1BC490_bot
      integer :: k  


! First, convert CDOM(ppb) into CDOM, a490 (m-1)
! Once the CDOM (QSE ppb) is in the model domain, we advect and mix using the same 
! routine as for other dissolved constituents. However, to use the CDOM in the light 
! attenuation models, we need to calculate a490 (Penta et al. 2008). 
! 1) convert CDOM(QSE ppb) back to a312: a312 = (CDOM(QSE ppb)-0.538)/2.933 
! 2) convert a312 to a490: a490 = a312*exp(-0.016*(490-312)), where here S = 0.016 
! (mean value from D'Sa and DiMarco (2008)
   do k=1,numdepths
      CDOM(k) = (CDOM_k(k) - 0.538)/2.933 !ppb to a312
      CDOM(k) = CDOM(k) * exp(-0.016*(490.-312.))
   enddo 


!Initialize counters for Chla, CDOM, and detritus:
   Chla_tot = 0.
   CDOM_tot = 0.
   OM1A_tot = 0.
   OM1Z_tot = 0.
   OM1R_tot = 0.
   OM1BC_tot = 0.

   do k=1,numdepths
!Sum components to depth:
      Chla_tot = Chla_tot + totChl(k)
      CDOM_tot = CDOM_tot + CDOM(k)
      OM1A_tot = OM1A_tot + OM1_A(k)
      OM1Z_tot = OM1Z_tot + OM1_Z(k)
      OM1R_tot = OM1R_tot + OM1_R(k)
      OM1BC_tot = OM1BC_tot + OM1_BC(k)
!Calculate absorption coefficients:
      aSw_mid = aw490 * d_sfc(k) !Sea water absorption at mid cell
      aChl490_mid = astar490 * Chla_tot / d_sfc(k)        !Chla absorption at mid cell
      aCDOM490_mid = CDOM_tot / d_sfc(k)        !CDOM absorption at mid cell
      aOM1A490_mid = astarOMA * OM1A_tot / d_sfc(k) ! absorption at mid cell
      aOM1Z490_mid = astarOMZ * OM1Z_tot / d_sfc(k) ! absorption at mid cell
      aOM1R490_mid = astarOMR * OM1R_tot / d_sfc(k) ! absorption at mid cell
      aOM1BC490_mid = astarOMBC * OM1BC_tot / d_sfc(k) ! absorption at mid cell
      a490_mid = aSw_mid + aChl490_mid + aCDOM490_mid + aOM1A490_mid + aOM1Z490_mid + aOM1R490_mid + aOM1BC490_mid
!Calculate backscattering coefficients:
      bbChl490_mid = 0.015 * (0.3*((Chla_tot / d_sfc(k))**0.62)*(550./490.)) !Chla backscatter at mid cell
      bb490_mid = bbChl490_mid !Only Chla backscatters for now
! Calculate PAR at depth
      call IOP_PARattenuation(AMAX1(a490_mid,0.), bb490_mid, PARsurf, sun_zenith, d_sfc(k), PARdepth(k)) 
      PAR_percent(k) = 100.*PARdepth(k)/PARsurf
   enddo

! Calculate PAR at sea bottom
      aSw_bot = aw490 * bottom_depth(numdepths) !Sea water absorption at bottom of cell
      aChl490_bot = astar490 * Chla_tot / bottom_depth(numdepths) !Chla absorption at bottom
      aCDOM490_bot = CDOM_tot / bottom_depth(numdepths) !CDOM absorption at bottom
      aOM1A490_bot = astarOMA * OM1A_tot / bottom_depth(numdepths)    !A absorption at bottom
      aOM1Z490_bot = astarOMZ * OM1Z_tot / bottom_depth(numdepths) !FP absorption at bottom
      aOM1R490_bot = astarOMR * OM1R_tot / bottom_depth(numdepths) !SPM absorption at bottom
      aOM1BC490_bot = astarOMbc * OM1BC_tot / bottom_depth(numdepths) !INIT/BC absorption at bottom
      a490_bot = aSw_bot + aChl490_bot + aCDOM490_bot + aOM1A490_bot + aOM1Z490_bot + aOM1R490_bot + aOM1BC490_bot
      bbChl490_bot = 0.015 * (0.3*((Chla_tot / bottom_depth(numdepths))**0.62)*(550./490.))
!Chla backscatter at bottom
      bb490_bot = bbChl490_bot !Only Chla backscatters for now
      call IOP_PARattenuation(AMAX1(a490_bot,0.), bb490_bot, PARsurf, sun_zenith, bottom_depth(numdepths), PARbot)

   END Subroutine Call_IOP_PAR
!----------------------------------------------------------------------
!*****************************************************************************************
      subroutine IOP_PARattenuation(a490, bb490, PARsurf, sun_zenith, d_sfc, PARdepth)

! Penta et al., 2008 PAR penetration model: based on the IOP (inherent optical properties)
! absorption and backscatter at 490nm and the zenith angle of the sun; modified from Lee 
! et al., 2005 PAR penetration model: based on satellite absorption and backscater at 490nm

!    Lee, Z., K. Du, R. Arnone, S. Liew, and B. Penta, .Penetration of solar radiation
!         in the upper ocean: a numerical model for oceanic and coastal waters,. 
!         J. Geophys. Res. 110, C09019 doi:10.1029/2004JC002780 (2005).
!    Penta, B., Z. Lee, R.M. Kudela, S.L. Palacios, D.J. Gray, J.K. Jolliff, and 
!         I.G. Shulman, .An underwater light attenuation scheme for marine ecosystem 
!         models., Optics Express, 16, 16582-16591 (2008).

! Absorption (a490) is the total absorption at 490 nm calculated from all of the 
! constituents of the model that absorb light (Seawater, chlorophyll, CDOM, detritus, etc.
! Backscatter (bb490) is similar 

! Use the average value from the sea-surface to the depth of the calculation 
! (these calculations moved to a new subroutine(s) and the a490 and bb490 will be passed 
! into this subroutine  

! PAR (photosynthetically active radiation) just below the sea surface is used as a 
! starting value to be multiplied by the attenuation factor computed in this subroutine. 
! The starting value does not affect any of these calculations

! We generally use a factor of 0.4815 to represent the loss of PAR passing across the
! Air/Sea interface - this is included already in the data 

! Define coefficients for light attenuation model       
      real a490, alpha0, alpha1, alpha2, bb490, chi0, chi1, chi2, d_sfc
      real k1, k2, kpar, PARdepth, PARsurf, sun_zenith, zeta0, zeta1
      real zeta2 
     
! sun_zenith => Solar Zenith Angle in radians for time and location
! d_sfc => depth of center of cell from elevated sea surface        

! Set values of coefficients for light attenuation model 
! (Lee et al., 2005; Penta et al., 2008)       
      parameter(alpha0=0.090, alpha1=1.465, alpha2=-0.667, chi0=-0.057, chi1=0.482, & 
     &          chi2=4.221, zeta0=0.183, zeta1=0.702, zeta2=-2.567)
     
! Calculate the attenuation (Equation 2 Penta et al., 2008 errata)
      k1 = ((chi0 + chi1*(a490**0.5) + chi2*bb490) & 
     &     * (1 + alpha0 * sin(sun_zenith)))
     
      k2 = ((zeta0 + zeta1*a490 + zeta2*bb490) &
     &     * (alpha1 + alpha2 * cos(sun_zenith) ))
     
      kpar = k1 + (k2 / (1+(d_sfc))**0.5)
     
      PARdepth = PARsurf * exp(-(kpar*(d_sfc)))

      return
      end
!*****************************************************************************************
