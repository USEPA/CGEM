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

  real   , intent(in)    :: totChl(km)  ! total Chl-a (mg/m3)

  real   , intent(in)    :: CDOM_k(km)  ! CDOM (ppb) 

  real   , intent(in)    :: OM1_A(km)   ! Concentration of particulate
                                         ! dead phytoplankton (g/m3)

  real   , intent(in)    :: OM1_Z(km)   ! Concentration of particulate
                                         ! fecal pellets (g/m3)

  real   , intent(in)    :: OM1_R(km)   ! Concentration of particulate
                                         ! river generated SPM (g/m3)

  real   , intent(in)    :: OM1_BC(km)  ! Concentration of particulate
                                         ! initial and boundary condition generated SPM (g/m3)

  real   , intent(in)    :: bottom_depth(km)
                                         ! depths(k) is the depth
                                         ! at the Bottom of layer k.
                                         ! it is assumed that the top
                                         ! of layer k=1 has a depth
                                         ! of zero.

  real   , intent(in)    :: d_sfc(km)   ! depth at center of cell k from surface

  integer, intent(in)    :: numdepths    ! Total number of layers in water
                                         ! column

  real  , intent(out)    :: PAR_percent(km) ! PAR_percent(k) is the % of
                                             ! incoming visible
                                             ! irradiance, PARsurf.

  real  , intent(out)    :: PARbot           ! PARbot is Par,the
                                             ! visible irradiance,
                                             ! at the sea bottom 
                                             ! (quanta/cm**2/sec)

  real  , intent(out)    :: PARdepth(km)    ! PARdepth(k) is Par, the
                                             ! visible irradiance
                                             ! at the middle of layer k.
                                             ! (quanta/cm**2/sec)

!----------------------------------------------------------------
! Calculate absorption (490 nm) components: seawater, chl, SPM from rivers, CDOM,
! detritus (dead cells), fecal pellets ...
      real Chla_tot(km), CDOM_tot(km), OM1A_tot(km), OM1Z_tot(km), OM1R_tot(km), OM1BC_tot(km), CDOM(km)
      real Chla_mass(km), CDOM_mass(km), OM1A_mass(km), OM1Z_mass(km), OM1R_mass(km), OM1BC_mass(km)
      real a490_mid, aSw_mid, aChl490_mid, aCDOM490_mid, bbChl490_mid, bb490_mid
      real a490_bot, aSw_bot, aChl490_bot, aCDOM490_bot, bbChl490_bot, bb490_bot
      real aOM1A490_mid, aOM1Z490_mid, aOM1R490_mid, aOM1BC490_mid
      real aOM1A490_bot, aOM1Z490_bot, aOM1R490_bot, aOM1BC490_bot
      real cell_depth, bd_km1
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
      CDOM(k) = AMAX1(CDOM(k),0.)
   enddo 

!Initialize counters for Chla, CDOM, and detritus:
   Chla_tot = 0.
   CDOM_tot = 0.
   OM1A_tot = 0.
   OM1Z_tot = 0.
   OM1R_tot = 0.
   OM1BC_tot = 0.
   bd_km1 = 0.

!Mass in each cell at layer k (area of volume part cancels out)
!The unit is mg[mmol] / m2
   do k=1,numdepths
      cell_depth = bottom_depth(k) - bd_km1
      bd_km1 = bottom_depth(k)
      Chla_mass(k) = totChl(k)*cell_depth
      CDOM_mass(k) = CDOM(k)*cell_depth
      OM1A_mass(k) = OM1_A(k)*cell_depth
      OM1Z_mass(k) = OM1_Z(k)*cell_depth
      OM1R_mass(k) = OM1_R(k)*cell_depth
      OM1BC_mass(k) = OM1_BC(k)*cell_depth
   enddo

!Mass from surface to center of cell at layer k
!Is the sum of the mass of all previous k layers plus 
!half of the current k layer 
!Concentration is that divided by the distance
!from the surface to the center of cell at layer k
!(Division by d_sfc is in the next loop)  
      Chla_tot(1) = 0.5*Chla_mass(1)
      CDOM_tot(1) = 0.5*CDOM_mass(1)
      OM1A_tot(1) = 0.5*OM1A_mass(1)
      OM1Z_tot(1) = 0.5*OM1Z_mass(1)
      OM1R_tot(1) = 0.5*OM1R_mass(1)
      OM1BC_tot(1) = 0.5*OM1BC_mass(1)
   do k=2,numdepths
      Chla_tot(k)  = 0.5*Chla_mass(k) + SUM(Chla_mass(1:k-1))
      CDOM_tot(k)  = 0.5*CDOM_mass(k) + SUM(CDOM_mass(1:k-1))
      OM1A_tot(k)  = 0.5*OM1A_mass(k) + SUM(OM1A_mass(1:k-1))
      OM1Z_tot(k)  = 0.5*OM1Z_mass(k) + SUM(OM1Z_mass(1:k-1))
      OM1R_tot(k)  = 0.5*OM1R_mass(k) + SUM(OM1R_mass(1:k-1))
      OM1BC_tot(k) = 0.5*OM1BC_mass(k)+ SUM(OM1BC_mass(1:k-1))
   enddo


   do k=1,numdepths
!Calculate absorption coefficients:
      aSw_mid = aw490  !Sea water absorption at mid cell
      aChl490_mid = astar490 * Chla_tot(k) / d_sfc(k)        !Chla absorption at mid cell
      aCDOM490_mid = CDOM_tot(k) / d_sfc(k)        !CDOM absorption at mid cell
      aOM1A490_mid = astarOMA * OM1A_tot(k) / d_sfc(k) ! absorption at mid cell
      aOM1Z490_mid = astarOMZ * OM1Z_tot(k) / d_sfc(k) ! absorption at mid cell
      aOM1R490_mid = astarOMR * OM1R_tot(k) / d_sfc(k) ! absorption at mid cell
      aOM1BC490_mid = astarOMBC * OM1BC_tot(k) / d_sfc(k) ! absorption at mid cell
      a490_mid = aSw_mid + aChl490_mid + aCDOM490_mid + aOM1A490_mid + aOM1Z490_mid + aOM1R490_mid + aOM1BC490_mid
!Calculate backscattering coefficients:
      bbChl490_mid = 0.015 * (0.3*((Chla_tot(k) / d_sfc(k))**0.62)*(550./490.)) !Chla backscatter at mid cell
      bb490_mid = bbChl490_mid !Only Chla backscatters for now
! Calculate PAR at depth
      !Why would we check if a490_mid=0??
      !If it is zero, stop.
      !  if(a490_mid.le.0) then
      !     write(6,*) k,CDOM(k),CDOM_k(k)
      !     write(6,*) "a490_mid.le.0, =",a490_mid,aSw_mid,aChl490_mid,aCDOM490_mid
      !     !stop
      !  endif
      call IOP_PARattenuation(a490_mid, bb490_mid, PARsurf, sun_zenith, d_sfc(k), PARdepth(k)) 
      PAR_percent(k) = 100.*PARdepth(k)/PARsurf

      if(PARdepth(k).ne.PARdepth(k)) then
          write(6,*) "k,d,Par,a,b,Rad,zenith",k,d_sfc(k),PARdepth(k),a490_mid, bb490_mid, PARsurf, sun_zenith
          write(6,*) "k,Sw,Chl,CDOM,OM1A,OM1Z,OM1R,OM1BC",k,aSw_mid,aChl490_mid, aCDOM490_mid , aOM1A490_mid , aOM1Z490_mid , aOM1R490_mid,aOM1BC490_mid
      endif
   enddo

! Calculate PAR at sea bottom
      aSw_bot = aw490  !Sea water absorption at bottom of cell
      aChl490_bot = astar490 * (Chla_tot(numdepths)+0.5*Chla_mass(numdepths)) / bottom_depth(numdepths) !Chla absorption at bottom
      aCDOM490_bot = CDOM_tot(numdepths)+(0.5*CDOM_mass(numdepths)) / bottom_depth(numdepths) !CDOM absorption at bottom
      aOM1A490_bot = astarOMA * (OM1A_tot(numdepths)+0.5*OM1A_mass(numdepths)) / bottom_depth(numdepths)    !A absorption at bottom
      aOM1Z490_bot = astarOMZ * (OM1Z_tot(numdepths)+0.5*OM1Z_mass(numdepths)) / bottom_depth(numdepths) !FP absorption at bottom
      aOM1R490_bot = astarOMR * (OM1R_tot(numdepths)+0.5*OM1R_mass(numdepths)) / bottom_depth(numdepths) !SPM absorption at bottom
      aOM1BC490_bot = astarOMbc * (OM1BC_tot(numdepths)+0.5*OM1BC_mass(numdepths)) / bottom_depth(numdepths) !INIT/BC absorption at bottom
      a490_bot = aSw_bot + aChl490_bot + aCDOM490_bot + aOM1A490_bot + aOM1Z490_bot + aOM1R490_bot + aOM1BC490_bot
      bbChl490_bot = 0.015 * (0.3*(((Chla_tot(numdepths)+0.5*Chla_mass(numdepths)) / bottom_depth(numdepths))**0.62)*(550./490.))
!Chla backscatter at bottom
      bb490_bot = bbChl490_bot !Only Chla backscatters for now
      call IOP_PARattenuation(a490_bot, bb490_bot, PARsurf, sun_zenith, bottom_depth(numdepths), PARbot)

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
