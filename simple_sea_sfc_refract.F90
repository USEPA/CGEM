!-----------------------------------------------------------------------------
      SUBROUTINE simple_sea_sfc_refract(Radbeamin, Raddiffin,          &
      &          reduce_irrad, SunZenithAtm, Radout             )

!---------------------------------------------------------------------
! Calculate the irradiance just under the sea surface based on Fresnel
! refraction expressions, the value of the irradiance in the atm just
! above the sea surface, and the underwater long light wavelength 
! irradiance reduction factor reduce_irrad which is about 0.43.  
!
! The irradiance value just above the sea surface, Radbeamin, is 
! assumed to not have the underwater long light wavelength irradiance 
! reduction factor taken into account.
!----------------------------------------------------------------------

!-------------------------------------------
! Declare parameters in parameter statements
!--------------------------------------------

     REAL    , PARAMETER   :: MinCosSunZenithAtm = 1.0E-5 
     real, parameter :: m_rel     = 1.33      ! m_rel = m_ocean/m_atm     
                                              ! where m_ocean is the index
                                              ! of refraction for the ocean
                                              ! (about 1.33), and m_atm is
                                              ! the index of refraction for
                                              ! the atmosphere (~=1.0)
        
     REAL    , PARAMETER   ::  mu_diff           = 1.0/SQRT(3.0) ! new 
     REAL    , PARAMETER   ::  mu_diff_inv       = 1.0/ mu_diff  ! new         

!-------------------------------------------
! Declare variables in subroutine interface.
!-------------------------------------------

      REAL   , INTENT(IN)  :: Radbeamin
      REAL   , INTENT(IN)  :: Raddiffin      
      REAL   , INTENT(IN)  :: reduce_irrad
      REAL   , INTENT(IN)  :: SunZenithAtm  ! in radians                
      REAL   , INTENT(OUT) :: Radout

!------------------------------------      
! Declare remaining scalar variables
!------------------------------------  
      
      REAL    :: CosSunZenithAtm = 0.0
      
      REAL    :: CosSunZenithOcn = 0.0 
      
      REAL    :: E_beam_ocn_d    = 0.0       
      REAL    :: E_diff_ocn_d    = 0.0 
      REAL    :: E_tot_ocn_d     = 0.0                 
      
      REAL    :: I_beam_ocn_d    = 0.0        
      REAL    :: I_diff_atm_d    = 0.0
      REAL    :: I_diff_ocn_d    = 0.0           
      
      REAL    :: m_rel_sq        = 0.0
      
      REAL    :: tau_cap_diff    = 0.0      
      
      REAL    :: temp1           = 0.0
      REAL    :: temp2           = 0.0
      REAL    :: temp3           = 0.0 
      REAL    :: temp3sq         = 0.0        
      REAL    :: temp4           = 0.0        
      REAL    :: temp4sq         = 0.0    
      REAL    :: temp5           = 0.0 
      REAL    :: temp5A          = 0.0    ! New        
      REAL    :: temp6           = 0.0            
      REAL    :: temp7           = 0.0          
!------------------------------------      
! Declare remaining array variables
!------------------------------------

! Begin computations
      m_rel_sq        = m_rel * m_rel
      
      CosSunZenithAtm = COS(SunZenithAtm)
            
      IF(CosSunZenithAtm <= 0.0) THEN
          CosSunZenithAtm = 0.0   ! No neg. cosines--
	                          ! they correspond 
	                          ! to night
      ELSE       
           CosSunZenithAtm = amax1(CosSunZenithAtm, MinCosSunZenithAtm)  
      ENDIF						  
						      
      temp1	      = CosSunZenithAtm * CosSunZenithAtm
      temp2           = (m_rel-1)*(1.0 + m_rel)
      		      
!     CosSunZenithOcn = (SQRT(temp1 + temp1))/m_rel  ! error in this
      CosSunZenithOcn = (SQRT(temp1 + temp2))/m_rel           !  (1)

!-----------------------------------------------------------------------
! Calculate the direct beam spectrally integrated downward radiation
! intensity  I_beam_atm_d just above the sea surface. We note that 
! Radbeamin is assumed to be the downward solar beam irradiance in 
! the atmosphere just above the sea surface, hence the use of the 
! formula below relating I_beam_atm_d, Radbeamin, and CosSunZenithAtm
!-----------------------------------------------------------------------

! Careful-- I_beam_atm_d--> infinity as CosSunZenithAtm --> 0. But not
!           to worry. What's really need in expressions below is the
!           product of I_beam_atm_d and CosSunZenithAtm which is bounded
!           and equal to Radbeamin
!
!      I_beam_atm_d = Radbeamin/CosSunZenithAtm
!
!----------------------------------------------------------------------
! Calculate tau_cap_beam, the transmission coefficent for 
!
!    (I_beam_d/m_rel_sq), 
!
! where 
!
!  I_beam_d      = the cross-sectionally averaged (across the beam)
!                  solar beam beam intensity. In the atmosphere, I_beam_d
!                  is I_beam_atm_d and in the ocean, it is I_beam_ocn_d
!                  where:
!
!  I_beam_ocn_d  = the cross-sectionally averaged (across the beam)
!                  solar beam beam intensity in the ocean just below
!                  the sea surface.
!
!
!  tau_cap_beam  = transmission coefficient of (I_beam_d/(m_rel**2)) across  
!                  the sea surface. This is calculated from the Fresnel 
!                  eqns. See p 186 & 319 &  and the Appendix E 
!                  (Reflectance and Transmission at an interface) in the 
!                  book entitled: "Radiative Transfer in the Atmosphere 
!                  and Ocean" by G.E. Thomas and K Stamnes.
!----------------------------------------------------------------------------
      temp3        =  CosSunZenithAtm + m_rel*CosSunZenithOcn
      temp4        =  CosSunZenithOcn + m_rel*CosSunZenithAtm
      
      temp3sq      = temp3 * temp3
      temp4sq      = temp4 * temp4
      
      temp5        = (1.0/temp3sq) + (1.0/temp4sq)

! See comment two lines below for revised calc of I_beam_ocn_d which
! doesn't use calced value of tau_cap_beam
      
!     tau_cap_beam = 2.0*m_rel*CosSunZenithOcn*CosSunZenithAtm*temp5

!---------------------------------------------------------------------      
! Using tau_cap_beam,I_beam_atm_d and m_rel_sq calculate the value of 
! I_beam_ocn_d. Note that:
!
!    I_beam_ocn_d = m_rel_sq * tau_cap_beam *I_beam_atm_d         (2)
!
! and that
!    (tau_cap_beam *I_beam_atm_d) = 
!                     2.0*m_rel*CosSunZenithOcn*CosSunZenithAtm*temp5 *
!                   * Radbeamin/CosSunZenithAtm
!
!                                 = 2.0*m_rel*CosSunZenithOcn*temp5*  (3)
!                                  *Radbeamin
!
! Substitute eqn (3) into eqn (2) and the latter becomes:
!
!    I_beam_ocn_d =  m_rel_sq * 2.0*m_rel*CosSunZenithOcn*temp5*Radbeamin (4)
!
! which can b e expressed as;
!
!     I_beam_ocn_d = m_rel_sq*2.0*(m_rel*CosSunZenithOcn)*temp5*Radbeamin (5)
!
! Substituting eqn (1) for CosSunZenithOcn  into eqn (5), the latter
! eqn becomes:
!
!     I_beam_ocn_d = 2.0*m_rel_sq*(SQRT(temp1 + temp2))*temp5*Radbeamin (6)
!
! defining temp6 as:
!
!     temp5A        = 2.0 * (SQRT(temp1 + temp2))*temp5                  (7)
!
! Substituting eqn 7 into eqn 6 and the latter becomes:
!
!     I_beam_ocn_d = m_rel_sq * temp5A * Radbeamin                       (8)
!
! We will use eqns 7 and 8 to evaluate I_beam_ocn_d
!--------------------------------------------------------------------- 
!     I_beam_ocn_d = m_rel_sq * tau_cap_beam *I_beam_atm_d

      temp5A        = 2.0      * (SQRT(temp1 + temp2)) * temp5        
      I_beam_ocn_d  = m_rel_sq * temp5A                * Radbeamin

!----------------------------------------------------------------------     
! Now calc E_beam_d_ocn, the solar beam downward irradiance just below 
! the sea surface.  It is calculated as follows:
!----------------------------------------------------------------------
      E_beam_ocn_d = CosSunZenithOcn * I_beam_ocn_d

!----------------------------------------------------------------------      
! This completes calculations for the solar beam. Now do calculations for
! the diffuse downward visible radiation in the atm and ocean near the
!  sea surface,
!----------------------------------------------------------------------

!--------------------------------------------------------------------------   
! The average cosine of the diffuse radiation propagation direction is
! approximated as being the same just above the sea surface and just below
! the sea surface. Designate this cosine as 
!
!       mu_diff.
! 
! We shall take it, based on recommendations in the Thomas and Stamnes 
! book, to be 1/SQRT(3). There are better oceanographically applicable
! approximations available, so we consider 1/SQRT(3) to be a placeholder 
! until a better approximation can be installed at a later time..
!-------------------------------------------------------------------------- 

!----------------------------------------------------------------------
! Calculate tau_cap_diff, the diffuse radiation downward transmission 
! coefficient across the seasurface. Since the average cosine of the 
! diffuse radiation propagation direction is approximated as being the 
! same just above the sea surface and just below the sea surface, the
! genl. expression for the transmission coefficient simplifies to the
! following:
!----------------------------------------------------------------------
      temp6        = 1.0 + m_rel
      temp7        = temp6 * temp6

      tau_cap_diff = 4.0*m_rel/temp7
      
!-----------------------------------------------------------------------
! Calculate the diffuse spectrally integrated downward radiation
! intensity  I_diff_atm_d just above the sea surface. We note that 
! Raddiff in is assumed to be the downward diffuse irradiance in 
! the atmosphere just above the sea surface, hence the use of the 
! formula below relating I_diff_atm_d, Raddiffin, and mu_diff
!-----------------------------------------------------------------------
      I_diff_atm_d = Raddiffin * mu_diff_inv     

!----------------------------------------------------------------------     
! Now calc I_diff_ocn_d, the diffuse downward azmuthally averaged 
! visible radiation intensity just below the sea surface.  It is 
! calculated as follows:
!----------------------------------------------------------------------
      I_diff_ocn_d = tau_cap_diff * I_diff_atm_d

!----------------------------------------------------------------------     
! Now calc E_diff_ocn_d, the diffuse downward azmuthally averaged 
! visible radiation irradiance just below the sea surface.  It is 
! calculated as follows:
!----------------------------------------------------------------------
      E_diff_ocn_d = mu_diff * I_diff_ocn_d

!--------------------------------------------------------------------------
! Calculate E_tot_ocn_d, the total downward irradiance just below the
! sea surface. This is the amount available for propagation into the ocean
!--------------------------------------------------------------------------
      E_tot_ocn_d = E_beam_ocn_d + E_diff_ocn_d

!-------------------------------------------------------------------------
! Now adjust  E_tot_ocn_d to eliminate the long wavelength contributions
! to  E_tot_ocn_d. These die away so close to the sea surface that they
! are effectively not available in the layers of the model, except perhaps
! the first one, if it is thin enough. the adjusted downward irradiance
! is set equal to Radout.
!-------------------------------------------------------------------------
      Radout = reduce_irrad * E_tot_ocn_d

      RETURN
  
      END SUBROUTINE simple_sea_sfc_refract
!----------------------------------------------------------------- 
