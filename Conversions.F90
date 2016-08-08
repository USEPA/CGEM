Module Conversions 
!-----------------------------------------------------------------------
!-- Conversion factors between mg Chla and phytoplankton number density
!-----------------------------------------------------------------------
      real, parameter :: C2_chla_mg       = 3.00203293024339E-09 
      real, parameter :: C2_chla_mg_inv   = 1./C2_chla_mg
!-- Number of sec in 24 hr day
      real, parameter :: SDay = 86400.0 
!-- Convert quanta/cm2/s to mol photons/m2/d
!   N_Av=6.0221413E+23
!   quanta/cm2/s * 1 mol/N_av quanta * 10,000cm2/m2 * 86400s/d = mol/m2/d
      real, parameter :: convert_I   = 1. / 6.0221413 * 8.64 * 1.e-15 

END Module Conversions
