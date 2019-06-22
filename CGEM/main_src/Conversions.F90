Module Conversions 

!-- Number of sec in 24 hr day
      real, parameter :: SDay = 86400.0 
      integer, parameter :: iSDay = 86400
!-- Convert quanta/cm2/s to mol photons/m2/d
!   N_Av=6.0221413E+23
!   quanta/cm2/s * 1 mol/N_av quanta * 10,000cm2/m2 * 86400s/d = mol/m2/d
      real, parameter :: convert_I   = 1. / 6.0221413 * 8.64 * 1.e-15 

END Module Conversions
