      Subroutine Meta_SOC(f,T,S,h,A_C,A_N,Z_C,Z_N)

      USE Model_dim
      USE CGEM_Vars
      USE INPUT_VARS
      USE INPUT_VARS_CGEM
      USE Conversions

      IMPLICIT NONE

      real, intent(inout) :: f(nf)        !State variable array, (mmol/m3)
      real, intent(in)    :: T            !Temperature (bottom sigma layer), (C)
      real, intent(in)    :: S            !Salinity (bottom sigma layer)
      real, intent(in)    :: h            !Thickness of bottom sigma layer
      real, intent(in)    :: A_C, A_N     !Carbon and Nitrogen stoichiometry for phytoplankton
      real, intent(in)    :: Z_C, Z_N     !Carbon and Nitrogen stoichiometry for zooplankton
      real :: F_POM_A, F_POM_Z, F_POM_R, F_POM_BC  !Flux of particle organic matter
      real :: F_POM                       !Total flux of particle organic matter
      real :: O2, NO3, NH4                 
      real :: F_O2, F_NO3, F_NH4

!      real :: DIC, ALK     !Testing Additional Terms
!      real :: F_DIC, F_ALK !Testing Additional Terms

!Calculate organic matter fluxes, "-SDay" converts from -m/s to m/d
!Then convert from C to N
      F_POM_A = -SDay*ws(iOM1_A)*f(iOM1_A)*A_N/A_C
      F_POM_Z = -SDay*ws(iOM1_Z)*f(iOM1_Z)*Z_N/Z_C
      F_POM_R = -SDay*ws(iOM1_R)*f(iOM1_R)*4.5/51.
      F_POM_BC = -SDay*ws(iOM1_BC)*f(iOM1_BC)*16./106.
      F_POM = F_POM_A + F_POM_Z + F_POM_R + F_POM_BC  


!Rename variables for equation readability
      O2  = f(iO2)
      NO3 = f(iNO3)
      NH4 = f(iNH4)
!Testing Additional Terms
!      DIC = f(iDIC)
!      ALK = f(iALK)

!Calculate fluxes based on meta-model approach
      F_O2 = 22.1151   -1.3381*F_POM + 0.0286*F_POM**2  -0.0001*F_POM**3 &
     & + 0.8138*S + 0.0868*S**2  -0.0023*S**3  -7.1247*T + 0.3668*T**2  &
     & -0.0069*T**3 + 0.4592*NH4  -0.2074*NH4**2 + 0.0112*NH4**3  -0.8055*NO3 + &
     & 0.0229*NO3**2  -0.0001*NO3**3  -0.0721*O2  -0.0001*O2**2
      !write(6,*) "F_O2",F_O2, F_O2/h*dT/Sday


      F_NH4 = -10.8192 +  0.0740*F_POM + 0.0023*F_POM**2  &
     & -0.0001*F_POM**3  -0.0833*S  -0.0064*S**2 + 0.0002*S**3 + 2.0967*T  &
     & -0.0996*T**2 + 0.0016*T**3  -0.2221*NH4 + 0.0500*NH4**2  -0.0023*NH4**3 + &
     & 0.0836*NO3  -0.0024*NO3**2  -0.0283*O2 + 0.0002*O2**2 
      !write(6,*) "F_NH4",F_NH4, F_NH4/h*dT/Sday


      F_NO3 = 3.6115   -0.0071*F_POM  -0.0014*F_POM**2 &
     & + 0.0463*S  -0.0035*S**2 + 0.0001*S**3  -0.5613*T + 0.0238*T**2  &
     & -0.0003*T**3 + 0.1142*NH4  -0.0209*NH4**2 + 0.0008*NH4**3  -0.0134*NO3 + &
     & 0.0001*NO3**2 + 0.0144*O2  -0.0001*O2**2 

!Testing Additional Terms
!      F_DIC = -F_O2 + 1.25*F_NO3
!      F_ALK = -F_NH4 + F_NO3

!Update state variables with flux values
     !O2, NH4, NO3
      f(iO2) =  AMAX1(f(iO2)  + F_O2/h*dT/Sday,0.)
      f(iNH4) = AMAX1(f(iNH4) + F_NH4/h*dT/Sday,0.)
      f(iNO3) = AMAX1(f(iNO3) + F_NO3/h*dT/Sday,0.)
!Testing additional terms
!      f(iDIC) = AMAX1(f(iDIC) + F_DIC/h*dT/Sday,0.)
!      f(iALK) = AMAX1(f(iALK) + F_ALK/h*dT/Sday,0.)

!Subtract OM flux, with conversion back to C from N
      f(iOM1_A) = AMAX1(f(iOM1_A)  - (A_C/A_N)*F_POM_A/h*dT/Sday,0.)
      f(iOM1_Z) = AMAX1(f(iOM1_Z)  - (Z_C/Z_N)*F_POM_Z/h*dT/Sday,0.)
      f(iOM1_R) = AMAX1(f(iOM1_R)  - (51./4.5)*F_POM_R/h*dT/Sday,0.)
      f(iOM1_BC) = AMAX1(f(iOM1_BC)  - (106./16.)*F_POM_BC/h*dT/Sday,0.)

      RETURN

      END Subroutine Meta_SOC 
