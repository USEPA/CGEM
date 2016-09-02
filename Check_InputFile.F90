Subroutine Check_InputFile()

USE Model_dim
USE INPUT_VARS
USE TEMP_VARS

IMPLICIT NONE


integer isp

!read(999,*) Which_fluxes

!read(999,*) Which_temperature
if(Which_temperature.ne.1.and.Which_temperature.ne.2.and.Which_temperature.ne.3) then
  write(6,*) "Using Sigmoidal temperature function"
  Which_temperature=1
endif

!read(999,*) Which_uptake
if(Which_uptake.ne.1.and.Which_uptake.ne.2.and.Which_uptake.ne.3) then
  write(6,*) "Using Michaelis-Menten uptake"
  Which_uptake=1
endif

!read(999,*) Which_quota
if(Which_quota.ne.1.and.Which_quota.ne.2.and.Which_quota.ne.3) then
  write(6,*) "Using Droop"
  Which_quota=1
endif


!read(999,*) Which_irradiance 
if(Which_irradiance.ne.1.and.Which_irradiance.ne.2) then
  write(6,*) "Using Penta Light Model"
  Which_irradiance=1
endif

!read(999,*) Which_chlaC
if(Which_chlaC.ne.1.and.Which_chlaC.ne.2) then
  write(6,*) "Using Regression for chlA:C"
  Which_chlaC=1
endif

!read(999,*) Which_photosynthesis 
if(Which_photosynthesis.ne.1.and.Which_photosynthesis.ne.2.and.Which_photosynthesis.ne.3) then
  write(6,*) "Using photoinhibition"
  Which_photosynthesis=1
endif

if(Which_photosynthesis.eq.3.and.Which_growth.ne.3) then
   write(6,*) "Setting Which_growth=3 to match Which_photosynthesis option"
   Which_growth = 3
endif

!read(999,*) Which_growth
if(Which_growth.ne.1.and.Which_growth.ne.2.and.Which_growth.ne.3) then
  write(6,*) "Using law of minimum growth formulation"
  Which_growth=1
endif

if(Which_growth.eq.3.and.Which_photosynthesis.ne.3) then
   write(6,*) "Setting Which_growth=3 to match Which_photosynthesis option"
   Which_photosynthesis = 3
endif

!read(999,*) SolarRad
if(SolarRad.ne.0.and.SolarRad.ne.1) then
  write(6,*) "Using COAMPS Solar Radiation"
  SolarRad=1
endif

!read(999,*) InitializeHow
if(InitializeHow.ne.0.and.InitializeHow.ne.1.and.InitializeHow.ne.2.and.InitializeHow.ne.3) then
  write(6,*) "Initializing using Salinity regression equations"
  InitializeHow=0
endif

! When InitializeHow = 3, RESTART_FILE_TIMESTEP needs to be specified.
if((InitializeHow.eq.3) .and. (RESTART_FILE_TIMESTEP.eq.0)) then
  write(6,*) "RESTART_FILE_TIMESTEP should be a non-zero value when InitilaizeHow = 3."
  stop
endif


!read(999,*) Which_Vmix   
if(Which_VMix.ne.0.and.Which_Vmix.ne.1) then
  write(6,*) "Using Vertical Mixing"
  Which_Vmix=1
endif

!read(999,*) Which_Outer_BC
if(Which_Outer_BC.ne.0.and.Which_Outer_BC.ne.1.and.Which_Outer_BC.ne.2.and.Which_Outer_BC.ne.3.and.Which_Outer_BC.ne.5) then
  write(6,*) "Using Salinity Boundary Conditions"
  Which_Outer_BC=0
endif

!read(999,*) wt_l,wt_o 
!Test to make sure weights are between 0 and 1:
if(wt_l.gt.1.or.wt_l.lt.0) then
  write(6,*) "wt_l must be between 0 and 1"
  stop
endif

if(wt_o.gt.1.or.wt_o.lt.0) then
  write(6,*) "wt_o must be between 0 and 1"
  stop
endif

!Do a test to make sure Qmax is greater than Qmin:
do isp=1,nospA
   if(QmaxN(isp).le.QminN(isp)) then
     write(6,*) "Please set QmaxN greater than QminN"
     stop
   endif
   if(QmaxP(isp).le.QminP(isp)) then
     write(6,*) "Please set QmaxP greater than QminP"
     stop
   endif
enddo

! An hour (3600 secs) must be divisible by dT:
if(mod(3600,dT).ne.0) then
  write(6,*) "Please pick a dT that is a divisor of 3600 seconds (an hour)."
!  stop
endif


return
END SUBROUTINE Check_InputFile
