Subroutine Check_InputFile()

USE Model_dim
USE INPUT_VARS
USE INPUT_VARS_CGEM
USE TEMP_VARS

IMPLICIT NONE

integer isp
real x

!read(999,*) Which_fluxes

!read(999,*) Which_temperature
if(Which_temperature.ne.1.and.Which_temperature.ne.2.and.Which_temperature.ne.3.and.Which_temperature.ne.4) then
  write(6,*) "Which_temperature is outside of range 1-4"
  stop 
endif

!read(999,*) Which_uptake
if(Which_uptake.ne.1.and.Which_uptake.ne.2.and.Which_uptake.ne.3) then
  write(6,*) "Which_uptake is outside of range 1-3"
  stop
endif

!read(999,*) Which_quota
if(Which_quota.ne.1.and.Which_quota.ne.2.and.Which_quota.ne.3.and.Which_quota.ne.4) then
  write(6,*) "Which_quota is outside of range 1-4"
  stop
endif


!read(999,*) Which_irradiance 
if(Which_irradiance.ne.1.and.Which_irradiance.ne.2.and.Which_irradiance.ne.3) then
  write(6,*) "Which_irradiance is outside of range 1-3"
  stop
endif

!read(999,*) Which_chlaC
if(Which_chlaC.ne.1.and.Which_chlaC.ne.2) then
  write(6,*) "Which_chlaC is outside of range 1-2"
  stop
endif

!read(999,*) Which_photosynthesis 
if(Which_photosynthesis.ne.1.and.Which_photosynthesis.ne.2.and.Which_photosynthesis.ne.3.and.Which_photosynthesis.ne.4) then
  write(6,*) "Which_photosynthesis is outside of range 1-4"
  stop
endif

if(Which_photosynthesis.eq.3.and.Which_growth.ne.3) then
  write(6,*) "If Which_photosynthese=3 then Which_growth should = 3"
  stop
endif

!read(999,*) Which_growth
if(Which_growth.ne.1.and.Which_growth.ne.2.and.Which_growth.ne.3) then
  write(6,*) "Which_growth is outside of range 1-3"
  stop
endif

if(Which_growth.eq.3.and.Which_photosynthesis.ne.3) then
  write(6,*) "If Which_growth=3 then Which_photosynthesis should =3"
  stop
endif

!read(999,*) SolarRad
!if(SolarRad.ne.0) then
!  write(6,*) "SolarRad will be calculated, currently the only option"
!endif


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
endif

! Diatom/Non Diatom check:
do isp=1,nospA
   if(KSi(isp).le.tiny(x).and.vmaxSi(isp).gt.0.) then
     write(6,*) "If KSi=0, then vmaxSi must =0 (designates non-diatoms)"
     stop
   endif
   if(vmaxSi(isp).le.tiny(x).and.KSi(isp).gt.0.) then
     write(6,*) "If vmaxSi=0, then KSi must =0 (designates non-diatoms)"
     stop
   endif
enddo


return
END SUBROUTINE Check_InputFile
