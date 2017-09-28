SUBROUTINE GOMDOM(TC_8)
!------------------------------------------------------------------------------
!

USE Model_dim
USE Grid
USE Hydro
USE State_Vars
USE STATES
USE INPUT_VARS, ONLY: Read_Solar,ws,dT
USE INPUT_VARS_GD, ONLY : Which_Fluxes,Which_irradiance
USE EUT
USE Which_Flux
USE InRemin
USE FLAGS, ONLY : DoDroop
USE LIGHT_VARS, ONLY: PARfac

IMPLICIT NONE

INTEGER*8, INTENT(IN) :: TC_8
REAL :: DTM(im,jm,km,nf),PAR(im,jm,km)
REAL :: SETRATE(nf)
INTEGER :: i,j,k,nz
REAL dTime

dTime = real(dT)

!
!------------------------------------------------------------------------------
!
!
if(Which_irradiance.eq.0) then
  PAR(:,:,1) = Rad(:,:)  * PARfac
  if(Read_Solar.ne.2) then
    PAR(:,:,1) = PAR(:,:,1) * .47 * 4.57 /2.77e14  
  endif 
elseif(Which_irradiance.eq.1) then
  call GD_Light_Model(f,S,Rad,PAR,dz)
elseif(Which_irradiance.eq.2) then
  call Brad_Light_Model(f,TC_8,lat,lon,Rad,d,d_sfc,PAR)
else
   write(6,*) "Error in light model"
   stop
endif


if(DoDroop.eq.1) then
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                ! Zooplankton kinetics
         CALL DIATOMS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k)            ! Diatom kinetics
         CALL GREENS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k)             ! Greens kinetics
         CALL CARBON(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH_droop(f(i,j,k,:),DTM(i,j,k,:),PAR(i,j,k),i,j,k)                      ! Phosphorous kinetics
         CALL NITROG_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             ! Silica kinetics
         CALL DISSOLVED_OXYGEN(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dTime,i,j,k)   ! Dissolved Oxygen
      enddo
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                !  Zooplankton kinetics
         CALL DIATOMS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k) ! Diatom kinetics
         CALL GREENS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k) ! Greens kinetics
         CALL CARBON(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH(f(i,j,k,:),DTM(i,j,k,:),i,j,k)                      !  Phosphorous kinetics
         CALL NITROG(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             !  Silica kinetics
         CALL DISSOLVED_OXYGEN(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dTime,i,j,k)   !  Dissolved Oxygen
      enddo
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif


if(Which_Fluxes(iInRemin).eq.1) then

if(DoDroop.eq.1) then
!Do settling fluxes for instant remineralization
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
       if(nz.ge.0.and.wsm(i,j).eq.0) then !If on shelf
         CALL EXCHANGE_droop(f(i,j,nz,:),area(i,j),Vol(i,j,nz),dTime,i,j,SETRATE(:))      ! Calculate IR fluxes and settling rates
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(nza(i,j) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
!Do settling fluxes for instant remineralization
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
       if(nz.gt.0.and.wsm(i,j).eq.0) then !If on shelf
         CALL EXCHANGE(f(i,j,nz,:),area(i,j),Vol(i,j,nz),dTime,i,j,SETRATE(:))      !  Calculate IR fluxes and settling rates
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(nza(i,j) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

endif


 do j = 1,jm
     do i = 1,im 
         nz = nza(i,j)
      do k = 1, nz
         f(i,j,k,:) = max(f(i,j,k,:) + DTM(i,j,k,:) * dTime,0.)
      enddo
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop


if(Which_Fluxes(iInRemin).eq.1) then
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
       if(nz.ge.0.and.wsm(i,j).eq.0) then
         f(i,j,nz,:) = max(f(i,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dTime,0.)
       endif !End of if(nza(i,j) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

if(Which_Fluxes(iInRemin).eq.2) then
 do j = 1,jm
     do i = 1,im
         nz = nza(i,j)
       if(nz.ge.0.and.wsm(i,j).eq.0) then
         SETRATE(:) = f(i,j,nz,:)*area(i,j)*(-ws(:))
         f(i,j,nz,:) = max(f(i,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dTime,0.)
       endif !End of if(nza(i,j) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif


!
!-----------------------------------------------------------------------------


RETURN

END SUBROUTINE GOMDOM
