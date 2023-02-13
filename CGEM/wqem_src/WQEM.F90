SUBROUTINE WQEM(TC_8)
!------------------------------------------------------------------------------
!

USE Model_dim
USE Grid
USE Hydro
USE State_Vars
USE STATES
USE INPUT_VARS, ONLY: Read_Solar,ws,dT
USE INPUT_VARS_WQEM, ONLY : Which_Fluxes,Which_irradiance
USE EUT
USE Which_Flux
USE InRemin
USE FLAGS, ONLY : DoDroop
USE LIGHT_VARS, ONLY: PARfac
USE RiverLoad

IMPLICIT NONE

INTEGER*8, INTENT(IN) :: TC_8
REAL :: DTM(im,jm,km,nf),PAR(im,jm,km)
REAL :: SETRATE(nf)
INTEGER :: i,j,k,nz,myi
REAL dTime
INTEGER :: icell, jcell
REAL :: rivLoadConvFactor

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
  call WQEM_Light_Model(f,S,Rad,PAR,dz)
elseif(Which_irradiance.eq.2) then
  call Brad_Light_Model(f,TC_8,lat,lon,Rad,d,d_sfc,PAR)
else
   write(6,*) "Error in light model"
   stop
endif


if(DoDroop.eq.1) then
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                ! Zooplankton kinetics
         CALL DIATOMS_droop(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k)            ! Diatom kinetics
         CALL GREENS_droop(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k)             ! Greens kinetics
         CALL CARBON(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH_droop(f(myi,j,k,:),DTM(i,j,k,:),PAR(i,j,k),i,j,k)                      ! Phosphorous kinetics
         CALL NITROG_droop(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             ! Silica kinetics
         CALL DISSOLVED_OXYGEN(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dTime,i,j,k)   ! Dissolved Oxygen
      enddo
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                !  Zooplankton kinetics
         CALL DIATOMS(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k) ! Diatom kinetics
         CALL GREENS(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dTime,i,j,k) ! Greens kinetics
         CALL CARBON(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH(f(myi,j,k,:),DTM(i,j,k,:),i,j,k)                      !  Phosphorous kinetics
         CALL NITROG(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             !  Silica kinetics
         CALL DISSOLVED_OXYGEN(f(myi,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dTime,i,j,k)   !  Dissolved Oxygen
      enddo
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif


if(Which_Fluxes(iInRemin).eq.1) then

if(DoDroop.eq.1) then
!Do settling fluxes for instant remineralization
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
       if(nz.ge.0.and.wsm(i,j).eq.0) then !If on shelf
         CALL EXCHANGE_droop(f(myi,j,nz,:),area(i,j),Vol(i,j,nz),dTime,i,j,SETRATE(:))      ! Calculate IR fluxes and settling rates
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(nza(i,j) statement
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
!Do settling fluxes for instant remineralization
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
       if(nz.gt.0.and.wsm(i,j).eq.0) then !If on shelf
         CALL EXCHANGE(f(myi,j,nz,:),area(i,j),Vol(i,j,nz),dTime,i,j,SETRATE(:))      !  Calculate IR fluxes and settling rates
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(nza(i,j) statement
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

endif


!---------------------------------------------------------------
! Add river loads.
!---------------------------------------------------------------
do i=1,nriv             ! Loop over the rivers
  icell = riversIJ(i,1) ! Extract the i index of grid cell where river discharges
  jcell = riversIJ(i,2) ! Extract the j index of grid cell where river discharges
  do k=1,nsl            ! Loop over the sigma layers
    rivLoadConvFactor = weights(i,k) / Vol(icell,jcell,k)
    DTM(icell,jcell,k,JTR) = DTM(icell,jcell,k,JTR) + Riv1(i) * rivLoadConvFactor
  enddo
enddo


 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
      do k = 1, nz
         f(myi,j,k,:) = max(f(myi,j,k,:) + DTM(i,j,k,:) * dTime,0.)
      enddo
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop


if(Which_Fluxes(iInRemin).eq.1) then
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
       if(nz.ge.0.and.wsm(i,j).eq.0) then
         f(myi,j,nz,:) = max(f(myi,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dTime,0.)
       endif !End of if(nza(i,j) statement
      myi = myi + 1
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

if(Which_Fluxes(iInRemin).eq.2) then
 do j = 1,jm
     myi=1
     do i = myi_start,myi_end
         nz = nza(i,j)
!       if(nz.ge.0.and.wsm(i,j).eq.0) then
       if(nz.gt.0.and.fm(i,j,1).eq.1) then
         SETRATE(:) = f(myi,j,nz,:)*area(i,j)*(-ws(:))
         f(myi,j,nz,:) = max(f(myi,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dTime,0.)
       endif !End of if(nza(i,j) statement
   enddo      ! end of do i block do loop
      myi = myi + 1
 enddo      ! end of do j block do loop
endif


!
!-----------------------------------------------------------------------------


RETURN

END SUBROUTINE WQEM
