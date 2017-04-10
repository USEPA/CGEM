SUBROUTINE EUTRO(f,TC_8,T,S,Rad,lat,lon,fm,wsm,d,d_sfc,dz,Vol,dTime)
!------------------------------------------------------------------------------
!

USE Model_dim
USE STATES
USE INPUT_VARS_GD, ONLY : Which_Fluxes,Read_Solar,ws,Which_irradiance
USE EUT
USE Which_Flux
USE InRemin
USE FLAGS, ONLY : DoDroop
USE LIGHT_VARS, ONLY: PARfac

IMPLICIT NONE

REAL, INTENT(INOUT) :: f(im,jm,nsl,nf)
REAL, INTENT(IN)    :: T(im,jm,nsl),S(im,jm,nsl),Rad(im,jm)
REAL, INTENT(IN)    :: lat(jm),lon(im) !Latitude and longitude of each grid cell
INTEGER, INTENT(IN) :: fm(im,jm),dTime
INTEGER*8, INTENT(IN) :: TC_8
REAL, INTENT(IN) :: d(im,jm,nsl),dz(im,jm,nsl),Vol(im,jm,nsl),wsm(im,jm),d_sfc(im,jm,nsl)
REAL :: DTM(im,jm,nsl,nf),SAL_TERM,CHL_TERM,POC_TERM,PAR(im,jm,nsl)
REAL :: IATTOP, IATBOT(im,jm,nsl),OPTDEPTH,Rad_Watts(im,jm)
REAL :: SETRATE(nf),area
INTEGER :: i,j,k

REAL dT

dT = real(dTime)
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
  call GD_Light_Model(f,S,Rad,PAR,fm,dz,dT)
elseif(Which_irradiance.eq.2) then
  call Brad_Light_Model(f,fm,TC_8,lat,lon,Rad,d,d_sfc,PAR)
else
   write(6,*) "Error in light model"
   stop
endif

if(DoDroop.eq.1) then
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                ! Zooplankton kinetics
         CALL DIATOMS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dT,i,j,k)            ! Diatom kinetics
         CALL GREENS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dT,i,j,k)             ! Greens kinetics
         CALL CARBON(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH_droop(f(i,j,k,:),DTM(i,j,k,:),i,j,k)                      ! Phosphorous kinetics
         CALL NITROG_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             ! Silica kinetics
         CALL DISSOLVED_OXYGEN(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dT,i,j,k)   ! Dissolved Oxygen
      enddo
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
 do j = 1,jm
     do i = 1,im
       if(fm(i,j).eq.1) then
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                !  Zooplankton kinetics
         CALL DIATOMS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dT,i,j,k) ! Diatom kinetics
         CALL GREENS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),PAR(i,j,k),Vol(i,j,k),dT,i,j,k) ! Greens kinetics
         CALL CARBON(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Carbon (detritus) kinetics
         CALL PHOSPH(f(i,j,k,:),DTM(i,j,k,:),i,j,k)                      !  Phosphorous kinetics
         CALL NITROG(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)         ! Nitrogen kinetics
         CALL SILICA(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)             !  Silica kinetics
         CALL DISSOLVED_OXYGEN(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),Vol(i,j,k),dT,i,j,k)   !  Dissolved Oxygen
      enddo
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif


if(Which_Fluxes(iInRemin).eq.1) then

if(DoDroop.eq.1) then
!Do settling fluxes for instant remineralization
 do j = 1,jm
     do i = 1,im
       if(fm(i,j).eq.1.and.wsm(i,j).eq.0.) then !If on shelf
         area = Vol(i,j,nz)/dz(i,j,nz)
         CALL EXCHANGE_droop(f(i,j,nz,:),area,Vol(i,j,nz),dT,i,j,SETRATE(:))      ! Calculate IR fluxes and settling rates
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
else
!Do settling fluxes for instant remineralization
 do j = 1,jm
     do i = 1,im
       if(fm(i,j).eq.1.and.wsm(i,j).eq.0.) then !If on shelf
         area = Vol(i,j,nz)/dz(i,j,nz)
         CALL EXCHANGE(f(i,j,nz,:),area,Vol(i,j,nz),dT,i,j,SETRATE(:))      !  Calculate IR fluxes and settling rates
         !write(6,*) "area eutro",area
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

endif


 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
      do k = 1, nz
         f(i,j,k,:) = max(f(i,j,k,:) + DTM(i,j,k,:) * dT,0.)
      enddo
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop


if(Which_Fluxes(iInRemin).eq.1) then
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1.and.wsm(i,j).eq.0.) then
         f(i,j,nz,:) = max(f(i,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dT,0.)
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif

if(Which_Fluxes(iInRemin).eq.2) then
 do j = 1,jm
     do i = 1,im
       if(fm(i,j).eq.1.and.wsm(i,j).eq.0.) then
         area = Vol(i,j,nz)/dz(i,j,nz)
         SETRATE(:) = f(i,j,nz,:)*area*(-ws(:))
         f(i,j,nz,:) = max(f(i,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dT,0.)
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif


!write(6,*) TSOD(1,1)*dT,SED_NO3_RATE(1,1)*dT,SED_NH3_RATE(1,1)*dT,SED_CARBON_RATE(1,1)*dT/Vol(1,1,1)
!f(:,:,:,JLOC) = 2.e-2
!
!-----------------------------------------------------------------------------


RETURN

END SUBROUTINE EUTRO
