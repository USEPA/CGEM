SUBROUTINE EUTRO(f,T,S,Rad,fm,wsm,d,dz,Vol,dT)
!------------------------------------------------------------------------------
!

USE Model_dim
USE STATES
USE INPUT_VARS_GD, ONLY : Which_Fluxes,Read_Solar
USE EUT
USE Which_Flux
USE InRemin
USE FLAGS, ONLY : DoDroop

IMPLICIT NONE

REAL, INTENT(INOUT) :: f(im,jm,nsl,nf)
REAL, INTENT(IN)    :: T(im,jm,nsl),S(im,jm,nsl),Rad(im,jm)
INTEGER, INTENT(IN) :: fm(im,jm),dT
REAL, INTENT(IN) :: d(im,jm,nsl),dz(im,jm,nsl),Vol(im,jm,nsl),wsm(im,jm)
REAL :: DTM(im,jm,nsl,nf),SAL_TERM,CHL_TERM,POC_TERM,IOPpar(im,jm,nsl)
REAL :: IATTOP, IATBOT(im,jm,nsl),OPTDEPTH,Rad_Watts(im,jm)
REAL :: SETRATE(nf),area
INTEGER :: i,j,k


!
!------------------------------------------------------------------------------
!

IOPpar(1,1,1) = Rad(1,1)

if(Read_Solar.ne.2) then

Rad_Watts = Rad/3.021948e14

!GoMDOM LIGHT MODEL, No Wind Speed
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
      do k = 1, nz
         SAL_TERM = 1.084E-06 * (S(i,j,k)**4)

          IF ((f(i,j,k,JDIA) + f(i,j,k,JGRE)) < 1.0E-07) THEN
               CHL_TERM = 0.0
          ELSE
               CHL_TERM = 0.2085 * LOG( (f(i,j,k,JDIA) * 1.0E6 / CCHLD) + &
                        & (f(i,j,k,JGRE) * 1.0E6 / CCHLG) )
          ENDIF

          POC_TERM = 0.7640 * SQRT( (f(i,j,k,JLOC) * 1.0E3) +  &
                   & (f(i,j,k,JROC) * 1.0E3) + (f(i,j,k,JZOO) * 1.0E3) )

          KESS(i,j,k) = ( ( -0.10 * (-0.5606 - SAL_TERM + CHL_TERM + POC_TERM) ) &
                  &  + 1 ) ** (1.0/(-0.10))
      enddo


      DO k = 1,1
         IATTOP    =  Rad_Watts(i,j)
         OPTDEPTH  =  KESS(i,j,k) * dz(i,j,k)
         IATBOT(i,j,k) =  IATTOP  * EXP(-OPTDEPTH)
         IOPpar(i,j,k)   =  (IATTOP - IATBOT(i,j,k)) / OPTDEPTH
      END DO
      DO k = 2,nz
         IATTOP    =  IATBOT(i,j,k-1)
         OPTDEPTH  =  KESS(i,j,k) * dz(i,j,k) 
         IATBOT(i,j,k) =  IATTOP * EXP(-OPTDEPTH)
         IOPpar(i,j,k) =  (IATTOP - IATBOT(i,j,k)) / OPTDEPTH
      END DO
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop

endif


if(DoDroop.eq.1) then
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
      do k = 1, nz
         DTM(i,j,k,:) = 0.
         CALL ZOO(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),i,j,k)                ! Zooplankton kinetics
         CALL DIATOMS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),IOPpar(i,j,k),Vol(i,j,k),dT,i,j,k)            ! Diatom kinetics
         CALL GREENS_droop(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),IOPpar(i,j,k),Vol(i,j,k),dT,i,j,k)             ! Greens kinetics
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
         CALL DIATOMS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),IOPpar(i,j,k),Vol(i,j,k),dT,i,j,k) ! Diatom kinetics
         CALL GREENS(f(i,j,k,:),DTM(i,j,k,:),T(i,j,k),IOPpar(i,j,k),Vol(i,j,k),dT,i,j,k) ! Greens kinetics
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
         TSOD(i,j) = TSOD(i,j)/Vol(i,j,nz)
         SED_NO3_RATE(i,j) = SED_NO3_RATE(i,j)/Vol(i,j,nz)
         SED_NH3_RATE(i,j) = SED_NH3_RATE(i,j)/Vol(i,j,nz)
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif
endif

       !write(6,*) DTM(1,1,1,JDOP)*dt,DTM(1,1,1,JLOP)*dt

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
         !write(6,*) area,f(i,j,nz,JDIA),SETRATE(JDIA),dT,(SETRATE(JDIA)/Vol(i,j,nz)) * dT
!         f(i,j,nz,:) = max(f(i,j,nz,:)  - (SETRATE(:)/Vol(i,j,nz)) * dT,0.)
          f(i,j,nz,:) = max( (f(i,j,nz,:)*Vol(i,j,nz) - SETRATE(:)*dT)/Vol(i,j,nz),0. )
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop
endif




!
!-----------------------------------------------------------------------------


RETURN

END SUBROUTINE EUTRO
