!**********************************************
! PURPOSE: routines for interpolating arrays
!
! Assumptions:
!
! Created by Cody Simmons/EMVL
!**********************************************


MODULE interp_utils

USE model_dim

IMPLICIT NONE


PUBLIC interp

PRIVATE interp3d, interp2d

  INTERFACE interp
    MODULE PROCEDURE interp3d, interp2d, interp1d
  END INTERFACE

CONTAINS

!----------------------------------------------------------------------


  ! Linearly interpolates 3d variable
  SUBROUTINE  interp3d(var1, var2, t1, t2, t_current, var, full)
    IMPLICIT NONE

    real :: fac
    integer(kind=8), INTENT(IN) :: t_current, t1, t2
    real, dimension(:,:,:), INTENT(IN)  :: var1, var2
    real, dimension(:,:,:), INTENT(OUT) :: var
    integer :: i
    logical, INTENT(IN) :: full

    
    ! linear interpolation
    fac = real(t_current - t1)
    fac = real(fac,4) / real((t2-t1), 4)

!    if (full) then
      var = var1 + (var2-var1)*fac
!    else 
!      do i = myi_start, myi_end
!        var(i,:,:)  = var1(i,:,:) + (var2(i,:,:)-var1(i,:,:))*fac
!      enddo
!    endif

  END SUBROUTINE interp3d


  ! Linearly interpolates 2d variable
  SUBROUTINE  interp2d(var1, var2, t1, t2, t_current, var, full)
    IMPLICIT NONE

    real :: fac
    integer(kind=8), INTENT(IN) :: t_current, t1, t2
    real, dimension(:,:), INTENT(IN)  :: var1, var2
    real, dimension(:,:), INTENT(OUT) :: var
    integer :: i
    logical, INTENT(IN) :: full


    ! linear interpolation
    fac = real(t_current - t1)
    fac = real(fac,4) / real((t2-t1), 4)

!    if (full) then
      var = var1 + (var2-var1)*fac
!    else
!      do i = myi_start, myi_end
!       var(i,:) = var1(i,:) + (var2(i,:)-var1(i,:))*fac
!      enddo
!    endif


  END SUBROUTINE interp2d

  ! Linearly interpolates 1d variable
  SUBROUTINE  interp1d(var1, var2, t1, t2, t_current, var, full)
    IMPLICIT NONE

    real :: fac
    integer(kind=8), INTENT(IN) :: t_current, t1, t2
    real, dimension(:), INTENT(IN)  :: var1, var2
    real, dimension(:), INTENT(OUT) :: var
    integer :: i
    logical, INTENT(IN) :: full


    ! linear interpolation
    fac = real(t_current - t1)
    fac = real(fac,4) / real((t2-t1), 4)

!    if (full) then
      var = var1 + (var2-var1)*fac
!    else
!      do i = myi_start, myi_end
!       var(i) = var1(i) + (var2(i)-var1(i))*fac
!      enddo
!    endif


  END SUBROUTINE interp1d

END MODULE interp_utils
