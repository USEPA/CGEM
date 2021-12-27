Subroutine Ave_istep_offset()

USE Model_dim
USE INPUT_VARS

IMPLICIT NONE

integer :: iSec_diff   ! Difference between starting time and time to start averaging
integer :: iSec_wait   ! How many seconds to wait before averaging
!  dT == timestep, decimal secs

! We want to start counting at 0600 UTC (Midnight in the GOM).
! Need to find starting time offset in seconds, then convert to timesteps.

! Start time of simulation is iHrS:iMinS:iSecS
      iSec_diff = iHrS * 3600 + iMinS * 60 + iSecS - 6 * 3600

      if(iSec_diff.le.0) then
        iSec_wait = ABS(iSec_diff)
      else
        iSec_wait = 86400 - iSec_diff
      endif

      istep_wait = iSec_wait/dT     ! Timesteps to wait
      print_ave  = 86400/dT         ! Timesteps to print

END Subroutine Ave_istep_offset 
