! -----------------------------------------------------------
      function iFijn (X,N,XI)    RESULT(iiFijn)
!         Written  by ::  D.S.Ko
!         Modified by ::  Barry Herchenroder/EMVL, April 2010
!                                                  May   2011
! ------------------------------------------------------------
!     Purpose of this function: Find nearest grid point
! ------------------------------------------------------------

!------------------
!  Input and Output
!------------------ 
      INTEGER, INTENT(IN) :: N
      
      real   , INTENT(IN) :: X(*)
      real   , INTENT(IN) :: XI     
      
!----------------      
! Other variables      
!----------------
      INTEGER  :: i
      INTEGER  :: iiFijn      

!------------------------------------------------
 
      if (XI.le.X(1)) then
        iiFijn = 1
      else if (XI.ge.X(N)) then
        iiFijn = N
      else
        do i = 1, N-1
          if (XI.ge.X(i).and.XI.lt.X(i+1)) goto 8
        end do
8       iiFijn = i+nint((XI-X(i))/(X(i+1)-X(i)))
      end if

      return
      end FUNCTION iFijn
