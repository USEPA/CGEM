! ----------------------------------------------------------------
   function  JY(iYr)     RESULT(JJY)

!      Written  by ::    D.S.Ko
!      Modified by ::    Barry Herchenroder/EMVL, April 2010
!                                                 May 2011
!-----------------------------------------------------------------
!   Purpose of function: find leap year. If iYr is a leap year,
!                                            JY = 2
!                                        IF iYr is not a leap year,
!                                            JY = 1
! -----------------------------------------------------------------
     IMPLICIT NONE

!-----------------
! Input and Output
!-----------------

   INTEGER, INTENT(IN) :: iYr ! year (e.g. 2005)

   INTEGER             :: JJY

   if (mod(iYr,4).eq.0) then
       JJY = 2
       if (mod(iYr,100).eq.0) then
           JJY = 1
           if (mod(iYr,400).eq.0) then
               JJY = 2
                end if
            end if
   else
       JJY = 1
   end if

   return
   END FUNCTION JY
!-----------------------------------------------------------
