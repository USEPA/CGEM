      FUNCTION mdate_julian(iMonTC,iDayTC,leapyr ) RESULT(jday)

!--------------------------------------------------------------      
!     Written by Barry Herchenroder/EMVL, April/May 2010 
! 
!     Based in part on subroutine mdate written by D.S. Ko/NRL  
!--------------------------------------------------------------

!--------------------------------------------------------------
!      mdate_julian returns the Julian day associated with the
!      month iMonTC and day in month iDayTC of a particular year.
!      Whether the year is a leap year (leapyr == .TRUE.) or
!      not a leap year (leapyr == .FALSE.) is taken into account.
!------------------------------------------------------------------  

!---------------------------------------------------------
! Declare variables coming through the function interface:
!---------------------------------------------------------

    INTEGER, INTENT(IN)    :: iMonTC
    
    INTEGER, INTENT(IN)    :: iDayTC 
    
    LOGICAL, INTENT(IN)    :: leapyr 
    
!---------------------------------------------------------
! Declare other scalar variables
!--------------------------------------------------------- 

       INTEGER  ::  jday  
       
!---------------------------------------------------------
! Declare other array variables
!--------------------------------------------------------- 
! Note: tht is a zero at the end of JM0.

   integer  JM0(13,2) ! Cumulative days in year at beginning of each
                      ! month. JMO(:,1) is for non-leap year and
		      !        JMO(:,2) is for     leap year
   
   data  JM0/0,31,59,90,120,151,181,212,243,273,304,334,365,           &
     &             0,31,60,91,121,152,182,213,244,274,305,335,366/
   save  JM0
!------------------------------------------------------------------------  

!----------------
! Begin main code
!----------------

! Branch depending upon whether leapyr is true or false. Note that it
! is assumed that 12:00 midnight is associated with the beginning of
! the following day, not the end of the preceeding day. So if 


      IF(leapyr .eqv. .FALSE.) THEN
         ! The year is not a leap year
           jday = JM0(iMonTC,1) + iDayTC
      
      ELSE
         ! The year is a leap year
           jday = JM0(iMonTC,2) + iDayTC           
      ENDIF

      END FUNCTION mdate_julian
!-----------------------------------------------------------------------------
