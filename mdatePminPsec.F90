!#define DEBUG

! file: mdatePminPsec.F90
! -----------------------------------------------------------------
   Subroutine  mdatePminPsec (iYr,iMon,iDay,iHr,iMin,iSec, YDay_8, II)
   
!------------------------------------------------------------------------
!   Written  by :: D.S.Ko  CMPO/MIT
!   Modified by :: Barry Herchenroder/EMVL, July 2010
!                                           Jan & June 2011
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                                           May 2013--extended year range
!                                           to 2002 thru 2013
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   Generalization of sub. mdate to include minutes and seconds, 
!   i.e. date-times are input or output in the form yyyy/mm/dd/hh/mn/sec
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!------------------------------------------------------------------
! OBSOLETE!   where: iYr .= 2003 or later, up to and including 2007
!------------------------------------------------------------------
!   where: iYr .= 2002 or later, up to and including 2013
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          iMon = 1,2,...,12
!          iDay = 1,..., (number of days in month iMon of Year iYr, after
!                        accounting for whether year iYr is a Leap year. 
!          iHr  = 0, 1, 2,...,23---iHr = 0 is 12:00 Midnight at the 
!                          beginning of day iDay
!          iMin = minutes in the hour IHr, with iMin in the range 
!                          0 <= iMin <= 59 . Note that mn is sometimes
!                          used in place of iMin.
!          iSec = seconds in the minute iMin with iSec in the range
!                          0 <= iSec <= 59
!
! ------------------------------------------------------------------
! Convert calendar date, yyyy/mm/dd/hh/mn/sec, to model yearday, YDay_8 (II= 1)
! Convert model yearday, YDay_8, to calendar date, yyyy/mm/dd/hh/mn/sec (II=-1)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! YDay_8 (KIND=8)is in decimal days from beginning of Jan 1 2002
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-------------------------------------------------------------------        

!---------------------------------------------------
! Variables coming through the subroutine interface:
!---------------------------------------------------

   INTEGER, INTENT(IN)    :: II   !  Should not be zero

   INTEGER, INTENT(INOUT) :: iYr  ! IN  when II=1; OUT when II=-1
   INTEGER, INTENT(INOUT) :: iMon ! IN  when II=1; OUT when II=-1
   INTEGER, INTENT(INOUT) :: iDay ! IN  when II=1; OUT when II=-1
   INTEGER, INTENT(INOUT) :: iHr  ! IN  when II=1; OUT when II=-1
   INTEGER, INTENT(INOUT) :: iMin ! IN  when II=1; OUT when II=-1 
   INTEGER, INTENT(INOUT) :: iSec ! IN  when II=1; OUT when II=-1 
   
   REAL(KIND=8), INTENT(INOUT) :: YDay_8 ! OUT when II=1; IN  when II=-1

!-------------------------------------------------------   
! Declare and in some cases set the remaining variables
!------------------------------------------------------- 

!---------------
! PARAMETERS
!----------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-------------------------------------------------------------------------
! Obsolete--
!  INTEGER, PARAMETER  :: iYr0 = 2003 
!       iYr0 is the Starting Date: 2003/01/01 00Z --> model yearday = 1.00
!                                                 --> Yday_8 = 0.0_8
!-------------------------------------------------------------------------

   INTEGER, PARAMETER  :: iYr0 = 2002 
!       iYr0 is the Starting Date: 2002/01/01 00Z --> model yearday = 1.00 
!                                                 --> Yday_8 = 0.0_8
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   REAL(KIND=8), PARAMETER  :: NumMinPday = 24.0_8 * 60.0_8 
!       Number of minutes per day 

   REAL(KIND=8), PARAMETER  :: NumSecPday = NumMinPday * 60.0_8 
!       Number of minutes per day 

!----------------
! Integer scalars
!----------------

   INTEGER :: i   
   INTEGER :: iYrTest   
   INTEGER :: iMonTest
   INTEGER :: iDayTest   
   INTEGER :: iHrTest   
   INTEGER :: iMinTest   
   INTEGER :: iSecTest      
   INTEGER :: iTotTest 
   
   INTEGER(KIND=8) :: iHr_8
   INTEGER(KIND=8) :: iMin_8     
   INTEGER(KIND=8) :: iSec_8        

   INTEGER :: J
   INTEGER :: JD   
   INTEGER          :: JDay 
   INTEGER (KIND=8) :: JDay_8
   INTEGER :: JM 
   INTEGER :: JY
   
   INTEGER :: LOC

!----------------
! REAL scalars
!----------------  

   REAL(KIND=8) :: Hr_8
   REAL(KIND=8) :: HrP_8 
     
   REAL(KIND=8) :: Min_8 
   REAL(KIND=8) :: MinP_8 
        
   REAL(KIND=8) :: Sec_8 
   REAL(KIND=8) :: SecP_8      
!----------------
! Integer arrays
!---------------- 
  
   integer  JM0(13,2)
!---------------------------------------------------------------------   
! The first row below in JM0 is for a non-leap year and the second row  
! is for a leap year. Note that JM0(
!---------------------------------------------------------------------   
   data  JM0/0,31,59,90,120,151,181,212,243,273,304,334,365,           &
     &             0,31,60,91,121,152,182,213,244,274,305,335,366/
   save  JM0
   
   integer iDayMax(12,2)
   data iDayMax/31,28,31,30,31,30,31,31,30,31,30,31,                      &
     &          31,29,31,30,31,30,31,31,30,31,30,31/     
   Save iDayMax   
   !------------------------------------------------------------------------   

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! End of Declarations: Begin main code

#ifdef DEBUG
   WRITE(6, "()")
   WRITE(6, "('In subroutine mdatePminPsec')") 
   WRITE(6, "('just after declarations,')")    
IF(II > 0) THEN 
   WRITE(6, "('Since II > 0, just below  are values for the')")
   WRITE(6, "('following input parameters:')")         
   WRITE(6, "('iYr   = ', I4    )")  iYr
   WRITE(6, "('iMon  = ', I2    )")  iMon
   WRITE(6, "('iDay  = ', I2    )")  iDay  
   WRITE(6, "('iHr   = ', I2    )")  iHr    
   WRITE(6, "('iMin  = ', I2    )")  iMin
   WRITE(6, "('iSec  = ', I2    )")  iSec 
   WRITE(6, "('II    = ', I3    )")  II     
   WRITE(6, "()")     
ELSEIF(II < 0) THEN  
   WRITE(6, "('Since II < 0, just below are the values for the')")
   WRITE(6, "('following input parameters:')")             
   WRITE(6, "('YDay_8= ', F35.15)")  YDay_8  
   WRITE(6, "('II    = ', I3    )")  II    
   WRITE(6, "()")  
ELSE
 ! This is II = 0 situation which will be handled just below this 
 ! block if.
ENDIF              
#endif
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

! Check and make sure that II /= 0. If II==0, write error message
! and abort run

   LOC = 1

   IF( II == 0) THEN
       WRITE(6, "(' ')")
       WRITE(6, "('In sub. mdatePminPsec at LOC=1, a problem was  ')") 
       WRITE(6, "('found with the input parameter II. II was found')") 
       WRITE(6, "('to be zero when it should have been != zero.   ')")   
       WRITE(6, "('Abort Run. ')")
       WRITE(6, "(' ')")    
          
       STOP
   ENDIF

   if (II.gt.0) then 
   
! Check and make sure that:
!          iYr  >= iYr0, 
!     1 <= iMon <= 12 ,
!     1 <= iDay <= iDayMax(iMon)
!     0 <= iHr  >= 23
!     0 <= iMin <= 59
!     0 <= iSec <= 59  
!
! Where
!
!     iDayMax(iMon=9,or 4,or 6,or 11)                   = 30
!     iDayMax(iMon=1,or 2,or 5,or  7, or 8,or 10,or 12) = 31
!     iDayMax(iMon=2 and iYr is not a leap year)        = 28
!     iDayMax(iMon=2 and iYr is     a leap year)        = 29

     iYrTest  = 1
     iMonTest = 1
     iDayTest = 1
     iHrTest  = 1
     iMinTest = 1
     iSecTest = 1
     
     J        = JY(iYr) ! Note that JY is a function 
     
     IF(iYr  < iYr0                         ) iYrTest  = 0
     IF(iMon < 1 .OR. iMon > 12             ) iMonTest = 0
     IF(iDay < 1 .OR. iDay > iDayMax(iMon,J)) iDayTest = 0
     IF(iHr  < 0 .OR. iHr  > 23             ) iHrTest  = 0
     IF(iMin < 0 .OR. iMin > 59             ) iMinTest = 0
     IF(iSec < 0 .OR. iSec > 59             ) iSecTest = 0
     
     iTotTest = iYrTest*iMonTest*iDayTest*iHrTest*iMinTest*iSecTest
     
     IF(iTotTest == 0) THEN
        ! If we get here, then one or more of the parameters iYr
        ! iMon,...,iSec is out of range. Print an error message and
        ! abort run.
        
        WRITE(6, "(' ')")
        WRITE(6, "('In sub. mdatePminPsec, one or more of the input')")
        WRITE(6, "('parameters iYr,iMon,iDay,iHr,iMin,or iSec were ')")
        WRITE(6, "('found to be out of range. The values of these  ')")
        WRITE(6, "('parameters which were read in are:             ')")
        WRITE(6, "(' ')")
        WRITE(6, "('iYr  = ', I10)") iYr
        WRITE(6, "('iMon = ', I10)") iMon
        WRITE(6, "('iDay = ', I10)") iDay
        WRITE(6, "('iHr  = ', I10)") iHr
        WRITE(6, "('iMin = ', I10)") iMin
        WRITE(6, "('iSec = ', I10)") iSec
        WRITE(6, "(' ')")        
        WRITE(6, "('Abort Run ')") 
        
        STOP          
     ENDIF
     
!--------------------------------------------------------------------------   
! Convert calendar date, yyyy/mm/dd/hh/mn/sec, to model yearday,YDay_8 (II= 1)
!-------------------------------------------------------------------------- 
   
!    J = JY(iYr)                 ! Already calc.a few lines above

!    JDay = JM0(iMon,J) + iDay   ! Should this be iDay-1 so that JDAY here
                                 ! is the number of days in the last year
                                 ! of the range of years iYr0 inclusive to
                                 ! iYr inclusive
				 ! up to the beginning iDay in month iMon
				 ! of year iYr?
				 !    YES
				 
     JDay = JM0(iMon,J) + iDay - 1  				 

!--------------------------------------------------
!    We only need the second branch of the block if
!    below since iYr will always be >= iYr0 if we
!    get here
!				 
!    if (iYr.lt.iYr0) then
!      ! We should never get to this branch
!      ! of the block if
!      do i = iYr0-1, iYr, -1
!         JDay = JDay - JM0(13,JY(i))
!      end do
!    else if (iYr.gt.iYr0) then
!      do i = iYr0, iYr-1
!        JDay = JDay + JM0(13,JY(i))
!      end do
!    end if
!-------------------------------------------------------
     IF (iYr.gt.iYr0) THEN
       DO i = iYr0, (iYr - 1)
         JDay = JDay + JM0(13,JY(i))
       ENDDO 
     ENDIF    
!---------------------------------------------------------------------     
! At this point, JDay is the number of days from the beginning of year
! IYr0 to the beginning of day iDay in month iMon of year iYr 
!--------------------------------------------------------------------- 
    
!     YDay = JDay + float(iHr)/24.
      YDay_8 = REAL(JDay,KIND=8) + REAL(iHr,KIND=8)/24.0_8              &
      &      + REAL(iMin,KIND=8)/NumMinPday                             &
             + REAL(iSec,KIND=8)/NumSecPday
           
   else if (II.lt.0) then
!--------------------------------------------------------------------------- 
! Convert model yearday, YDay_8, to calendar date, yyyy/mm/dd/hh/mn/sec (II=-1) 
!---------------------------------------------------------------------------- 

!------------------------------
! Calculate iHr, iMin, and iSec
!------------------------------

! First make sure that YDay_8 is >= 0. If it is not, print error message
! and abort run.

     IF(YDay_8 < 0.0_8) THEN
       WRITE(6, "(' ')") 
       WRITE(6, "('In sub. mdatePminPsec a problem was found with ')") 
       WRITE(6, "('the value of YDay_8--it was found to be < 0 when ')") 
       WRITE(6, "('it should have been >= 0.0. The value was found to ')")
       WRITE(6, "('be YDay_8 = ',F16.8 )") YDay_8   
       WRITE(6, "('Abort Run. ')") 
       WRITE(6, "(' ')")   
       STOP                                           
     ENDIF
     
     JDay_8 = INT(YDay_8,KIND=8)  
                                 ! Number of days from beginning of year 
				 ! iYr0 to beginning of day JDAY, i.e. to 
				 ! end of day JDay-1
				 
     JDay   = INT(JDay_8,KIND=4)

!--------------------------------------------------------------------------     
!    if (float(JDay).ne.YDay.and.YDay.lt.0.) JDay = JDay-1  ! We don't need
!							    ! this
!    iHr = nint(24.*YDay - 24.*JDay)
!--------------------------------------------------------------------------  
  
     Hr_8   = 24.0_8 * (YDay_8 - REAL(JDay_8,KIND=8)) 
                                     ! Decimal number of hrs past midnight
                                     !    of the beginning of day JDay,
				     !    0 <= Hr8 <= 23
							 
   ! iHr_8  = INT(Hr_8,KIND=8)      ! Number of whole hrs past midnight of
   ! iHr    = INT(Hr_8,KIND=4)      !    the beginning of day JDay
     
     iHr_8  = FLOOR(Hr_8,KIND=8)
     iHr    = FLOOR(Hr_8,KIND=4)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#ifdef DEBUG     
     WRITE(6, "(' ')")
     WRITE(6, "('In sub. mdatePminPsec ')")
     WRITE(6, "(' ')")


     WRITE(6, "('YDay_8              = ', F16.8)") YDay_8 
     WRITE(6, "('JDay_8              = ', I10  )") JDay_8
     WRITE(6, "('JDay                = ', I10  )") JDay     
     WRITE(6, "('REAL(JDay_8,KIND=8) = ', F16.8)") REAL(JDay_8,KIND=8) 
     WRITE(6, "('Hr_8                = ', F16.8)") Hr_8
     WRITE(6, "('iHr_8               = ', I10  )") iHr_8
     WRITE(6, "('iHr                 = ', I10  )") iHr                   
     WRITE(6, "(' ')")   
#endif                    	
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%					
     HrP_8  = Hr_8 - REAL(iHr_8,KIND=8)  
                                     
					
     
     Min_8  = 60.0_8 * HrP_8         ! Decimal number of minutes past the
                                     !    beginning of hour iHr on day JDay 
						
     iMin_8 = FLOOR(Min_8,KIND=8)     ! Number of whole minutes past hour
     iMin   = FLOOR(Min_8,KIND=4)     !    iHr on day Jday	
							
     MinP_8  = Min_8 - REAL(iMin_8,KIND=8) 
                                     
					
     Sec_8  = 60.0_8 * MinP_8           ! Decimal number of seconds past the
                                        !    beginning of minute iMin
						
     iSec_8 = FLOOR(Sec_8,KIND=8)        ! Number of whole seconds past minute
     iSec   = FLOOR(Sec_8,KIND=4)        !    iMin in hr iHr on day JDay	
					
     SecP_8  = Sec_8 - REAL(iSec_8,KIND=8)  
                                        ! For diagnostic use	
!------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#ifdef DEBUG  
					
      WRITE(6, "('HrP_8               = ', F16.8)") HrP_8
      WRITE(6, "('Min_8               = ', F16.8)") Min_8
      WRITE(6, "('iMin_8              = ', I10  )") iMin_8          
      WRITE(6, "('iMin                = ', I10  )") iMin
      WRITE(6, "('MinP_8              = ', F16.8)") MinP_8            
      WRITE(6, "('Sec_8               = ', F16.8)") Sec_8 
      WRITE(6, "('iSec_8              = ', I10  )") iSec_8 
      WRITE(6, "('iSec                = ', I10  )") iSec
      WRITE(6, "('SecP_8              = ', F16.8)") SecP_8      
      WRITE(6, "(' ')")                              			
#endif                    	
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
                                     
!---------------------------------------------------------------------					
! Make sure that at this point, (0<= iSec < 60), (0<= Min < 60), 
! (0<= Hr < 24). If one of these inequalities is violated, there 
! is an error somewhere--print an error message and abort run.
!---------------------------------------------------------------------
          
    LOC  = 2      
    if((iSec < 0) .OR. (iSec >= 60) .OR.          &
    &  (iMin < 0) .OR. (iMin >= 60) .OR.          &
    &  (iHr  < 0) .OR. (iHr  >= 24)) THEN
       WRITE(6, "(' ')")
       WRITE(6, "('In sub. mdatePminPsec at LOC=2, a problem was  ')")
       WRITE(6, "('found with the calc. of iSec and/or iMin and/or  ')")
       WRITE(6, "('iHr when II < 0. '/)")
       WRITE(6, "(' ')")              
       WRITE(6, "('We have: iSec   = ', I14   )") iSec       
       WRITE(6, "('         iMin   = ', I14   )") iMin       
       WRITE(6, "('         iHr    = ', I14   )") iHr    
       WRITE(6, "('         YDay_8 = ', F16.9 )") YDay_8  ! kind=8 variable
       WRITE(6, "('         JDay   = ', I14   )") JDay 
       WRITE(6, "(' ')")                 
       WRITE(6, "('Abort run. ')")                                            
       WRITE(6, "(' ')")     
       
       STOP  
    endif

!--------------------------------
! Calculate iYr, iMon and iDay
!--------------------------------
     iYr = iYr0        ! Initialize iYr  

!----------------------------------------------------------------            
!    if (iHr.ge.24) then
!           ! This part of the block if should never be accessed
!           ! since iHr will be < 24.0 if we get here
!            iHr = iHr - 24
!            JD  = JDay + 1
!      else
!            JD  = JDay
!      end if
!-----------------------------------------------------------------  
    
      JD = JDay       ! Number of days from beginning of year iYr0 to
                      ! beginning of day JDAY, i.e. to end of day
		      ! JDay-1
      
      IF(JD == 0) THEN
      
         iYr = iYr0
         
      ELSE
      ! This is the JD > 0 branch since YDay and therefore JDay and 
      ! JD must be >= 0
      
!---------------------------------------------------------------------         
! The commented out loop below is not appropriate anymore since JD can 
! only be zero at this point and JD == 0 is taken separately 
!
!        do while (JD.lt.1)
!           iYr = iYr - 1
!           JD  = JD + JM0(13,JY(iYr))
!        end do
!----------------------------------------------------------------------
     
        J  = JY(iYr)             ! iYr = iYr0 here. .
	                         ! If iYr is     a leap year, J=JY = 2
				 ! If iYr is not a leap year, J=JY = 1
	
        JM = JM0(13,J)           ! # of days in year iYr0; Initial
                                 !    value of JM¬¬
                                 ! initial value of JD =JDay=int(YDay)
				 
         MYDO:DO WHILE (JD.gt.JM)     
           JD  = JD - JM
           iYr = iYr + 1
           J   = JY(iYr)
           JM  = JM0(13,J)
         END DO MYDO 
	    
      ENDIF ! end of "IF (JD == 0)" block if
     
!---------------------------------------------------------------------     
! At this point, the calculated value of iYr is the final value of iYr. 
! Also at this point, 
!        J = JY(iYr) is 1 if iYr is not a leap year and is 2 if iYr is a
!                    leap year
!
!       JM = number of whole Days in full year iYr 
!       JD = whole number of days in year iYr that are included in 
!               total number of model years/days, YDay
!        J = 1 if year iYr is not a leap year
!          = 2 if year iYr is     a leap year
!--------------------------------------------------------------------- 
    
     iMon = 12           ! Initialize iMon--

     J    = JY(iYr)              ! If iYr is     a leap year, J=JY = 2
				 ! If iYr is not a leap year, J=JY = 1
     
     ! WRITE(6, "(' ')")
     ! WRITE(6, "('In subroutine mdatePminPsec ')")  
     ! WRITE(6, "(' Got to line 454 ')") 
     ! WRITE(6, "(' ')")     
     ! WRITE(6, "('J = ',I2)") J        
     ! WRITE(6, "(' ')")                                
     
     JM   = JM0(iMon,J)  ! Initialize JM to number of days in year iYr
                         ! up thru the end of November
			 
  !  do while (JD.le.JM)   
     do while (JD  < JM)  ! We will handle JD = JM and JD > JM cases
                          ! separately   
       iMon = iMon - 1
       JM   = JM0(iMon,J)
     end do
!-----------------------------------------------------------------------     
! At this point, the calculated value of iMon is the final value of iMon 
!-----------------------------------------------------------------------    
  !  iDay = JD - JM      ! Final value of iDay? No, we need to distinguish
                         ! several cases as is done below.
     
     IF(JD == JM) THEN
        iDay = 1     ! since 12:00 midnight at the end of the month is 
	             ! taken to be the beginning of the next day
     ELSEIF( JD > JM) THEN
        iDay = 1+(JD - JM)  ! since JD = JM corresponds to beginning of
	                    ! the first day of the next month
     ELSE
     ! JD < JM. We should not get here. Print error message and
     ! abort run.
          WRITE(6,*) ' '  
          WRITE(6,*) 'In sub. mdatePminPsec, near the end, '  	  
	  WRITE(6, "('JD = ', I6, ' was found to be < ')") JD
	  WRITE(6, "('JM = ', I6, ' Abort run.')")	  
	  WRITE(6, "(' ')")	  
	  STOP     
     ENDIF
!L3 !KLUGE! Bandaid for problem that code does not get correct mm/dd/yy for Jan. 1st -
   if(iMon.eq.12.and.iDay.eq.32) then
      iMon=1
      iDay=1
      iYr = iYr + 1
   endif
!L3  !End KLUGE! ----------------------------------------------------------------------
     
   end if ! End of ¬"else if (II .lt.0)" part of "if (II .gt. 0) block if.
   return
   
   END SUBROUTINE mdatePminPsec
!---------------------------------------------------------   
