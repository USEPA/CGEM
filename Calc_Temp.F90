      subroutine Calc_Temp(START_SECONDS,TC_8,T)

      USE Model_dim

      IMPLICIT NONE

      integer*8, intent (in) :: START_SECONDS  !Integer seconds since 2002
      integer*8, intent (in) :: TC_8  !Integer seconds, simulation time
      real, intent (out) :: T(im,jm,nsl) !Temperature in C
      real,save :: Tmax,Tmin,MaxDay
      real*8 :: Td  !Days from 0 to 365.
      integer, save :: init = 1
!      T = 23.333 !Our room temperature, 74F

! Convert T (in decimals days since 2002?) to from 0 to 365.
      Td = TC_8 - START_SECONDS
      !write(6,*) "Simulation time in seconds",Td
      Td = Td/86400.d0
      !write(6,*) "Simulation time in days",Td
      Td = mod(Td,365.)
      !write(6,*) "Simulation time within context of year",Td

      if(init.eq.1) then  !Only read in data at the first timestep
      open (19,file='./data/T.dat',status='old')
      read (19,*)  !Assumes header file
      read (19,*) Tmax
      read (19,*) Tmin
      read (19,*) MaxDay 
      close(19)
      init=0
      if(Tmax-Tmin.lt.0) then
          write(6,*) "Tmax < Tmin, exiting"
          stop
      endif
      if(MaxDay.lt.0.or.MaxDay.gt.365.) then
          write(6,*) "MaxDay not within calendar year, exiting"
          stop
      endif

      endif

! Regression equation found by excel from data from a Beach website:
! http://beachhunter.net/thingstoknow/gulfwatertemp/index.htm
! First equation is in Celsius and Td is the temperature in days starting 
! January 1st = 1.
      T = Tmin + (Tmax-Tmin)*(COS(((Td-MaxDay))*3.1415/365.)*(COS(((Td-MaxDay))*3.1415/365.)))
      

! Here is from 4th order fit in Excel:
!      T = 1.7065959e-8*Td**4 - 1.2879208e-5*Td**3 + 2.5643923e-3*Td*Td &
!     &    - 0.05151689*Td+13.578683
!       write(6,*) Td,T

      return 
      end subroutine Calc_Temp 
