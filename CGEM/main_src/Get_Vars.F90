       Subroutine Get_Vars(TC_8) 

       USE Model_dim
       USE INPUT_VARS, ONLY: START_SECONDS,&
     & Read_T, Read_Sal, Read_Solar, Read_Wind
       USE Grid
       USE DATE_TIME
       USE Hydro

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
       integer(kind=8) :: TC_in
       integer, save :: init=1
       integer i,j,k

        TC_in = TC_8

        if (Which_gridio.eq.0) then   !CGEM_0D

          if(Read_T.eq.0) then
            call Calc_Temp(START_SECONDS,TC_in,T)
          else
            call USER_Read_Temp(TC_in,T)
          endif
  
          if(Read_Sal.eq.0) then
            if(init.eq.1) call Calc_Sal(S)
          else
            call USER_Read_Sal(TC_in,S)
          endif
  
          if(Read_Solar.eq.0) then
            call getSolar( TC_in, lon, lat, Rad)
          else
            call USER_Read_Solar(TC_in,Rad)
          endif
  
          if(Read_Wind.eq.0) then
            Wind=5.
          else
            call USER_Read_Wind(TC_in,Wind)
          endif

        else if (Which_gridio.eq.1 .or. Which_gridio.eq.2) then  !EFDC.or.NCOM
          call interpVar(hydro_info(eSal), TC_8, startIndex(eSal), S)  
          call interpVar(hydro_info(eTemp), TC_8, startIndex(eTemp), T) 
          call interpVar(hydro_info(eUx), TC_8, startIndex(eUx), Ux)   
          call interpVar(hydro_info(eVx), TC_8, startIndex(eVx), Vx)   
          call interpVar(hydro_info(eWx), TC_8, startIndex(eWx), Wx)   
          call interpVar(hydro_info(eKh), TC_8, startIndex(eKh), Kh)   
          call interpVar(hydro_info(eE), TC_8, startIndex(eE), E)  
          call getSolar( TC_8, lon, lat, Rad)  !Calculate Solar Radiation for now
          Wind = 5                             !Set constant wind speed for now
        endif 

!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
!      call USER_get_masks()


        if(im*jm.eq.1) then  !For 1D column, turn off advection
         Ux=0.
         Vx=0.
         Wx=0.
        endif

        init=0


       return

       End Subroutine Get_Vars 
