       Subroutine Get_Vars(TC_8) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow,START_SECONDS,&
     & Read_T, Read_Sal, Read_Solar, Read_Wind
       USE Grid
       USE DATE_TIME
       USE Hydro

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
       integer, save :: init=1

        if(Read_T.eq.0) then
          call Calc_Temp(START_SECONDS,TC_8,T)
        else
         call USER_Read(TC_8,T,'t',init)
        endif

        if(Read_Sal.eq.0) then
          if(init.eq.1) call Calc_Sal(S)
        else
          call USER_Read(TC_8,S,'s',init)
        endif

        if(Read_Solar.eq.0) then
         call getSolar( TC_8, lon, lat, Rad)
        else
         call USER_Read(TC_8,Rad,'p',init)
        endif

        if(Read_Wind.eq.0) then
         Wind=5.
        else
         call USER_Read(TC_8,Wind,'w',init)
        endif

        Ux=0.
        Vx=0.
        Wx=0.
        Kh=0.
        E=0.
        init=0

       return

       End Subroutine Get_Vars 
