       Subroutine Get_Vars(TC_8,T_8,myid,numprocs) 

       USE Model_dim
       USE INPUT_VARS, ONLY: START_SECONDS,&
     & Read_T, Read_Sal, Read_Solar, Read_Wind
       USE Grid
       USE DATE_TIME
       USE Hydro
       USE RiverLoad
       USE BoundaryConcentration
       use mpi_interface

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8,T_8 ! Current time in seconds since Model_dim::iYr0.
       integer(kind=8) :: TC_in
       integer, intent(in) :: myid,numprocs
       integer, save :: init=1
       integer i,j,k,mpierr



      if(myid.eq.0) then

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

        else if (Which_gridio.eq.1 .or. Which_gridio.eq.2 .or. Which_gridio.eq.3) then  !EFDC.or.NCOM.or.POM
          if (Which_gridio.ne.3) then
            call interpVar(hydro_info(eSal), TC_8, startIndex(eSal), S)  
          else 
            S = 0.3  !Rough estimate for Lake Michigan  (varies from 0.05 to 0.6 ?)
          endif
          call interpVar(hydro_info(eTemp), TC_8, startIndex(eTemp), T) 
          call interpVar(hydro_info(eUx), TC_8, startIndex(eUx), Ux)   
          call interpVar(hydro_info(eVx), TC_8, startIndex(eVx), Vx)   
          call interpVar(hydro_info(eWx), TC_8, startIndex(eWx), Wx)   
          call interpVar(hydro_info(eKh), TC_8, startIndex(eKh), Kh)   
!          call interpVar(hydro_info(eE), TC_8, startIndex(eE), E)  
          call interpVar(hydro_info(eE), T_8, startIndex(eE), E)

! River vars
          if (nRiv > 0) then
              call interpRiverVar(riverload_info(eVar1), TC_8, startRivIndex(eVar1), Var1)
              call interpRiverVar(riverload_info(eVar2), TC_8, startRivIndex(eVar2), Var2)
              call interpRiverVar(riverload_info(eVar3), TC_8, startRivIndex(eVar3), Var3)
              call interpRiverVar(riverload_info(eVar4), TC_8, startRivIndex(eVar4), Var4)
              call interpRiverVar(riverload_info(eVar5), TC_8, startRivIndex(eVar5), Var5)
              call interpRiverVar(riverload_info(eVar6), TC_8, startRivIndex(eVar6), Var6)
              call interpRiverVar(riverload_info(eVar7), TC_8, startRivIndex(eVar7), Var7)
              call interpRiverVar(riverload_info(eVar8), TC_8, startRivIndex(eVar8), Var8)
              call interpRiverVar(riverload_info(eVar9), TC_8, startRivIndex(eVar9), Var9)
          endif

          ! Boundary concentration variables
          if (nBC > 0) then
              call interpBcVar(boundaryconcentration_info(eBCvar1), TC_8, startBcIndex(eBCvar1), BCvar1)
              call interpBcVar(boundaryconcentration_info(eBCvar2), TC_8, startBcIndex(eBCvar2), BCvar2)
              call interpBcVar(boundaryconcentration_info(eBCvar3), TC_8, startBcIndex(eBCvar3), BCvar3)
              call interpBcVar(boundaryconcentration_info(eBCvar4), TC_8, startBcIndex(eBCvar4), BCvar4)
              call interpBcVar(boundaryconcentration_info(eBCvar5), TC_8, startBcIndex(eBCvar5), BCvar5)
              call interpBcVar(boundaryconcentration_info(eBCvar6), TC_8, startBcIndex(eBCvar6), BCvar6)
              call interpBcVar(boundaryconcentration_info(eBCvar7), TC_8, startBcIndex(eBCvar7), BCvar7)
              call interpBcVar(boundaryconcentration_info(eBCvar8), TC_8, startBcIndex(eBCvar8), BCvar8)
              call interpBcVar(boundaryconcentration_info(eBCvar9), TC_8, startBcIndex(eBCvar9), BCvar9)
          endif

          if (Which_gridio.eq.3) then
            call interpVar(hydro_info(eRad), TC_8, startIndex(eRad), Rad)
            call interpVar(hydro_info(eWind), TC_8, startIndex(eWind), Wind)
          else
            call getSolar( TC_8, lon, lat, Rad)  !Calculate Solar Radiation for now
            Wind = 5                             !Set constant wind speed for now
          endif
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
      endif

      if(numprocs.gt.1) then
        call MPI_BCAST(S,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(T,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Wind,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Rad,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Ux,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Vx,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Wx,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(Kh,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(E,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
      endif 

#ifdef DEBUG
write(6,*) "Subroutine Get_Vars,myid=",myid
write(6,*) "S,T,Wind,Rad,Ux,Vx,Wx,Kh,E"
write(6,*) 
#endif

       return

       End Subroutine Get_Vars 
