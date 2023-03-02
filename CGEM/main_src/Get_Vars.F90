!*******************************************************************************
! 12/02/2022 Wilson Melendez: Removed argument .TRUE. from call to interp.
!                             Removed i, j, k, temp1, and temp2.
!*******************************************************************************

       Subroutine Get_Vars(TC_8,T_8,myid,numprocs,Which_code) 

       USE Model_dim
       USE INPUT_VARS, ONLY: START_SECONDS, Read_T, Read_Sal, Read_Solar, Read_Wind
       USE Grid
       USE DATE_TIME
       USE Hydro
       USE RiverLoad
       USE BoundaryConcentration
       USE interp_utils
       use mpi_interface
       use serial

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8,T_8 ! Current time in seconds since Model_dim::iYr0.
       integer(kind=8) :: TC_in, T_in
       integer, intent(in) :: myid,numprocs
       integer, save :: init=1
       integer :: mpierr

       logical :: broadcast_T, broadcast_TC  !logical for whether to broadcast T_8 or TC_8 hydro variables
       logical :: broadcast_river  !logical for whether to broadcast river variables
       logical :: broadcast_bc  !logical for whether to broadcast BC variables

       character(6), intent(in) :: Which_code

       TC_in = TC_8
       T_in = T_8


      !set logical broadcasts here

      broadcast_TC = .FALSE.
      broadcast_T = .FALSE.
      broadcast_river = .FALSE.
      broadcast_bc = .FALSE.

      if(myid.eq.0) then
      
       if (Which_gridio.eq.0) then
         broadcast_TC = .TRUE.
!         broadcast_T = .FALSE.
       else if (Which_gridio.eq.1 .or. Which_gridio.eq.2 .or. Which_gridio.eq.3) then
         if (TC_in .gt. hydro_tc2) then
           broadcast_TC = .TRUE.
         endif
         if (T_in .gt. hydro_t2) then
           broadcast_T = .TRUE.
         endif
         if (nRiv.gt.0 .AND. Which_gridio.eq.1 .AND. TC_in .gt. river_tc2) then
           broadcast_river = .TRUE.
         endif
         if (nBC.gt.0 .AND. Which_gridio.eq.1 .AND. TC_in .gt. bc_tc2) then
           broadcast_bc = .TRUE.
         endif
       endif


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

          ! retrieve bookend values only if necessary
          if (broadcast_TC) then
            call retrieveBookendVar(hydro_info(eTemp), TC_in, startIndex(eTemp), T1, T2, hydro_tc1, hydro_tc2)
            call retrieveBookendVar(hydro_info(eUx), TC_in, startIndex(eUx), Ux1, Ux2, hydro_tc1, hydro_tc2)
            call retrieveBookendVar(hydro_info(eVx), TC_in, startIndex(eVx), Vx1, Vx2, hydro_tc1, hydro_tc2)
            call retrieveBookendVar(hydro_info(eWx), TC_in, startIndex(eWx), Wx1, Wx2, hydro_tc1, hydro_tc2)
            call retrieveBookendVar(hydro_info(eKh), TC_in, startIndex(eKh), Kh1, Kh2, hydro_tc1, hydro_tc2)
            if (Which_gridio.ne.3) then
              call retrieveBookendVar(hydro_info(eSal), TC_in, startIndex(eSal), S1, S2, hydro_tc1, hydro_tc2)
              call getSolar( TC_8, lon, lat, Rad)  !Calculate Solar Radiation for now
              Wind = 5
            else 
              call retrieveBookendVar(hydro_info(eRad), TC_in, startIndex(eRad), Rad1, Rad2, hydro_tc1, hydro_tc2)
              call retrieveBookendVar(hydro_info(eWind), TC_in, startIndex(eWind), Wind1, Wind2, hydro_tc1, hydro_tc2)
              S = 0.3 !Rough estimate for Lake Michigan (varies from 0.05 to 0.6 ?)
            endif
          end if

          if (broadcast_T) then
            call retrieveBookendVar(hydro_info(eE), T_in, startIndex(eE), E1, E2, hydro_t1, hydro_t2)
          endif
            



! River vars
           if (nRiv .gt. 0 .AND. broadcast_river) then
               if (Which_code.eq."CGEM") then
                  call retrieveBookendRiverVar(riverload_info(eRiv1), TC_8, startRivIndex(eRiv1), Riv1A, Riv1B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv2), TC_8, startRivIndex(eRiv2), Riv2A, Riv2B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv3), TC_8, startRivIndex(eRiv3), Riv3A, Riv3B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv4), TC_8, startRivIndex(eRiv4), Riv4A, Riv4B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv5), TC_8, startRivIndex(eRiv5), Riv5A, Riv5B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv6), TC_8, startRivIndex(eRiv6), Riv6A, Riv6B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv7), TC_8, startRivIndex(eRiv7), Riv7A, Riv7B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv8), TC_8, startRivIndex(eRiv8), Riv8A, Riv8B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv9), TC_8, startRivIndex(eRiv9), Riv9A, Riv9B, river_tc1, river_tc2)
               else if (Which_code.eq."WQEM") then
                  call retrieveBookendRiverVar(riverload_info(eRiv1), TC_8, startRivIndex(eRiv1), Riv1A, Riv1B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv2), TC_8, startRivIndex(eRiv2), Riv2A, Riv2B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv3), TC_8, startRivIndex(eRiv3), Riv3A, Riv3B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv4), TC_8, startRivIndex(eRiv4), Riv4A, Riv4B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv5), TC_8, startRivIndex(eRiv5), Riv5A, Riv5B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv6), TC_8, startRivIndex(eRiv6), Riv6A, Riv6B, river_tc1, river_tc2)
                  call retrieveBookendRiverVar(riverload_info(eRiv7), TC_8, startRivIndex(eRiv7), Riv7A, Riv7B, river_tc1, river_tc2)
               else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
               endif
           endif


           if (nBC.gt.0 .AND. broadcast_bc) then
                if (Which_code.eq."CGEM") then
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC1), TC_8, startBCIndex(eBC1), BC1A, BC1B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC2), TC_8, startBCIndex(eBC2), BC2A, BC2B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC3), TC_8, startBCIndex(eBC3), BC3A, BC3B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC4), TC_8, startBCIndex(eBC4), BC4A, BC4B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC5), TC_8, startBCIndex(eBC5), BC5A, BC5B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC6), TC_8, startBCIndex(eBC6), BC6A, BC6B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC7), TC_8, startBCIndex(eBC7), BC7A, BC7B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC8), TC_8, startBCIndex(eBC8), BC8A, BC8B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC9), TC_8, startBCIndex(eBC9), BC9A, BC9B, bc_tc1, bc_tc2)
               else if (Which_code.eq."WQEM") then
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC1), TC_8, startBCIndex(eBC1), BC1A, BC1B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC2), TC_8, startBCIndex(eBC2), BC2A, BC2B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC3), TC_8, startBCIndex(eBC3), BC3A, BC3B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC4), TC_8, startBCIndex(eBC4), BC4A, BC4B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC5), TC_8, startBCIndex(eBC5), BC5A, BC5B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC6), TC_8, startBCIndex(eBC6), BC6A, BC6B, bc_tc1, bc_tc2)
                   call retrieveBookendBCVar(boundaryconcentration_info(eBC7), TC_8, startBCIndex(eBC7), BC7A, BC7B, bc_tc1, bc_tc2)
               else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
               endif
           endif

        endif 

!--------------------------------
! --- get land/water and shelf masks
!--------------------------------

        if(im*jm.eq.1) then  !For 1D column, turn off advection
         Ux=0.
         Vx=0.
         Wx=0.
        endif

        init=0
      endif

      if(numprocs.gt.1) then
        call MPI_BCAST(broadcast_tc,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(broadcast_t,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(broadcast_river,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpierr)
        call MPI_BCAST(broadcast_bc,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpierr)

        if (broadcast_TC) then
          if(numprocs.gt.1) then
            call MPI_BCAST(hydro_tc1,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
            call MPI_BCAST(hydro_tc2,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

            if (Which_gridio.eq.0)then
              call MPI_BCAST(S,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(T,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Wind,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Rad,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)       
            else 

              if (Which_gridio.eq.3) then
                call MPI_BCAST(S,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Wind1,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Wind2,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Rad1,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Rad2,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              else
                call MPI_BCAST(S1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(S2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Wind,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(Rad,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              endif
  
!              call MPI_BCAST(S1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(T1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Ux1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Vx1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Wx1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Kh1,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
!              call MPI_BCAST(S2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(T2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Ux2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Vx2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Wx2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
              call MPI_BCAST(Kh2,im*jm*nsl,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            endif
          endif 
        endif


        if (broadcast_T) then
          if(numprocs.gt.1) then
            call MPI_BCAST(hydro_t1,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
            call MPI_BCAST(hydro_t2,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

            call MPI_BCAST(E1,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            call MPI_BCAST(E2,im*jm,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
          endif
        endif


        if (broadcast_river) then
            call MPI_BCAST(river_tc1,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
            call MPI_BCAST(river_tc2,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
          
            if(Which_code.eq."CGEM") then
               call MPI_BCAST(Riv1A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv1B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv2A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv2B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv3A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv3B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv4A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv4B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv5A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv5B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv6A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv6B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv7A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv7B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv8A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv8B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv9A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv9B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            else if (Which_code.eq."WQEM") then
               call MPI_BCAST(Riv1A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv1B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv2A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv2B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv3A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv3B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv4A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv4B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv5A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv5B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv6A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv6B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv7A,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
               call MPI_BCAST(Riv7B,nRiv,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
            endif
        endif
        

        if (broadcast_bc) then
            call MPI_BCAST(bc_tc1,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
            call MPI_BCAST(bc_tc2,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

            if(Which_code.eq."CGEM") then
                call MPI_BCAST(BC1A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC2A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC3A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC4A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC5A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC6A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC7A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC8A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC9A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

                call MPI_BCAST(BC1B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC2B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC3B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC4B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC5B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC6B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC7B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC8B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC9B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            else if (Which_code.eq."WQEM") then
                call MPI_BCAST(BC1A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC2A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC3A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC4A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC5A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC6A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC7A,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

                call MPI_BCAST(BC1B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC2B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC3B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC4B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC5B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC6B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
                call MPI_BCAST(BC7B,nBC,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
            else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
            endif
        endif
      endif

      !interpolate
      call interp(T1, T2, hydro_tc1, hydro_tc2, TC_in, T)
      call interp(Ux1, Ux2, hydro_tc1, hydro_tc2, TC_in, Ux)
      call interp(Vx1, Vx2, hydro_tc1, hydro_tc2, TC_in, Vx)
      call interp(Wx1, Wx2, hydro_tc1, hydro_tc2, TC_in, Wx)
      call interp(Kh1, Kh2, hydro_tc1, hydro_tc2, TC_in, Kh)
      if (Which_gridio.ne.3)then
        call interp(S1, S2, hydro_tc1, hydro_tc2, TC_in, S)
      else
        call interp(Rad1, Rad2, hydro_tc1, hydro_tc2, TC_in, Rad)
        call interp(Wind1, Wind2, hydro_tc1, hydro_tc2, TC_in, Wind)
      endif

      if (Which_gridio.eq.1)then
        if (nRiv .gt. 0)then        
          if(Which_code.eq."CGEM") then    
              call interp(Riv1A, Riv1B, river_tc1, river_tc2, TC_in, Riv1)        
              call interp(Riv2A, Riv2B, river_tc1, river_tc2, TC_in, Riv2)
              call interp(Riv3A, Riv3B, river_tc1, river_tc2, TC_in, Riv3)
              call interp(Riv4A, Riv4B, river_tc1, river_tc2, TC_in, Riv4)
              call interp(Riv5A, Riv5B, river_tc1, river_tc2, TC_in, Riv5)
              call interp(Riv6A, Riv6B, river_tc1, river_tc2, TC_in, Riv6)
              call interp(Riv7A, Riv7B, river_tc1, river_tc2, TC_in, Riv7)
              call interp(Riv8A, Riv8B, river_tc1, river_tc2, TC_in, Riv8)
              call interp(Riv9A, Riv9B, river_tc1, river_tc2, TC_in, Riv9)
          else if (Which_code.eq."WQEM") then
              call interp(Riv1A, Riv1B, river_tc1, river_tc2, TC_in, Riv1)        
              call interp(Riv2A, Riv2B, river_tc1, river_tc2, TC_in, Riv2)
              call interp(Riv3A, Riv3B, river_tc1, river_tc2, TC_in, Riv3)
              call interp(Riv4A, Riv4B, river_tc1, river_tc2, TC_in, Riv4)
              call interp(Riv5A, Riv5B, river_tc1, river_tc2, TC_in, Riv5)
              call interp(Riv6A, Riv6B, river_tc1, river_tc2, TC_in, Riv6)
              call interp(Riv7A, Riv7B, river_tc1, river_tc2, TC_in, Riv7)
          else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
          endif
        endif

        if (nBC .gt. 0)then
          if(Which_code.eq."CGEM") then 
              call interp(BC1A, BC1B, bc_tc1, bc_tc2, TC_in, BC1)
              call interp(BC2A, BC2B, bc_tc1, bc_tc2, TC_in, BC2)
              call interp(BC3A, BC3B, bc_tc1, bc_tc2, TC_in, BC3)
              call interp(BC4A, BC4B, bc_tc1, bc_tc2, TC_in, BC4)
              call interp(BC5A, BC5B, bc_tc1, bc_tc2, TC_in, BC5)
              call interp(BC6A, BC6B, bc_tc1, bc_tc2, TC_in, BC6)
              call interp(BC7A, BC7B, bc_tc1, bc_tc2, TC_in, BC7)
              call interp(BC8A, BC8B, bc_tc1, bc_tc2, TC_in, BC8)
              call interp(BC9A, BC9B, bc_tc1, bc_tc2, TC_in, BC9)
          else if (Which_code.eq."WQEM") then
              call interp(BC1A, BC1B, bc_tc1, bc_tc2, TC_in, BC1)
              call interp(BC2A, BC2B, bc_tc1, bc_tc2, TC_in, BC2)
              call interp(BC3A, BC3B, bc_tc1, bc_tc2, TC_in, BC3)
              call interp(BC4A, BC4B, bc_tc1, bc_tc2, TC_in, BC4)
              call interp(BC5A, BC5B, bc_tc1, bc_tc2, TC_in, BC5)
              call interp(BC6A, BC6B, bc_tc1, bc_tc2, TC_in, BC6)
              call interp(BC7A, BC7B, bc_tc1, bc_tc2, TC_in, BC7)
          else
                  WRITE(6,*) "Model ", Which_code," not found in Get_Vars.F90"
                  WRITE(6,*) "Exiting."
                  STOP
          endif
        endif
      endif


#ifdef DEBUG
write(6,*) "Subroutine Get_Vars,myid=",myid
write(6,*) "S,T,Wind,Rad,Ux,Vx,Wx,Kh,E"
write(6,*) 
#endif

       return

       End Subroutine Get_Vars 
