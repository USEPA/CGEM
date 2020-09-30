!-------------------------------------------
!PI: John C. Lehrter, Ph.D.
!USEPA/NHEERL Gulf Ecology Division
!1 Sabine Island Drive
!Gulf Breeze, FL 32561-5299
!(850) 934-9255
!(850) 934-2401 -- fax
!lehrter.john@epa.gov      
!-------------------------------------------
!Generalized CGEM/GoMDOM code

!----------------------------------
      PROGRAM main 
!----------------------------------

!------------------------------------------------------------------------
!     Written  by :: Lisa L. Lowe/EMVL, lowe.lisa@epa.gov 
!                    D.S.Ko/NRL
!                    Wei Tang/EMVL
!                    Louis Olszyk/EMVL
!                    Cody Simmons/EMVL
! -----------------------------------------------------------------------

      USE Model_dim
      USE Grid
      USE Hydro
      USE RiverLoad
      USE BoundaryConcentration
      USE State_Vars
      USE INPUT_VARS
      USE serial
      use mpi

      IMPLICIT NONE


!---------------------
! Declare Variables:    
!---------------------
      integer, parameter :: exit_code = 0   ! Status 0 when run is successful 
      integer(kind=8) :: T_8,TC_8 ! Current time in seconds since Model_dim::iYr0.
      integer  :: istep, istep_out
      character(120) input_filename         !Input file
      character(120) init_filename         !Initial conditions file
      character(6) Which_code
      character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME
!------------------------------------------------ 
!---------------------
! MPI variables
!---------------------
      integer :: myid=0
      integer :: numprocs=1
      real*8  mpitime1,mpitime2,my_wtime   !timing
      integer mpierr

!------------------------------------------------
!Initialize MPI
      call MPI_INIT(mpierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myid, mpierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, mpierr)
      mpitime1 = MY_WTIME()
!      mpitime1 = MPI_Wtime()
      PRINT*,"mpitime1=",mpitime1, " with myid=",myid

! --- Command Line Arguments for file names ---
      if(myid.eq.0) then
        call Command_Line_Args(Which_code,input_filename,init_filename,BASE_NETCDF_OUTPUT_FILE_NAME)
      endif
      if(numprocs.gt.1) then 
       call MPI_BCAST(Which_code,6,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(input_filename,120,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(init_filename,120,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(BASE_NETCDF_OUTPUT_FILE_NAME,100,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
      endif

      call Set_Model_dim(myid,numprocs)
      call Set_Grid(myid,numprocs)
      call Allocate_Input_Vars(Which_code)
      call Allocate_Hydro
      if (nRiv > 0) call Allocate_RiverLoads()
      if (nBC  > 0) call Allocate_BoundaryConcentrations()

! Read_InputFile must define nstep, iout, dT, START_SECONDS
      call Read_InputFile(input_filename,Which_code,myid,numprocs) 
#ifdef DEBUG
      write(6,*) "----After Read_InputFile"
      write(6,*) "  start,dT",START_SECONDS, dT
#endif

      T_8 = START_SECONDS 
      TC_8 = START_SECONDS - (dT / 2) ! Subtract half dT to 'center' of timestep.

      if(myid.eq.0)then
        print*,"start T_8=",T_8
        print*,"start_TC_8=",TC_8
      endif

      if (Which_gridio .gt. 0.and.myid.eq.0) then
        call Init_Hydro_NetCDF()
        if (nRiv > 0) call Init_RiverLoad_NetCDF()
        if (nBC  > 0) call Init_BoundaryConcentration_NetCDF()
      endif

      if(numprocs .gt. 1) then
       call MPI_BCAST(riversIJ,nRiv*2,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(weights,nRiv*NSL,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
      endif
  
      call Get_Vars(TC_8,T_8,myid,numprocs) !Hydro for initial timestep 

! Initialize time an loop variables
      istep = 0
      istep_out = 0

      if (Which_gridio.eq.1) then
        call USER_update_EFDC_grid(TC_8,T_8,myid,numprocs)
      else if (Which_gridio.eq.2) then
        call USER_update_NCOM_grid(T_8,myid,numprocs)
      else if (Which_gridio.eq.3) then
        call USER_update_POM_grid(T_8,myid,numprocs)
      endif

!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      if(myid.eq.0) call USER_get_masks()
      if(numprocs.gt.1) then 
       call MPI_BCAST(nza,im*jm,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(wsm,im*jm,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
       call MPI_BCAST(fm,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
      endif

     
      call Set_Vars(Which_code,init_filename,myid,numprocs) !initialize 'f' array

      call Initialize_Output(Which_code,BASE_NETCDF_OUTPUT_FILE_NAME,myid,numprocs)     !Open file and write initial configuration

!-------------- START TIME LOOP -----------------------------------
      do istep = 1, nstep
       TC_8 = TC_8 + dT
       T_8 = T_8 + dT
#ifdef DEBUG
     write(6,*) "TC_8=", TC_8
#endif
      if(myid.eq.0)then
        write(6,*) "istep=",istep, " of ",nstep
      endif

      call Get_Vars(TC_8,T_8,myid,numprocs) !Hydro, Solar, Wind, Temp, Salinity, and riverloads

      if (Which_gridio.eq.1) then
        call USER_update_EFDC_grid(TC_8,T_8,myid,numprocs)
      elseif (Which_gridio.eq.2) then
        call USER_update_NCOM_grid(T_8,myid,numprocs)
      elseif (Which_gridio.eq.3) then
        call USER_update_POM_grid()
      endif


!!      print*,"Main T(49,48,1)=",T(49,48,1)," with myid:",myid
!!      print*,"Main T(205,38,1)=",T(205,38,1)," with myid:",myid
!!      print*,"Main S(47,94,1)=",S(47,94,1)," with myid:",myid
!!      print*,"Main S(222,108,1)=",S(222,108,1)," with myid:",myid
!!      print*,"Main Ux(47,94,1)=",Ux(47,94,1)," with myid:",myid
!!      print*,"Main Ux(222,108,1)=",Ux(222,108,1)," with myid:",myid
!!      print*,"Main Vx(47,94,1)=",Vx(47,94,1)," with myid:",myid
!!      print*,"Main Vx(222,108,1)=",Vx(222,108,1)," with myid:",myid
!!      print*,"Main Wx(47,94,1)=",Wx(47,94,1)," with myid:",myid
!!      print*,"Main Wx(222,108,1)=",Wx(222,108,1)," with myid:",myid
!!      print*,"Main Kh(47,94,1)=",Kh(47,94,1)," with myid:",myid
!!      print*,"Main Kh(222,108,1)=",Kh(222,108,1)," with myid:",myid
!!      print*,"Main Wind(47,94)=",Wind(47,94)," with myid:",myid
!!      print*,"Main Wind(222,108)=",Wind(222,108)," with myid:",myid
!!      print*,"Main Rad(47,94)=",Rad(47,94)," with myid:",myid
!!      print*,"Main Rad(222,108)=",Rad(222,108)," with myid:",myid

       call USER_update_masks()

       call WQ_Model(Which_code,TC_8,istep,istep_out,myid,numprocs)

       call Flux(Which_code,istep)

       call Transport(Which_code,myid,numprocs)

      ! -------------- BEGIN OUTPUT DATA
      ! --- dump output when istep is a multiple of iout
       if (  mod( istep, iout ) .eq. 0 ) then
        istep_out = istep_out + 1
        if(Which_gridio.ne.0.and.myid.eq.0) write(6,*) "output=",istep_out+1
        call Model_Output(Which_Code,istep_out,myid,numprocs)
       endif
      enddo

      Call Model_Finalize(Which_code,Which_gridio,myid,numprocs) ! Closes the NetCDF files and whatever else

      mpitime2 = MY_WTIME()
!      mpitime2 = MPI_Wtime()
      PRINT*,"mpitime2=",mpitime2, " with myid=",myid
      if(myid.eq.0) write(6,*) "Code took",mpitime2-mpitime1,"seconds"

      call MPI_FINALIZE(mpierr)

!----------------------------------------------------------------
! If we get here, there will be a normal exit from the program and
! exit code will be set to 0
!----------------------------------------------------------------
      if(myid.eq.0)then
        print*,"Code complete"
      endif
      call EXIT(exit_code)


!-----------------------------------      
      END PROGRAM main 
!-----------------------------------  


