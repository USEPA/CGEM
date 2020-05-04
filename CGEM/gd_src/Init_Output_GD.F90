       Subroutine Init_Output_GD(BASE_NETCDF_OUTPUT_FILE_NAME,myid,numprocs)

       USE Model_dim
       USE INPUT_VARS, ONLY : nstep,dT_out, IYRS, IMONS,&
     & IDAYS, IHRS, IMINS, ISECS, IYRE, IMONE, IDAYE, IHRE,&
     & IMINE, ISECE
       USE Grid
       USE State_Vars
       USE OUTPUT_NETCDF_GD
       USE states
       USE OUTPUT

       IMPLICIT NONE

       character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME
       character(256) :: NETCDF_OUTPUT_FILE_NAME
       integer ::  tinit=0, i,j,k,nz,myi,myid,numprocs,mpierr
       real :: dumf(myim,jm,km,nf)

       ! Change True/False parameters for netCDF Write Variables
       !L3 add Which_Output to GD InputFile
       !if(Which_Output.eq.2) call OUTPUT_ALL_FALSE() 

       WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), 0, '.nc'

       if(myid.eq.0) then
          CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                            im, jm, km, nstep, nf, EXTRA_VARIABLES, &
                            iYr0, &
                            IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                            IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                            DT_OUT, &
                            LON, LAT, d, FM, &
                            DZ, AREA )

          CALL CLOSE_FILE()
      endif
      CALL MPI_BARRIER( MPI_COMM_WORLD,mpierr) ! Wait until file is created.

      ! Opens the output file for writing:
       CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME), nf, EXTRA_VARIABLES, 0 )
       tinit=0

        dumf = f(1:myim,:,:,:)
        do j=1,jm
        myi = 1
        do i=myi_start,myi_end
          nz=nza(i,j)
          do k=1,nz
            dumf(myi,j,k,JTR) = f(myi,j,k,JTR) * Vol(i,j,k)
          enddo
          myi = myi + 1
         enddo
        enddo

        CALL WRITE_DATA( myi_start, myim, 1,jm, 1, km, nf,0, dumf)
        CALL MPI_BARRIER( MPI_COMM_WORLD,mpierr ) ! Wait until file is updated.

       return
       End Subroutine Init_Output_GD
