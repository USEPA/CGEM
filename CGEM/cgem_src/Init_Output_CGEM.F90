       Subroutine Init_Output_CGEM(BASE_NETCDF_OUTPUT_FILE_NAME,comt_filename,myid,numprocs)

       USE Model_dim
       USE INPUT_VARS, ONLY: nstep,dT_out,IYRS,IMONS,&
     & IDAYS, IHRS, IMINS, ISECS, IYRE, IMONE, IDAYE, IHRE,&
     & IMINE, ISECE,Which_Output, code_ID
       USE INPUT_VARS_CGEM, ONLY : Which_chlaC, MC
       USE Grid
       USE OUTPUT_NETCDF_CGEM
       USE Model_Compare
       USE State_Vars
       USE CGEM_vars

       IMPLICIT NONE

       character(100),intent(in) :: BASE_NETCDF_OUTPUT_FILE_NAME
       character(100),intent(in) :: comt_filename
       character(256) :: NETCDF_OUTPUT_FILE_NAME
       real :: dumf(myim,jm,km,nf)
       integer :: i,j,k,nz,myi,myid,numprocs,mpierr


       ! Change True/False parameters for netCDF Write Variables
       if(Which_chlaC.ne.2) call OUTPUT_NotCloern() !Gets rid of unused vars (Cloern)
       if(Which_Output.eq.1) call OUTPUT_NRL() !Limit Outputs
       if(Which_Output.eq.2) call OUTPUT_ALL_FALSE()


      WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), 0, '.nc'

      if(myid.eq.0) then
          CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                            im, jm, km, nstep, iYr0,       & 
                            IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                            IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                            DT_OUT, &
                            LON, LAT, d, FM, &
                            DZ )
          CALL CLOSE_FILE()
          IF (.FALSE.) WRITE(6,*) "numprocs = ", numprocs
      endif
      CALL MPI_BARRIER( MPI_COMM_WORLD,mpierr) ! Wait until file is created.

!For a model inter-comparison study through the Coastal Ocean Modeling Testbed
      if (MC .eq. 1) then
          if(myid .eq. 0) then
             CALL CREATE_FILE_MC(trim(comt_filename), im, jm, km, iYr0, LON, LAT, FM, code_id)
             CALL CLOSE_FILE_MC()
          endif
          CALL MPI_BARRIER( MPI_COMM_WORLD, mpierr ) ! Wait until file is created.
          CALL OPEN_FILE_MC(trim(comt_filename))
      endif


! Opens the output file for writing:
       CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME), 0 )

        dumf = f(1:myim,:,:,:)

        do j=1,jm
         myi = 1
         do i=myi_start,myi_end
          nz = nza(i,j)
          do k=1,nz
           dumf(myi,j,k,iTr) = f(myi,j,k,iTr) * Vol(i,j,k)
          enddo
          myi = myi+1
         enddo
        enddo

        CALL WRITE_DATA( myi_start, myim, 1,jm, 1, km, 0, dumf)
        CALL MPI_BARRIER( MPI_COMM_WORLD,mpierr ) ! Wait until file is updated.

#ifdef DEBUG
write(6,*) "---- Init_Output_CGEM ---"
write(6,*) 
#endif

       return
       End Subroutine Init_Output_CGEM
