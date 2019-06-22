       Subroutine Init_Output_CGEM(BASE_NETCDF_OUTPUT_FILE_NAME)

       USE Model_dim
       USE INPUT_VARS, ONLY: nstep,dT_out,IYRS,IMONS,&
     & IDAYS, IHRS, IMINS, ISECS, IYRE, IMONE, IDAYE, IHRE,&
     & IMINE, ISECE,Which_Output
       USE INPUT_VARS_CGEM, ONLY : Which_chlaC
       USE Grid
       USE OUTPUT_NETCDF_CGEM
       USE State_Vars
       USE CGEM_vars

       IMPLICIT NONE

       character(100),intent(in) :: BASE_NETCDF_OUTPUT_FILE_NAME
       character(256) :: NETCDF_OUTPUT_FILE_NAME
       real :: dumf(im,jm,km,nf)
       integer :: i,j,k,nz

#ifdef map_code
write(6,*) "---- Init_Output_CGEM ---"
#endif

       ! Change True/False parameters for netCDF Write Variables
       if(Which_chlaC.ne.2) call OUTPUT_NotCloern() !Gets rid of unused vars (Cloern)
       if(Which_Output.eq.1) call OUTPUT_NRL() !Limit Outputs
       if(Which_Output.eq.2) call OUTPUT_ALL_FALSE()

      !write(6,*) d
      !write(6,*) dz

      WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), 0, '.nc'
          CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                            im, jm, km, nstep, nf, EXTRA_VARIABLES, &
                            iYr0, &
                            IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                            IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                            DT_OUT, &
                            LON, LAT, d, FM, &
                            DZ )
          CALL CLOSE_FILE()

#ifdef DEBUG
write(6,*) "After Set_Vars"
#endif

! Opens the output file for writing:
       CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME), nf, EXTRA_VARIABLES, 0 )

#ifdef DEBUG
write(6,*) "After Set_Vars"
#endif
        dumf = f

        do j=1,jm
        do i=1,im
          nz = nza(i,j)
          do k=1,nz
           dumf(i,j,k,iTr) = f(i,j,k,iTr) * Vol(i,j,k)
          enddo
         enddo
        enddo

       CALL WRITE_DATA( im, jm, km, nf, 0, dumf)

#ifdef DEBUG
write(6,*) "After Write_Data"
#endif

       End Subroutine Init_Output_CGEM
