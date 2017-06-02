!********************************************************************
! PURPOSE: netcdf_utils.F90 - Helper routines for netcdf operations
!
! Assumptions:  The subroutine getVarAtTimeXX assumes that the 
!                 supplied netcdf files have a certain format 
!                 For 2d variables: (x,y,time,data)
!                 For 3d variables: (x,y,z,time,data)
!               The subroutines getVarAtTimeXX and interpVarXX
!                 assume that IM, JM, NSL variables are loaded
!                 for allocating proper array sizes
!******************************************************************** 

MODULE netcdf_utils


USE Model_dim
IMPLICIT NONE



PUBLIC netCDF_file, open_file, close_file, interpVar, getTimeIndex, report_info, init_info

type netCDF_file
   integer :: ncid
   integer :: ndims
   integer :: nvars
   integer :: natts
   integer :: unlim
   character(200) :: fileName
   integer, dimension(:), allocatable :: dimLengths
   character(50), allocatable :: dimNames(:)
   character(50), allocatable :: varNames(:)
   integer, dimension(:), allocatable :: varTypes
   integer, dimension(:), allocatable :: varDims
   integer, dimension(:), allocatable :: varAtts
end type netCDF_file


PRIVATE report_info_from_id, report_info_from_filename, report_info_from_type, init_info_from_id, init_info_from_filename, & 
        init_info_from_type, interpVar3d, interpVar2d, getVarAtTime2d, getVarAtTime3d

  INTERFACE report_info
    MODULE PROCEDURE report_info_from_id, report_info_from_filename, report_info_from_type
  END INTERFACE

  INTERFACE init_info
    MODULE PROCEDURE init_info_from_id, init_info_from_filename, init_info_from_type
  END INTERFACE

  INTERFACE interpVar
    MODULE PROCEDURE interpVar3d, interpVar2d
  END INTERFACE

CONTAINS

  !Open a NetCDF file
  SUBROUTINE open_file(ncfile, mode, ncid)
    IMPLICIT NONE
  
    character(len=*), INTENT(IN) :: ncfile
    integer, INTENT(IN) :: mode
    integer, INTENT(OUT) :: ncid

    call nf_open(ncfile,mode,ncid)
  END SUBROUTINE open_file

  !Close a NetCDF file
  SUBROUTINE close_file(ncid)
    IMPLICIT NONE

    integer, INTENT(IN) :: ncid
    call nf_close(ncid)
  END SUBROUTINE


  !Initializes netCDF_file type
  !   Notes:  opens and closes file
  !           called via interface init_info
  SUBROUTINE init_info_from_filename(ncfile, info)
    IMPLICIT NONE

    character(len=*), INTENT(IN) :: ncfile
    type (netCDF_file), INTENT(OUT) :: info

    integer :: ncid

    call open_file(ncfile, 0, ncid)

    call init_info_from_id(ncid, info)

    call close_file(ncid)
  END SUBROUTINE init_info_from_filename

  !Initializes netCDF_file type
  !   Notes: assumes file is open.  Does not close.
  !          called via interface init_info
  SUBROUTINE init_info_from_id(ncid, info)
    IMPLICIT NONE
    
    integer, INTENT(IN) :: ncid
    type (netCDF_file), INTENT(OUT) :: info
    
    info%ncid = ncid
    call init_info_from_type(info)

  END SUBROUTINE init_info_from_id

  !Initializes netCDF_file type
  !   Notes: assumes file is open. Does not close
  !          called via interface init_info
  SUBROUTINE init_info_from_type(info)
    IMPLICIT NONE

    type (netCDF_file), INTENT(INOUT) :: info
    integer :: i,j,k, temp
    integer, dimension(:), allocatable ::  varDimIds
  
    ! get file info
    call nf_inq(info%ncid, info%ndims, info%nvars, info%natts, info%unlim)

    allocate(info%dimNames(info%ndims))
    allocate(info%dimLengths(info%ndims))
    allocate(info%varNames(info%nvars))
    allocate(info%varTypes(info%nvars))
    allocate(info%varDims(info%nvars))
    allocate(info%varAtts(info%nvars))
 
    !get dim info
    do i=1, info%ndims
      call nf_inq_dim(info%ncid, i, info%dimNames(i), info%dimLengths(i))
    enddo

    !get var info
    do i=1, info%nvars
      call nf_inq_varndims(info%ncid, i, temp)
      allocate(varDimIds(temp))
      call nf_inq_var(info%ncid, i, info%varNames(i), info%varTypes(i), info%varDims(i), varDimIds, info%varAtts(i))
      deallocate(varDimIds)
    enddo

  END SUBROUTINE init_info_from_type

  !Prints netCDF_file info to screen
  !   Notes:  assumes file is open.  Does not close.
  !           called via interface report_info
  SUBROUTINE report_info_from_type(info)
    IMPLICIT NONE

    type (netCDF_file), INTENT(IN) :: info
    integer :: i

    ! print file info
    write(6,*) "ndims: ", info%ndims
    write(6,*) "nvars: ", info%nvars
    write(6,*) "natts: ", info%natts
    write(6,*) "unlim: ", info%unlim
    write(6,*)
    
    ! print dim info
    do i = 1, info%ndims
      write(6,*) "dimName: ", info%dimNames(i)
      write(6,*) "    length: ", info%dimLengths(i)  
    enddo
    write(6,*)
    
    ! print var info
    do i = 1, info%nvars
      write(6,*) "VarId: ", i
      write(6,*) "  varName: ", info%varNames(i)
      write(6,*) "  varType: ", info%varTypes(i)
      write(6,*) "  varDim : ", info%varDims(i)
    enddo

  END SUBROUTINE report_info_from_type
  
  !Prints netCDF_file info to screen
  !   Notes:  opens and closes file
  !           called via interface report_info
  SUBROUTINE report_info_from_filename(ncfile, info)
    IMPLICIT NONE

    character(len=*), INTENT(IN) :: ncfile
    type (netCDF_file), INTENT(OUT) :: info

    integer :: ncid

    call open_file(ncfile, 0, ncid)

    call report_info_from_id(ncid, info)

    call close_file(ncid)
  END SUBROUTINE report_info_from_filename

  !Prints netCDF_file info to screen
  !   Notes:  assumes file is open.  Does not close.
  !           called via interface report_info
  SUBROUTINE report_info_from_id(ncid, info)
    IMPLICIT NONE
    
    integer, INTENT(IN) :: ncid
    type (netCDF_file), INTENT(OUT) :: info
    
    info%ncid = ncid
    call report_info_from_type(info)

  END SUBROUTINE report_info_from_id


  ! Finds time values to bookend current time (t_current)
  SUBROUTINE getTimeIndex(ncid, t_current, tVar_index, start_index, t1, t2)
    IMPLICIT NONE

    integer, INTENT(IN) :: ncid, tVar_index
    integer, INTENT(INOUT) :: start_index
    integer(kind=8), INTENT(IN) :: t_current
    integer(kind=8), INTENT(OUT) :: t1,t2
    integer :: timeLength
    integer :: i
    integer(kind=8), dimension(:), allocatable :: timeVals
    
    ! get time var values 
    call nf_inq_dimlen(ncid, tVar_index, timeLength)

    allocate(timeVals(timeLength))
    call nf_get_var_int64(ncid,tVar_index,timeVals)

    ! check if requested timeVal is inside dataset
    if (t_current.lt.timeVals(1) .OR. (t_current.gt.timeVals(timeLength))) then
       write(6,*) "Requested time value (",t_current,") is not in dataset!"
       write(6,*) " dataset has time range of ", timeVals(1), " to ", timeVals(timeLength)
       write(6,*) "   ...Stopping simulation"
       STOP
    endif

    do i=start_index, timeLength
      if(timeVals(i)-t_current.gt.0) then
        start_index = i-1
        t1 = timeVals(i-1)
        t2 = timeVals(i)
        exit
      endif
    enddo
    
    !check if is last entry in dataset
    if (ABS(timeVals(timeLength) - t_current) .LT.0) then
      t1 = t_current
      t2 = 0
      start_index = timeLength
    endif

  END SUBROUTINE getTimeIndex

  !Returns 3d variable at index var_index at tstep
  ! This assumes netcdf files has variables (X,Y,Z,Time,Var)
  SUBROUTINE getVarAtTime3d(ncid, var_index, tstep, varSlice)
    IMPLICIT NONE
    
    integer, INTENT(IN) :: ncid, tstep, var_index       !file id, timestep, netcdf variable index
    real, dimension(IM,JM,NSL), INTENT(OUT) :: varSlice !variable values at timestep
        
    call nf_get_vara_real(ncid, var_index, (/1,1,1,tstep/), (/IM, JM, NSL, 1/), varSlice)  

  END SUBROUTINE getVarAtTime3d

  !Returns 2d variable at var_index at tstep
  ! This assumes netcdf files has variables (X,Y,Time,Var)
  SUBROUTINE getVarAtTime2d(ncid, var_index, tstep, varSlice)
    IMPLICIT NONE
    
    integer, INTENT(IN) :: ncid, tstep, var_index   !file id, timestep, netcdf variable index
    real, dimension(IM,JM), INTENT(OUT) :: varSlice !variable values at timestep
        
    call nf_get_vara_real(ncid, var_index, (/1,1,tstep/), (/IM, JM, 1/), varSlice)        

  END SUBROUTINE getVarAtTime2d

  ! interpolates value of 3d variable
  SUBROUTINE interpVar3d(info, t_current, var_index, tvar_index, tstep, var)  
    IMPLICIT NONE

    type (netCDF_file), INTENT(IN) :: info             !info for netcdf file
    integer, INTENT(IN) :: var_index, tvar_index       !index in netcdf of interp variable and time variable
    integer, INTENT(INOUT) :: tstep                    !tstep to start looking for current time bookend
    integer(kind=8), INTENT(IN) :: t_current           !current time value to bookend
    real, dimension(:,:,:), INTENT(OUT):: var          !interpolated variable for output
    real, dimension(:,:,:), allocatable :: var1, var2  !bookend variable values
    integer(kind=8) :: t1, t2                          !bookend time values
    real :: fac
    integer :: i,j,k
    
    call getTimeIndex(info%ncid, t_current, tvar_index, tstep, t1, t2)

    allocate(var1(IM,JM,NSL))
    allocate(var2(IM,JM,NSL))
    call getVarAtTime3d(info%ncid, var_index, tstep, var1)
    if (tstep .LE. info%dimLengths(tvar_index)) then         
      call getVarAtTime3d(info%ncid, var_index, tstep+1, var2)
    else 
      write(6,*)"timestep is outside of supplied data range!"
      write(6,*)"Stopping execution (netcdf_utils.F90)"
      STOP
      !var2 = 0.0
    endif

#ifdef DEBUG_CWS    
    write(6,*)"      t1 = ", t1
    write(6,*)"      t2 = ", t2
    write(6,*)"      var1(9,26,3) = ", var1(9,26,3)
    write(6,*)"      var2(9,26,3) = ", var2(9,26,3)
#endif
    
    ! linear interpolation
    fac = real(t_current - t1)
    fac = real(fac,4) / real((t2-t1), 4)
    var = var1 + (var2-var1) *fac

  END SUBROUTINE interpVar3d

  ! interpolates value of 2d variable
  SUBROUTINE interpVar2d(info, t_current, var_index, tvar_index, tstep, var)  
    IMPLICIT NONE

    type (netCDF_file), INTENT(IN) :: info           !info for netcdf file
    integer, INTENT(IN) :: var_index, tvar_index     !index in netcdf of interp variable and time variable
    integer, INTENT(INOUT) :: tstep                  !tstep to start looking for current time bookend
    integer(kind=8), INTENT(IN) :: t_current         !current time value to bookend
    real, dimension(:,:), INTENT(OUT):: var          !interpolated variable for output
    real, dimension(:,:), allocatable :: var1, var2  !bookend variable values
    integer(kind=8) :: t1, t2                        !bookend time values
    integer :: i,j,k
    real :: fac

    call getTimeIndex(info%ncid, t_current, tvar_index, tstep, t1, t2)

    allocate(var1(IM,JM))
    allocate(var2(IM,JM))
    call getVarAtTime2d(info%ncid, var_index, tstep, var1)
    if (tstep .LE. info%dimLengths(tvar_index)) then                    
      call getVarAtTime2d(info%ncid, var_index, tstep+1, var2)
    else 
      write(6,*)"timestep is outside of supplied data range!"
      write(6,*)"Stopping execution (netcdf_utils.F90)"
      STOP
      !var2 = 0.0
    endif

#ifdef DEBUG_CWS    
    write(6,*)"      t1 = ", t1
    write(6,*)"      t2 = ", t2
    write(6,*)"      var1(9,26) = ", var1(9,26)
    write(6,*)"      var2(9,26) = ", var2(9,26)
#endif

    ! linear interpolation
    fac = real(t_current - t1)
    fac = real(fac,4) / real((t2-t1), 4)
    var = var1 + (var2-var1) *fac
    
  END SUBROUTINE interpVar2d

END MODULE netcdf_utils
