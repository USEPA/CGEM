!******************************************************************************
! PURPOSE: NETCDF_UTILITIES.F90 - Helper routines for writing to a NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!******************************************************************************

MODULE NETCDF_UTILITIES

  USE pnetcdf ! For NFMPI_*

  IMPLICIT NONE
  INCLUDE 'mpif.h' ! For MPI_*

PUBLIC CHKERR, DEFDIM, &
       DEFVI1, DEFVR1, DEFVD1, DEFVI2, DEFVR2, DEFVI3, DEFVR3, DEFVR4, &
       DEFVAR, DEFIAT, DEFRAT, DEFRATX, DEFTAT, &
       READIAT, READRAT, CHECKIAT, CHECKRAT, CONVERT_LONGITUDES, &
       DEFVRTATT

PRIVATE
CONTAINS

  ! Public

  ! Commands:

  ! CHKERR: Check error status and if not ok, print explanation & stop (UGLY).
  !
  SUBROUTINE CHKERR( ERR, MESSAGE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: ERR
    CHARACTER(LEN=*),INTENT(IN):: MESSAGE

    IF ( ERR .NE. 0 ) THEN
      PRINT *, 'PROBLEM: Failed to ', MESSAGE
      PRINT *, NFMPI_STRERROR( ERR )
      STOP
    END IF

    RETURN
  END SUBROUTINE CHKERR



  ! Define dimensions in a NetCDF file.
  !
  SUBROUTINE DEFDIM( FILEID, DIM_ID, NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, VALUE
    INTEGER,INTENT(OUT):: DIM_ID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    ! Locals:
    INTEGER ERR
    INTEGER(MPI_OFFSET_KIND):: VALUE_COPY

    VALUE_COPY = VALUE
    ERR = NFMPI_DEF_DIM( FILEID, TRIM( NAME ), VALUE_COPY, DIM_ID )
    CALL CHKERR( ERR, 'create dimension ' // NAME )
    RETURN
  END SUBROUTINE DEFDIM



  ! Define a integer 1D variable in a NetCDF file.
  !
  SUBROUTINE DEFVI1( FILEID, DIMID, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR
    INTEGER TEMP(1) ! Must use a temp array for NetCDF call.

    TEMP( 1 ) = DIMID
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 4, 1, TEMP, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVI1



  ! Define a real 1D variable in a NetCDF file.
  !
  SUBROUTINE DEFVR1( FILEID, DIMID, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR
    INTEGER TEMP(1) ! Must use a temp array for NetCDF call.

    TEMP( 1 ) = DIMID
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 5, 1, TEMP, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVR1



  ! Define a double 1D variable in a NetCDF file.
  !
  SUBROUTINE DEFVD1( FILEID, DIMID, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR
    INTEGER TEMP(1) ! Must use a temp array for NetCDF call.

    TEMP( 1 ) = DIMID
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 6, 1, TEMP, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVD1



  ! Define an integer 2D variable in a NetCDF file.
  !
  SUBROUTINE DEFVI2( FILEID, DIMID1, DIMID2, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID1, DIMID2
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR, DIM_IDS( 2 )

    DIM_IDS( 1 ) = DIMID1
    DIM_IDS( 2 ) = DIMID2
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 4, 2, DIM_IDS, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVI2



  ! Define a real 2D variable in a NetCDF file.
  !
  SUBROUTINE DEFVR2( FILEID, DIMID1, DIMID2, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID1, DIMID2
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR, DIM_IDS( 2 )

    DIM_IDS( 1 ) = DIMID1
    DIM_IDS( 2 ) = DIMID2
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 5, 2, DIM_IDS, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVR2



  ! Define an integer 3D variable in a NetCDF file.
  !
  SUBROUTINE DEFVI3( FILEID, DIMID1, DIMID2, DIMID3, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID1, DIMID2, DIMID3
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR, DIM_IDS( 3 )

    DIM_IDS( 1 ) = DIMID1
    DIM_IDS( 2 ) = DIMID2
    DIM_IDS( 3 ) = DIMID3
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 4, 3, DIM_IDS, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )
    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVI3



  ! Define a real 3D variable in a NetCDF file.
  !
  SUBROUTINE DEFVR3( FILEID, DIMID1, DIMID2, DIMID3, VARID, VARNAM, VARDES, &
                     UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, DIMID1, DIMID2, DIMID3
    INTEGER,INTENT(OUT):: VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR, DIM_IDS( 3 )

    DIM_IDS( 1 ) = DIMID1
    DIM_IDS( 2 ) = DIMID2
    DIM_IDS( 3 ) = DIMID3
    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 5, 3, DIM_IDS, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )

    ERR = NFMPI_PUT_ATT_TEXT(FILEID, VARID, 'coordinates', 18_8, 'latitude longitude')
    CALL CHKERR( ERR, 'create coordinates attribute ' // VARNAM )

    ERR = NFMPI_PUT_ATT_TEXT(FILEID, VARID, 'cell_measures', 10_8, 'area: Area')
    CALL CHKERR( ERR, 'create cell measures attribute ' // VARNAM )


    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )

    RETURN
  END SUBROUTINE DEFVR3



  ! Define a real 4D variable in a NetCDF file.
  !
  SUBROUTINE DEFVR4( FILEID, DIMIDS, VARID, VARNAM, VARDES, UNITS, IS_POSITIVE )
    IMPLICIT NONE
    INTEGER :: IS_POSITIVE  ! = 1 if valid range is positive only
    INTEGER, INTENT(IN):: FILEID
    INTEGER, DIMENSION(4), INTENT(IN):: DIMIDS
    INTEGER, INTENT(OUT):: VARID
    CHARACTER(LEN=*), INTENT(IN):: VARNAM, VARDES, UNITS
    INTEGER(MPI_OFFSET_KIND):: TWO  !Need to pass mpi_offset_kind
 
    ! Locals:
    INTEGER :: ERR

    TWO = 2
    if (is_positive .eq. 1) then
       ERR = nfmpi_put_att_real(fileid, VARID, "valid_range", 5, TWO, (/ 0., 1.e38 /))
    else
       ERR = nfmpi_put_att_real(fileid, VARID, "valid_range", 5, TWO, (/ -1.e2, 1.e38 /))
    endif

    ERR = NFMPI_DEF_VAR( FILEID, VARNAM, 5, 4, DIMIDS, VARID )
    CALL CHKERR( ERR, 'create variable ' // VARNAM )

    ERR = NFMPI_PUT_ATT_TEXT(FILEID, VARID, 'coordinates', 18_8, 'latitude longitude')
    CALL CHKERR( ERR, 'create coordinates attribute ' // VARNAM )

    ERR = NFMPI_PUT_ATT_TEXT(FILEID, VARID, 'cell_measures', 10_8, 'area: Area')
    CALL CHKERR( ERR, 'create cell measures attribute ' // VARNAM )


    CALL DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    RETURN
  END SUBROUTINE DEFVR4



  ! Define rest of a real variable in a NetCDF file.
  !
  SUBROUTINE DEFVAR( FILEID, VARID, VARNAM, VARDES, UNITS )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, VARID
    CHARACTER(LEN=*),INTENT(IN):: VARNAM, VARDES, UNITS
    ! Locals:
    INTEGER ERR
    INTEGER(MPI_OFFSET_KIND):: LEN_COPY

    LEN_COPY = LEN( VARDES )
    ERR = NFMPI_PUT_ATT_TEXT( FILEID, VARID, 'description', LEN_COPY, &
                              VARDES )
    CALL CHKERR( ERR, 'create description attribute for ' // VARNAM )

    IF ( LEN( UNITS ) > 0 ) THEN
      LEN_COPY = LEN( UNITS )
      ERR = NFMPI_PUT_ATT_TEXT( FILEID, VARID, 'units', LEN_COPY, UNITS )
      CALL CHKERR( ERR, 'create units attribute for ' // VARNAM )
    END IF

    RETURN
  END SUBROUTINE DEFVAR


  ! Define text attribute for variable in a NetCDF file.
  !
  SUBROUTINE DEFVRTATT( FILEID, VARID, ATTNAME, ATT)
    IMPLICIT NONE
    INTEGER , INTENT(IN):: FILEID, VARID
    CHARACTER(LEN=*),INTENT(IN):: ATTNAME, ATT
    ! Externals:
    INTEGER NF_PUT_ATT_TEXT
    EXTERNAL NF_PUT_ATT_TEXT
    ! Locals:
    INTEGER ERR
    INTEGER(MPI_OFFSET_KIND):: LEN_COPY

    LEN_COPY = LEN(ATT)
    ERR = NFMPI_PUT_ATT_TEXT(FILEID, VARID, ATTNAME, LEN_COPY, ATT)
    CALL CHKERR(ERR, 'create variable attribute ' // ATTNAME )

  END SUBROUTINE DEFVRTATT


  ! Define an integer attribute in a NetCDF file.
  !
  SUBROUTINE DEFIAT( FILEID, NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    INTEGER,INTENT(IN):: VALUE
    ! Locals:
    INTEGER(MPI_OFFSET_KIND) ONE
    INTEGER ERR, TEMP(1)

    ONE = 1
    TEMP(1) = VALUE ! Must copy for NetCDF call.
    ERR = NFMPI_PUT_ATT_INT( FILEID, 0, TRIM( NAME ), 4, ONE, TEMP )
    CALL CHKERR( ERR, 'create integer attribute ' // NAME )

    RETURN
  END SUBROUTINE DEFIAT



  ! Define a real attribute in a NetCDF file.
  !
  SUBROUTINE DEFRAT( FILEID, NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    REAL,INTENT(IN):: VALUE
    ! Locals:
    INTEGER(MPI_OFFSET_KIND) ONE
    INTEGER ERR
    REAL TEMP(1)

    ONE = 1
    TEMP( 1 ) = VALUE ! Must copy for NetCDF call.
    ERR = NFMPI_PUT_ATT_REAL( FILEID, 0, TRIM( NAME ), 5, ONE, TEMP )
    CALL CHKERR( ERR, 'create real attribute ' // NAME )

    RETURN
  END SUBROUTINE DEFRAT



  ! Define a real attribute of 8 values in a NetCDF file.
  !
  SUBROUTINE DEFRATX( FILEID, NAME, VALUE, X )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    INTEGER(MPI_OFFSET_KIND),INTENT(IN):: X
    CHARACTER(LEN=*),INTENT(IN):: NAME
    REAL,INTENT(IN):: VALUE(X)
    ! Locals:
    INTEGER ERR
    REAL TEMP(X)

    TEMP = VALUE ! Must copy for NetCDF call.
    ERR = NFMPI_PUT_ATT_REAL( FILEID, 0, TRIM( NAME ), 5, X, TEMP )
    CALL CHKERR( ERR, 'create X real attributes ' // NAME )

    RETURN
  END SUBROUTINE DEFRATX


  ! Define a text attribute in a NetCDF file.
  !
  SUBROUTINE DEFTAT( FILEID,  NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME, VALUE
    ! Locals:
    INTEGER ERR
    INTEGER(MPI_OFFSET_KIND):: LEN_VALUE
    LEN_VALUE = LEN( VALUE )

    ERR = NFMPI_PUT_ATT_TEXT( FILEID, 0, TRIM( NAME ), LEN_VALUE , VALUE )
    CALL CHKERR( ERR, 'create text attribute ' // NAME )

    RETURN
  END SUBROUTINE DEFTAT



  ! Read an integer attribute from a NetCDF file.
  !
  FUNCTION READIAT( FILEID, NAME ) RESULT( RES )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    INTEGER RES
    ! Locals:
    INTEGER ERR, TEMP(1)

    ERR = NFMPI_GET_ATT_INT( FILEID, 0, TRIM( NAME ), TEMP )
    CALL CHKERR( ERR, 'read integer attribute ' // NAME )
    RES = TEMP( 1 )
    RETURN
  END FUNCTION READIAT



  ! Read a real attribute from a NetCDF file.
  !
  FUNCTION READRAT( FILEID, NAME ) RESULT( RES )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    REAL RES
    ! Locals:
    INTEGER ERR
    REAL TEMP(1)

    ERR = NFMPI_GET_ATT_REAL( FILEID, 0, TRIM( NAME ), TEMP )
    CALL CHKERR( ERR, 'read real attribute ' // NAME )
    RES = TEMP( 1 )
    RETURN
  END FUNCTION READRAT



  ! Read and match an integer attribute from a NetCDF file.
  !
  SUBROUTINE CHECKIAT( FILEID, NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID, VALUE
    CHARACTER(LEN=*),INTENT(IN):: NAME
    ! Locals:
    INTEGER FILE_VALUE

    FILE_VALUE = READIAT( FILEID, NAME )

    IF ( VALUE .NE. VALUE ) THEN
      PRINT *, 'PROBLEM: Unmatched file attribute: ', TRIM( NAME ), &
               ' ', VALUE, ' does not match ', VALUE
      STOP
    END IF

    RETURN
  END SUBROUTINE CHECKIAT



  ! Read and match a real attribute from a NetCDF file.
  !
  SUBROUTINE CHECKRAT( FILEID, NAME, VALUE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: FILEID
    CHARACTER(LEN=*),INTENT(IN):: NAME
    REAL,INTENT(IN):: VALUE
    ! Locals:
    REAL FILE_VALUE

    FILE_VALUE = READRAT( FILEID, NAME )

    IF ( FILE_VALUE .NE. VALUE ) THEN
      PRINT *, 'PROBLEM: Unmatched file attribute: ', TRIM( NAME ), &
               ' ', VALUE, ' does not match ', VALUE
      STOP
    END IF

    RETURN
  END SUBROUTINE CHECKRAT



  ! CONVERT_LONGITUDE: Convert longitudes from [0, 360] to [-180, 180].
  !
  SUBROUTINE CONVERT_LONGITUDES( COUNT, LONGITUDES )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: COUNT
    REAL,DIMENSION(COUNT),INTENT(INOUT):: LONGITUDES
    ! Locals:
    INTEGER INDEX

    DO INDEX = 1, COUNT

      IF ( LONGITUDES( INDEX ) .GT. 180.0 ) THEN
        LONGITUDES( INDEX ) = LONGITUDES( INDEX ) - 360.0
      END IF
    END DO

    RETURN
  END SUBROUTINE CONVERT_LONGITUDES



END MODULE NETCDF_UTILITIES


