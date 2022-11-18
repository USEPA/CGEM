       Module OUTPUT 

        SAVE

        CHARACTER(LEN=6),DIMENSION(:),allocatable :: VARIABLE_NAMES
        LOGICAL,DIMENSION(:), allocatable :: WRITE_VARIABLE
        CHARACTER(LEN=100),DIMENSION(:), allocatable :: VARIABLE_DESCRIPTIONS
        CHARACTER(LEN=100),DIMENSION(:), allocatable :: VARIABLE_UNITS
        CHARACTER(LEN=19),DIMENSION(:), allocatable :: EXTRA_VARIABLE_NAMES
        LOGICAL,DIMENSION(:), allocatable :: WRITE_EXTRA_VARIABLE
        CHARACTER(LEN=100),DIMENSION(:),allocatable :: EXTRA_VARIABLE_DESCRIPTIONS
        CHARACTER(LEN=100),DIMENSION(:),allocatable :: EXTRA_VARIABLE_UNITS
        INTEGER,DIMENSION(:),allocatable :: EXTRA_VAR ! NetCDF IDs for extra vars.
        INTEGER,DIMENSION(:),allocatable :: F_VAR ! NetCDF IDs for each variable.
        INTEGER,save :: EXTRA_VARIABLES
        INTEGER,save :: STATE_VARIABLES

        REAL, DIMENSION(:,:,:), allocatable :: SUM_PrimProd
        REAL, DIMENSION(:,:,:), allocatable :: SUM_PrimProd_out
        REAL, DIMENSION(:,:,:), allocatable :: SUM_RESP
        REAL, DIMENSION(:,:), allocatable :: FO2
        REAL, DIMENSION(:,:), allocatable :: FNO3
        REAL, DIMENSION(:,:), allocatable :: FNH4
        REAL, DIMENSION(:,:), allocatable :: FPO4
        REAL, DIMENSION(:,:), allocatable :: FPOM

       END Module OUTPUT 
