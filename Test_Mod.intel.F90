       Module Test_Mod

        CHARACTER(LEN=:),DIMENSION(:),allocatable :: VARIABLE_NAMES
        LOGICAL,DIMENSION(:), allocatable :: WRITE_VARIABLE
        CHARACTER(LEN=:),DIMENSION(:), allocatable :: VARIABLE_DESCRIPTIONS
        CHARACTER(LEN=:),DIMENSION(:), allocatable :: VARIABLE_UNITS
        CHARACTER(LEN=:),DIMENSION(:), allocatable :: EXTRA_VARIABLE_NAMES
        LOGICAL,DIMENSION(:), allocatable :: WRITE_EXTRA_VARIABLE
        CHARACTER(LEN=:),DIMENSION(:),allocatable :: EXTRA_VARIABLE_DESCRIPTIONS
        CHARACTER(LEN=:),DIMENSION(:),allocatable :: EXTRA_VARIABLE_UNITS
        INTEGER,DIMENSION(:),allocatable :: EXTRA_VAR ! NetCDF IDs for extra vars.
        INTEGER,DIMENSION(:),allocatable :: F_VAR ! NetCDF IDs for each variable.

       END Module Test_Mod
