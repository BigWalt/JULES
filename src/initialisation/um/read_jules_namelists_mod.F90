#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Wrapper module containing subroutines for reading JULES namelists
!
MODULE read_jules_namelists_mod

! Description:
!  Contains read_jules_<namelist> and read_<urban_namelist> subroutines
!  for reading namelists into JULES during a UM-JULES job.
!
! Method:
!  The unit number holding the namelist is passed as the sole argument
!  to each file.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: top_level
!
! Code Description:
!  Language: FORTRAN 95.
!  This code is written to UMDP3 v8.5 programming standards.

USE check_iostat_mod, ONLY:                                                   &
  check_iostat

USE umPrintMgr     ,  ONLY:                                                   &
  PrintStatus, PrStatus_Oper

USE UM_ParCore,       ONLY:                                                   &
  mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Local variables common to each subroutine
INTEGER, PRIVATE             :: errorstatus

INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_out = 1

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='READ_JULES_NAMELISTS_MOD'

CONTAINS

SUBROUTINE read_jules_elevate (unitnumber)

! Description:
!  Read the JULES_ELEVATE namelist

USE c_elevate,        ONLY:                                                   &
  jules_elevate, print_nlist_jules_elevate, read_nml_jules_elevate

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_ELEVATE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_elevate(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_elevate()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_elevate

! *********************************************************************

SUBROUTINE read_jules_hydrology (unitnumber)

! Description:
!  Read the JULES_HYDROLOGY namelist

USE jules_hydrology_mod,  ONLY:                                               &
  jules_hydrology, print_nlist_jules_hydrology, check_jules_hydrology,        &
  read_nml_jules_hydrology

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_HYDROLOGY'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_hydrology(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_hydrology()
END IF
CALL check_jules_hydrology()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_hydrology

! *********************************************************************

SUBROUTINE read_jules_nvegparm (unitnumber)

! Description:
!  read the JULES_NVEGPARM namelist

USE nvegparm_io,      ONLY:                                                   &
  jules_nvegparm, print_nlist_jules_nvegparm, read_nml_jules_nvegparm

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_NVEGPARM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_nvegparm(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_nvegparm()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_nvegparm

! *********************************************************************

SUBROUTINE read_jules_pftparm (unitnumber)

! Description:
!  Read the JULES_PFTPARM namelist

USE pftparm_io,       ONLY:                                                   &
  jules_pftparm, print_nlist_jules_pftparm, read_nml_jules_pftparm

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_PFTPARM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_pftparm(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_pftparm()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_pftparm

! *********************************************************************

SUBROUTINE read_jules_radiation (unitnumber)

! Description:
!  Read the JULES_RADIATION namelist

USE jules_radiation_mod,  ONLY:                                               &
  jules_radiation, print_nlist_jules_radiation, check_jules_radiation,        &
  read_nml_jules_radiation

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_RADIATION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_radiation(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_radiation()
END IF
CALL check_jules_radiation()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_radiation

! *********************************************************************

SUBROUTINE read_jules_sea_seaice (unitnumber)

! Description:
!  Read the jules_sea_seaice namelist

USE jules_sea_seaice_mod,  ONLY:                                              &
  jules_sea_seaice, print_nlist_jules_sea_seaice, check_jules_sea_seaice,     &
  read_nml_jules_sea_seaice

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SEA_SEAICE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_sea_seaice(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_sea_seaice()
END IF
CALL check_jules_sea_seaice()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_sea_seaice

! *********************************************************************

SUBROUTINE read_jules_snow (unitnumber)

! Description:
!  Read the JULES_SNOW namelist

USE jules_snow_mod,  ONLY:                                                    &
  jules_snow, print_nlist_jules_snow, check_jules_snow,                       &
  read_nml_jules_snow

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SNOW'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_snow(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_snow()
END IF
CALL check_jules_snow()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_snow

! *********************************************************************

SUBROUTINE read_jules_soil (unitnumber)

! Description:
!  Read the JULES_SOIL namelist

USE jules_soil_mod,  ONLY:                                                    &
  jules_soil, print_nlist_jules_soil, check_jules_soil,                       &
  read_nml_jules_soil

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SOIL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_soil(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_soil()
END IF
! Since we need access to sm_levels, we run the check in
! init_from_jules_namelists
! The check is not required for the reconfiguration - dzsoil_io is used
! directly in rcf_readnl_vertical
!  CALL check_jules_soil(sm_levels)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_soil

! *********************************************************************

SUBROUTINE read_jules_soil_biogeochem (unitnumber)

! Description:
!  Read the JULES_SOIL_BIOGEOCHEM namelist

USE jules_soil_biogeochem_mod,  ONLY:                                         &
  jules_soil_biogeochem, print_nlist_jules_soil_biogeochem,                   &
  check_jules_soil_biogeochem, read_nml_jules_soil_biogeochem

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SOIL_BIOGEOCHEM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_soil_biogeochem(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_soil_biogeochem()
END IF
CALL check_jules_soil_biogeochem()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_soil_biogeochem

! *********************************************************************

SUBROUTINE read_jules_surface (unitnumber)

! Description:
!  Read the jules_surface namelist

USE jules_surface_mod,  ONLY:                                                 &
  jules_surface, print_nlist_jules_surface, check_jules_surface,              &
  read_nml_jules_surface

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SURFACE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_surface(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_surface()
END IF
CALL check_jules_surface()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_surface

! *********************************************************************

SUBROUTINE read_jules_surface_types (unitnumber)

! Description:
!  Read the JULES_SURFACE_TYPES namelist

USE jules_surface_types_mod,  ONLY:                                           &
  jules_surface_types, print_nlist_jules_surface_types,                       &
  check_jules_surface_types, read_nml_jules_surface_types

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_SURFACE_TYPES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_surface_types(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_surface_types()
END IF
CALL check_jules_surface_types()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_surface_types

! *********************************************************************

SUBROUTINE read_jules_triffid (unitnumber)

! Description:
!  Read the JULES_TRIFFID namelist

USE trif_io,          ONLY:                                                   &
  jules_triffid, print_nlist_jules_triffid, read_nml_jules_triffid

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_TRIFFID'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_triffid(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_triffid()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_triffid

! *********************************************************************

SUBROUTINE read_jules_vegetation (unitnumber)

! Description:
!  Read the JULES_VEGETATION namelist

USE jules_vegetation_mod,  ONLY:                                              &
  jules_vegetation, print_nlist_jules_vegetation,                             &
  check_jules_vegetation, read_nml_jules_vegetation

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_VEGETATION'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL read_nml_jules_vegetation(unitnumber)
IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_vegetation()
END IF
CALL check_jules_vegetation()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_vegetation

! *********************************************************************

SUBROUTINE read_jules_urban2t_param (unitnumber)

! Description:
!  Read the JULES_URBAN2T_PARAM namelist

USE urban_param,      ONLY:                                                   &
  jules_urban2t_param, print_nlist_jules_urban2t_param,                       &
  read_nml_jules_urban2t_param

USE jules_surface_mod, ONLY: l_urban2t

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_URBAN2T_PARAM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_urban2t ) CALL read_nml_jules_urban2t_param(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_urban2t_param()
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_urban2t_param

! *********************************************************************

SUBROUTINE read_jules_urban_switches (unitnumber)

! Description:
!  Read the JULES_URBAN_SWITCHES namelist

USE switches_urban,   ONLY:                                                   &
  jules_urban_switches, print_nlist_jules_urban_switches,                     &
  read_nml_jules_urban_switches, check_jules_urban_switches

USE jules_surface_mod, ONLY: l_urban2t

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

REAL(KIND=jprb) :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_JULES_URBAN_SWITCHES'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_urban2t ) CALL read_nml_jules_urban_switches(unitnumber)

IF (PrintStatus >= PrStatus_Oper .AND. mype == 0) THEN
  CALL print_nlist_jules_urban_switches()
END IF
CALL check_jules_urban_switches()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_jules_urban_switches

END MODULE read_jules_namelists_mod
#endif
