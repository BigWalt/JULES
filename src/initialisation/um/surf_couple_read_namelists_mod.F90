#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE surf_couple_read_namelists_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                       &
  ModuleName='SURF_COUPLE_READ_NAMELISTS_MOD'

CONTAINS

SUBROUTINE surf_couple_read_namelists(call_type, shared_unit, atmoscntl_unit)

!Wrapper routine for reading in the JULES namelists for the UM

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

!The SCM makes these calls individually as the calling sequence straddles
!opening and closing of file units.
!In the unlikely event of adding another CALL below, you may need to
!add them to:
!-control/top_level/scm_shell.F90

USE read_jules_namelists_mod, ONLY:                                           &
    read_jules_nvegparm,   read_jules_pftparm, read_jules_triffid,            &
    read_jules_elevate,    read_jules_urban_switches,                         &
    read_jules_vegetation, read_jules_urban2t_param,                          &
    read_jules_hydrology,  read_jules_radiation,  read_jules_sea_seaice,      &
    read_jules_snow,       read_jules_surface_types,                          &
    read_jules_soil,       read_jules_soil_biogeochem, read_jules_surface

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
CHARACTER(LEN=*), INTENT(IN) :: call_type
INTEGER,          INTENT(IN) :: atmoscntl_unit  ! unit no. for ATMOSCNTL file
INTEGER,          INTENT(IN) :: shared_unit     ! unit no. for SHARED file

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),   PARAMETER :: RoutineName='SURF_COUPLE_READ_NAMELISTS'

!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Call in UM, reconfiguration and scm
CALL read_jules_surface_types(shared_unit)
CALL read_jules_surface(shared_unit)
CALL read_jules_radiation(shared_unit)
CALL read_jules_hydrology(shared_unit)
CALL read_jules_sea_seaice(shared_unit)
CALL read_jules_soil(shared_unit)
CALL read_jules_vegetation(shared_unit)
CALL read_jules_soil_biogeochem(shared_unit)
CALL read_jules_snow(shared_unit)
CALL read_jules_urban_switches(shared_unit)

!Not called in recon
IF (call_type == "UM") THEN
  CALL read_jules_nvegparm(atmoscntl_unit)
  CALL read_jules_pftparm(atmoscntl_unit)
  CALL read_jules_triffid(atmoscntl_unit)
  CALL read_jules_elevate(atmoscntl_unit)
  CALL read_jules_urban2t_param(atmoscntl_unit)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_read_namelists

END MODULE surf_couple_read_namelists_mod
#endif
