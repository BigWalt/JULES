










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_surface_types_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_surface_types(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the surface types namelist items and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod

USE logging_mod, ONLY: log_info, log_error, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: error  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage


!-----------------------------------------------------------------------------
! First, read the surface types namelist
!-----------------------------------------------------------------------------
CALL log_info("init_surface_types", "Reading JULES_SURFACE_TYPES namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_surface_types.nml'),&
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = error,          &
     IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_surface_types",                                      &
                 "Error opening namelist file jules_surface_types.nml " //  &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_surface_types, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_surface_types",                                      &
                 "Error reading namelist JULES_SURFACE_TYPES " //           &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_surface_types",                                      &
                 "Error closing namelist file jules_surface_types.nml " //  &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")


CALL check_jules_surface_types


! Report some info about the surface types
CALL log_info("init_surface_types",                                         &
              "Using " // TRIM(to_string(nnpft)) // " natural PFTs and " // &
              TRIM(to_string(ncpft)) // " crop PFTs")

CALL log_info("init_surface_types",                                         &
              "Using " // TRIM(to_string(nnvg)) // " non-veg surface types")

CALL log_info("init_surface_types",                                         &
              "Soil is type #" // TRIM(to_string(soil)))

IF ( lake > 0 ) THEN
  CALL log_info("init_surface_types",                                       &
                "Lake (inland water) is type #" // TRIM(to_string(lake)))
ELSE
  CALL log_info("init_surface_types",                                       &
                "No lake (inland water) type specified")
END IF

IF ( ice > 0 ) THEN
  CALL log_info("init_surface_types",                                       &
                "Land ice is type #" // TRIM(to_string(ice)))
ELSE
  CALL log_info("init_surface_types",                                       &
                "No land ice type specified")
END IF

IF ( urban > 0 ) THEN
  CALL log_info("init_surface_types",                                       &
                "Urban is type #" // TRIM(to_string(urban)))
ELSE
  CALL log_info("init_surface_types",                                       &
                "No urban type specified (URBAN-1T)")
END IF

IF ( urban_canyon > 0 ) THEN
  CALL log_info("init_surface_types",                                       &
                "Urban canyon is type #" // TRIM(to_string(urban_canyon)))
ELSE
  CALL log_info("init_surface_types",                                       &
                "No canyon type specified (URBAN-2T or MORUSES)")
END IF

IF ( urban_roof > 0 ) THEN
  CALL log_info("init_surface_types",                                       &
                "Urban roof is type #" // TRIM(to_string(urban_roof)))
ELSE
  CALL log_info("init_surface_types",                                       &
                "No roof type specified (URBAN-2T or MORUSES)")
END IF

RETURN
END SUBROUTINE init_surface_types

END MODULE init_surface_types_mod
