










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_surface_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_surface(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the surface namelist items and checks them for consistency
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

USE jules_surface_mod

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
! First, we read the surface namelist
!-----------------------------------------------------------------------------
CALL log_info("init_surface", "Reading JULES_SURFACE namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_surface.nml'),     &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = error,          &
     IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal("init_surface",                                            &
                 "Error opening namelist file jules_surface.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")
END IF

READ(namelist_unit, NML = jules_surface, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal("init_surface",                                            &
                 "Error reading namelist JULES_SURFACE " //                 &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")
END IF

CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal("init_surface",                                            &
                 "Error closing namelist file jules_surface.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")
END IF

CALL check_jules_surface()

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options
!-----------------------------------------------------------------------------
IF ( l_aggregate ) THEN
  CALL log_info("init_surface",                                             &
                "Aggregate surface scheme has been selected")
END IF

RETURN

END SUBROUTINE init_surface

END MODULE init_surface_mod
