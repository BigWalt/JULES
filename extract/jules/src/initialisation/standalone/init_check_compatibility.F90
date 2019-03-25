#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_check_compatibility_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_check_compatibility()
!-----------------------------------------------------------------------------
! Description:
!   Checks that the enabled science schemes are compatible. Refer to the JULES
!   user manual for more information.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_vegetation_mod,   ONLY: l_triffid, l_inferno, l_irrig_dmd
USE jules_soil_mod,         ONLY: l_tile_soil, l_holdwater
USE jules_radiation_mod,    ONLY: l_albedo_obs

USE logging_mod,            ONLY: log_info, log_error, log_fatal
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Work variables
INTEGER :: error  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

! Check for compatibility with soil tiling. At present it is not allowed for
! TRIFFID, INFERNO, and l_albedo_obs

IF ( l_triffid .AND. l_tile_soil ) THEN
  CALL log_error("init_check_compatibility",                                &
                 "TRIFFID not presently compatible with soil tiling")
END IF

IF ( l_inferno .AND. l_tile_soil ) THEN
  CALL log_error("init_check_compatibility",                                &
                 "INFERNO not presently compatible with soil tiling")
END IF

IF ( l_albedo_obs .AND. l_tile_soil ) THEN
  CALL log_error("init_check_compatibility",                                &
                 "l_albedo_obs not presently compatible with soil tiling")
END IF

IF (l_irrig_dmd .AND. l_holdwater) THEN
  CALL log_error("init_check_compatibility",                                &
                 "l_holdwater=T not presently compatible with l_irrig_dmd=T")
END IF

RETURN
END SUBROUTINE init_check_compatibility

END MODULE init_check_compatibility_mod
#endif
