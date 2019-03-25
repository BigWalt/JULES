#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE jules_final_mod

IMPLICIT NONE

CONTAINS

!#############################################################################

SUBROUTINE jules_final

USE jules_soil_biogeochem_mod, ONLY:                                          &
!  imported scalar parameters
    soil_model_ecosse,                                                        &
!  imported scalars (IN)
    soil_bgc_model

USE jules_vegetation_mod, ONLY:                                            &
!  imported scalars (IN)
    can_rad_mod

USE logging_mod, ONLY:                                                     &
!  imported procedures
    log_warn


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes final warning messages at the end of a JULES run.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local scalar parameters.
CHARACTER(LEN=*), PARAMETER ::                                              &
  proc_name = 'jules_final'   !  Name of this procedure.

!-----------------------------------------------------------------------------
! Warn if a deprecated value of can_rad_mod is used.
! Using multiple calls to make this more prominent!
!-----------------------------------------------------------------------------
SELECT CASE ( can_rad_mod )
CASE ( 2, 3 )
  CALL log_warn(proc_name,'#############################################')
  CALL log_warn(proc_name,'#############################################')
  CALL log_warn(proc_name, 'can_rad_mod=2 and 3 are deprecated and ' //   &
                'will be removed in future. Use 1 or 6 instead.')
  CALL log_warn(proc_name,'#############################################')
  CALL log_warn(proc_name,'#############################################')
END SELECT

!-----------------------------------------------------------------------------
! Warn if the ECOSSE soil model is used - it is not fully functional in this
! version.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL log_warn(proc_name,'#############################################')
  CALL log_warn(proc_name, 'The ECOSSE soil model is not fully '    //      &
                           'functional in this version. It should ' //      &
                           'not be used!')
  CALL log_warn(proc_name,'#############################################')
END IF

END SUBROUTINE jules_final

!#############################################################################

END MODULE jules_final_mod
#endif
