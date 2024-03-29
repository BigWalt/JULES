#if !defined(UM_JULES)
!******************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

SUBROUTINE file_ncdf_sync(file)

  USE netcdf, ONLY : nf90_sync

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Syncs the file to a disk
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(IN) :: file     ! The file to sync

! Local variables
  INTEGER :: error

!-----------------------------------------------------------------------------

  error = nf90_sync(file%id)

  IF ( error /= nf90_noerr )                                                  &
    CALL log_fatal_ncdf("file_ncdf_sync",                                     &
                        "Error syncing file " // TRIM(file%name), error)

  RETURN

END SUBROUTINE file_ncdf_sync
#endif
