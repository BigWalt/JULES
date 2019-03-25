#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_close(file)

  USE netcdf, ONLY : nf90_close
  USE io_constants, ONLY : mode_write

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Closes and frees any resources consumed by the given file object
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file ! The file to close


! Work variables
  INTEGER :: error ! Error code for any errors that occur

!-----------------------------------------------------------------------------

  CALL log_info("file_ncdf_close",                                            &
                "Closing file " // TRIM(file%name))

  IF ( file%mode == mode_write )                                              &
    CALL file_ncdf_sync(file)

  error = nf90_close(file%id)
  IF ( error /= nf90_noerr )                                                  &
    CALL log_fatal_ncdf("file_ncdf_close",                                    &
                        "Error closing file " // TRIM(file%name), error)

  RETURN

END SUBROUTINE file_ncdf_close
#endif
