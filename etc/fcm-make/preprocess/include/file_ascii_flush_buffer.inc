#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_flush_buffer(file)

  USE io_constants, ONLY : mode_write, mdi
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE - flushes the current buffer to file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file


! Work variables
  INTEGER :: error  ! Error indicator
  CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------

! We can't flush the buffer until enddef has been called
  IF ( file%define_mode )                                                     &
    CALL log_fatal("file_ascii_flush_buffer",                                 &
                   "Cannot flush buffer - file is still in define mode")

  IF ( file%mode /= mode_write )                                              &
    CALL log_fatal("file_ascii_flush_buffer",                                 &
                   "Cannot write to file - file is not in write mode")

! Just write the current buffer to the file
  WRITE(file%unit, out_format_str, IOSTAT=error, IOMSG=iomessage)             &
       file%buffer(:)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("file_ascii_flush_buffer",                                 &
                   "Error writing to file " //                                &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

! Reset buffer values to missing data value
  file%buffer(:) = mdi
! Buffer is clean again (i.e. has no writes that have not been committed to
! file)
  file%buffer_is_dirty = .FALSE.

  RETURN

END SUBROUTINE file_ascii_flush_buffer
#endif
