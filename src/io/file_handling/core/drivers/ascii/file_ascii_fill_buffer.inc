#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_fill_buffer(file)

  USE io_constants, ONLY : mode_read
  USE errormessagelength_mod, ONLY: errormessagelength
  USE string_utils_mod, ONLY : str_starts_with

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE - reads the next data from file, skipping any comment
!   lines, and fills the buffer with it
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
  CHARACTER(LEN=file%record_len) :: line
  CHARACTER(LEN=errormessagelength) :: iomessage

  LOGICAL :: is_comment  ! Indicates if the current line is a comment line
  INTEGER :: i  ! Loop counter

  INTEGER :: error  ! Error indicator


!-----------------------------------------------------------------------------

! We can't fill the buffer until enddef has been called
  IF ( file%define_mode )                                                     &
    CALL log_fatal("file_ascii_fill_buffer",                                  &
                   "Cannot fill buffer - file is still in define mode")

  IF ( file%mode /= mode_read )                                               &
    CALL log_fatal("file_ascii_fill_buffer",                                  &
                   "Cannot read from file - file is not in read mode")

! Read lines from the file until we get a non-comment line
  DO
    READ(file%unit, "(A)", IOSTAT=error, IOMSG=iomessage) line
    IF ( error /= 0 )                                                         &
      CALL log_fatal("file_ascii_fill_buffer",                                &
                     "Error reading from file " //                            &
                     "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //     &
                     TRIM(iomessage) // ")")

! Remove any leading spaces
    line = ADJUSTL(line)

! We skip over empty lines
    IF ( LEN_TRIM(line) == 0 ) CYCLE

! If the line does not start with a comment char, we have found a line of data
! and can exit the loop
    is_comment = .FALSE.
    DO i = 1,SIZE(COMMENT_CHARS)
      IF ( str_starts_with(line, COMMENT_CHARS(i)) ) THEN
        is_comment = .TRUE.
        EXIT
      END IF
    END DO
    IF ( .NOT. is_comment ) EXIT
  END DO

! Fill the buffer from the line of data
  READ(line, *) file%buffer(:)

  RETURN

END SUBROUTINE file_ascii_fill_buffer
#endif
