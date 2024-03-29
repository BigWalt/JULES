#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_seek(file, record)

  USE io_constants, ONLY : mode_read
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file to a particular record (i.e. the next time
!   values are read from the file using file_read_var, they will be read from
!   the requested record)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to seek
  INTEGER, INTENT(IN) :: record            ! The record number to seek to


! Work variables
  INTEGER :: error  ! Error indicator

  INTEGER :: i  ! Loop counter
  
  CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------

! We can't seek the file if it's in define mode
  IF ( file%define_mode )                                                     &
    CALL log_fatal("file_ascii_seek",                                         &
                   "Cannot advance - file is still in define mode")

! Arbitrary seeks are only allowed in read mode
  IF ( file%mode /= mode_read )                                               &
    CALL log_fatal("file_ascii_seek",                                         &
                   "Arbitrary seeks are only allowed in read mode")

! We can only seek if there is a record dimension
  IF ( .NOT. file%has_record_dim )                                            &
    CALL log_fatal("file_ascii_seek",                                         &
                   "Cannot advance file - no record dimension defined")

!-----------------------------------------------------------------------------
! Actually seek the file
! We do this by rewinding the file to the start, then using fill_buffer to
! skip comment lines
!-----------------------------------------------------------------------------
  REWIND(file%unit, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("file_ascii_seek",                                         &
                   "Error rewinding file " //                                 &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  DO i = 1,record
    CALL file_ascii_fill_buffer(file)
  END DO

! By the time we get to here, the buffer is filled with the data from record
! #record, which is what we want...!

  RETURN

END SUBROUTINE file_ascii_seek
#endif
