#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_seek(file, record)

  USE io_constants, ONLY : mode_read

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file to a particular record (i.e. the next time values are read
!   from the file using file_read_var, they will be read from the requested
!   record)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file  ! The file to seek
  INTEGER, INTENT(IN) :: record           ! The record number to seek to


!-----------------------------------------------------------------------------

  IF ( file%mode /= mode_read )                                               &
    CALL log_fatal("file_ncdf_seek",                                          &
                   TRIM(file%name) // ": " //                                 &
                   "Arbitrary seeking is only allowed in read mode")

  IF ( file%record_dim < 0 )                                                  &
    CALL log_fatal("file_ncdf_seek",                                          &
                   TRIM(file%name) // ": " //                                 &
                   "Cannot seek - no record dimension defined")

  IF ( record < 1 )                                                           &
    CALL log_fatal("file_ncdf_seek",                                          &
                   TRIM(file%name) // ": " //                                 &
                   "Record number must be > 0")

  file%current_record = record

  RETURN

END SUBROUTINE file_ncdf_seek
#endif
