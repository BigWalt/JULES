#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_seek(file, record)

  USE driver_ascii_mod, ONLY : file_ascii_seek
  USE driver_ncdf_mod, ONLY : file_ncdf_seek

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file to before a particular record (i.e. the next time a
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
  TYPE(file_handle), INTENT(INOUT) :: file  ! The file to seek
  INTEGER, INTENT(IN) :: record           ! The record number to seek to


!-----------------------------------------------------------------------------

  SELECT CASE ( file%driver )
    CASE ( driver_ascii )
      CALL file_ascii_seek(file%ascii, record)

    CASE ( driver_ncdf )
      CALL file_ncdf_seek(file%ncdf, record)

    CASE DEFAULT
      CALL log_fatal("file_seek",                                             &
                     "Unrecognised driver - see docs for available drivers")
  END SELECT

  RETURN

END SUBROUTINE file_seek
#endif
