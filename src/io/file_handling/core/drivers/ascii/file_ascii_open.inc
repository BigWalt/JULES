#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ascii_open(name, mode, comm, info) RESULT(file)

  USE io_constants, ONLY : mode_read, mode_write, unit_stdin, unit_stdout
  USE errormessagelength_mod, ONLY: errormessagelength
  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Opens a file and returns a file_ascii object representing it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  CHARACTER(LEN=*), INTENT(IN) :: name
                              ! The name of the file
  INTEGER, INTENT(IN) :: mode ! The mode to open the file
                              ! One of mode_read or mode_write
  INTEGER, OPTIONAL, INTENT(IN) :: comm  ! MPI communicator to use for parallel
                                         ! I/O
                                         ! If this is given, an error is raised
                                         ! as it is not possible to use parallel
                                         ! access with this ASCII driver
  INTEGER, OPTIONAL, INTENT(IN) :: info  ! MPI info object to use for parallel
                                         ! I/O
                                         ! If this is given, an error is raised
                                         ! as it is not possible to use parallel
                                         ! access with this ASCII driver

!-----------------------------------------------------------------------------
! We know from the conditions imposed by file_open that comm and info are
! either both present or both not present
!-----------------------------------------------------------------------------

! Return type
  TYPE(file_ascii) :: file


! Work variables
  INTEGER :: unit ! The unit number for the opened file
  INTEGER :: error ! Error code for any errors that occur
  INTEGER :: ntasks  ! The number of MPI tasks

  LOGICAL :: unit_in_use ! Used in call to INQUIRE to detect if a unit is in use
  INTEGER :: i  ! Loop counter
  
  CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! If comm and info are specified and the number of available tasks is > 1,
! raise an error since parallel access is not possible with this ASCII driver
!-----------------------------------------------------------------------------
  IF ( PRESENT(comm) ) THEN
    CALL mpi_comm_size(comm, ntasks, error)

    IF ( ntasks > 1 )                                                         &
      CALL log_fatal("file_ascii_open",                                       &
                     "Parallel access is not available for ASCII files. " //  &
                     "File: " // TRIM(name) )
  END IF

!-----------------------------------------------------------------------------
! Get a spare unit to open the file
!-----------------------------------------------------------------------------
  unit = 0
! Search the allowed unit numbers until we find one with nothing connected
  DO i = 1,MAX_UNIT_NUMBER
! Avoid units for standard i/o
    IF ( i == unit_stdin .OR. i == unit_stdout ) CYCLE

! Find out if anything is connected to this unit.
    INQUIRE(unit=i, opened=unit_in_use )
    IF ( .NOT. unit_in_use ) THEN
! This unit is free, so exit the loop
      unit = i
      EXIT
    END IF
  END DO

  IF ( unit < 1 ) THEN
    CALL log_fatal("file_ascii_open",                                         &
                   "All allowed units are in use - try increasing " //        &
                   "MAX_UNIT_NUMBER")
  END IF


!-----------------------------------------------------------------------------
! Open the file in the requested mode
!-----------------------------------------------------------------------------
  SELECT CASE ( mode )
    CASE ( mode_read )
      CALL log_info("file_ascii_open",                                        &
                    "Opening file " // TRIM(name) // " for reading")
! Open file for reading only - file must already exist
      OPEN(unit, FILE=name, STATUS='old', POSITION='rewind', ACTION='read',   &
                 IOSTAT=error, IOMSG=iomessage)

    CASE ( mode_write )
      CALL log_info("file_ascii_open",                                        &
                    "Opening file " // TRIM(name) // " for writing")
! Create an empty file for writing only - if a file with the
! same name exists, overwrite it
      OPEN(unit, FILE=name, STATUS='replace', POSITION='rewind',              &
                 ACTION='write', IOSTAT=error)

    CASE DEFAULT
! Read and write are the only supported modes
      CALL log_fatal("file_ascii_open",                                       &
                     "Unsupported mode - " // TRIM(to_string(mode)))

  END SELECT

!-----------------------------------------------------------------------------
! Report any errors
!-----------------------------------------------------------------------------
  IF ( error /= 0 )                                                           &
    CALL log_fatal("file_ascii_open",                                         &
                   "Error opening file " // TRIM(name) //                     &
                   " (IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //      &
                   TRIM(iomessage) // ")")


! Initialise the file_ascii object
  file%name = name
  file%mode = mode
  file%unit = unit

  RETURN

END FUNCTION file_ascii_open
#endif
