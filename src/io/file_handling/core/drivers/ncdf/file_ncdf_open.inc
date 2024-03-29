#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ncdf_open(name, mode, comm, info) RESULT(file)

  USE io_constants, ONLY : mode_read, mode_write

  USE netcdf, ONLY :                                                          &
! Constants
    nf90_nowrite, nf90_clobber,                                               &
#if !defined(MPI_DUMMY)
! We only need these constants if we have 'real' MPI, and not all non-mpi
! or pre version 4 netcdf libraries are compiled with them
    nf90_netcdf4, nf90_mpiio,                                                 &
#endif
! Procedures
    nf90_open, nf90_create

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Opens a NetCDF file and returns a file_ncdf object representing it
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
                                         ! If not given, serial I/O is used
  INTEGER, OPTIONAL, INTENT(IN) :: info  ! MPI info object to use for parallel
                                         ! I/O
                                         ! If not given, serial I/O is used

!-----------------------------------------------------------------------------
! We know from the conditions imposed by file_open that comm and info are
! either both present or both not present
!-----------------------------------------------------------------------------

! Return type
  TYPE(file_ncdf) :: file


! Work variables
  INTEGER :: ncid ! The NetCDF id of the opened file
  INTEGER :: error ! Error code for any errors that occur

!-----------------------------------------------------------------------------


  SELECT CASE ( mode )
    CASE ( mode_read )
      CALL log_info("file_ncdf_open",                                         &
                    "Opening file " // TRIM(name) // " for reading")
! Open file for reading only - file must already exist
! We don't need to specify comm or info to get parallel access here - any
! number of parallel readers are possible anyway...
      error = nf90_open(name, nf90_nowrite, ncid)

    CASE ( mode_write )
      CALL log_info("file_ncdf_open",                                         &
                    "Opening file " // TRIM(name) // " for writing")
! Create an empty file for (reading and) writing - if a file with the
! same name exists, overwrite it
      IF ( PRESENT(comm) ) THEN
! Don't try to specify MPI variables to NetCDF routines if we are using the
! dummy MPI library
#if defined(MPI_DUMMY)
        error = nf90_create(name, nf90_clobber, ncid)
#else
! If we want to use parallel I/O, we have to force NetCDF-4 files to be created
        error = nf90_create(                                                  &
          name, IOR(nf90_clobber, IOR(nf90_netcdf4, nf90_mpiio)), ncid,       &
          COMM=comm, INFO=info                                                &
        )
! Remember that this file is opened for parallel access, so that we can set
! collective access for variables later
        file%parallel = .TRUE.
#endif
      ELSE
        error = nf90_create(name, nf90_clobber, ncid)
      END IF

    CASE DEFAULT
! Read and write are the only supported modes
      CALL log_fatal("file_ncdf_open",                                        &
                     "Unsupported mode - " // TRIM(to_string(mode)))

  END SELECT

  IF ( error /= nf90_noerr )                                                  &
    CALL log_fatal_ncdf("file_ncdf_open",                                     &
                        "Error opening file " // TRIM(name), error)


! Initialise the file_ncdf object
  file%name = name
  file%mode = mode
  file%id   = ncid

  RETURN

END FUNCTION file_ncdf_open
#endif
