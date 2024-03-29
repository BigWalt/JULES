#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_frac()

  USE io_constants, ONLY : max_file_name_len, max_sdf_name_len, namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE jules_vegetation_mod, ONLY : l_veg_compete
  
  USE dump_mod, ONLY: ancil_dump_read

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in the tile fractions and checks for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
  INTEGER :: error  ! Error indicator

!-----------------------------------------------------------------------------
! Definition of the jules_frac namelist
!-----------------------------------------------------------------------------
  LOGICAL :: read_from_dump
  CHARACTER(LEN=max_file_name_len) :: file
  CHARACTER(LEN=max_sdf_name_len) :: frac_name
  CHARACTER(LEN=errormessagelength) :: iomessage
  NAMELIST /jules_frac/ read_from_dump, file, frac_name

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  read_from_dump = .FALSE.
  file           = ''
  frac_name      = ''

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_frac", "Reading JULES_FRAC namelist...")

! First, we read the namelist
  READ(namelist_unit, nml=jules_frac, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_frac",                                               &
                   "Error reading namelist JULES_FRAC " //                    &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  ancil_dump_read%frac = read_from_dump

  IF ( .NOT. ancil_dump_read%frac) THEN !we read from the ancil file
    !-------------------------------------------------------------------------
    ! Set frac using namelist values
    !-------------------------------------------------------------------------
    ! Frac is prognostic if competing veg is on, so must be read as an initial
    ! condition
    IF ( l_veg_compete ) THEN
      CALL log_info("init_frac",                                              &
                    "Competing vegetation is enabled - frac will be read " // &
                    "as an initial condition")
      RETURN
    END IF

    !Check that file name was provided
    IF ( LEN_TRIM(file) == 0 )                                                &
      CALL log_fatal("init_frac", "No file name provided")

    CALL log_info("init_frac",                                                &
                  "Reading tile fractions from file " // TRIM(file))
    CALL fill_variables_from_file(file, (/ 'frac' /), (/ frac_name /))

  ELSE !We read from the dump file
    CALL log_info("init_frac",                                                &
                  "frac will be read from the dump file.  " //                &
                  "Namelist values ignored")

  END IF !.NOT. ancil_dump_read%frac

  RETURN

END SUBROUTINE init_frac
#endif
