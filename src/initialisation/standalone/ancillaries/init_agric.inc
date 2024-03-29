#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_agric()

  USE io_constants, ONLY : mdi, max_sdf_name_len, max_file_name_len,          &
                           namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE input_mod, ONLY : input_grid => grid

  USE trifctl, ONLY : frac_agr_grid => frac_agr_gb

  USE trif_vars_mod, ONLY: frac_past_grid => frac_past_gb

  USE jules_vegetation_mod, ONLY: l_trif_crop

  USE dump_mod, ONLY: ancil_dump_read

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the agricultural fraction
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
! Definition of the jules_agric namelist
! For data at a single point, a single fraction is specified in the namelist
! In all other cases, agricultural fraction is read from a file
!-----------------------------------------------------------------------------
  LOGICAL :: read_from_dump
  LOGICAL :: zero_agric           ! T - set agr. frac. at all points to 0.0
                                  ! F - read agr. frac. from input
  LOGICAL :: zero_past            ! T - set agr. frac. at all points to 0.0
                                  ! F - read agr. frac. from input
  REAL :: frac_agr                ! Single point fraction
  REAL :: frac_past               ! Single point fraction

  CHARACTER(LEN=max_file_name_len) :: file
                                  ! The file to read fraction from
  CHARACTER(LEN=max_sdf_name_len) :: agric_name
                                  ! The name of the variable in the file
  CHARACTER(LEN=max_file_name_len) :: file_past
                                  ! The file to read fraction from
  CHARACTER(LEN=max_sdf_name_len) :: past_name
                                  ! The name of the variable in the file
  CHARACTER(LEN=errormessagelength) :: iomessage
                                  ! I/O error string
  NAMELIST /jules_agric/ read_from_dump, zero_agric, zero_past, frac_agr, &
                         frac_past,file, agric_name, file_past, past_name


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  read_from_dump = .FALSE.
  zero_agric = .TRUE.  ! Default is to set agricultural fraction to 0 everywhere
  zero_past  = .TRUE.  ! Default is to set pasture fraction to 0 everywhere
  frac_agr   = mdi     ! Initialised to missing data so we can tell if it is set
                       ! using the namelist
  frac_past  = mdi     ! Initialised to missing data so we can tell if it is set
                       ! using the namelist
  file       = ''
  agric_name = ''
  file_past  = ''
  past_name = ''

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_agric", "Reading JULES_AGRIC namelist...")

! First, we read the namelist
  READ(namelist_unit, nml=jules_agric, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_agric",                                              &
                   "Error reading namelist JULES_AGRIC " //                   &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  ancil_dump_read%agric = read_from_dump

  IF ( .NOT. l_trif_crop) zero_past=.TRUE.

  IF ( ancil_dump_read%agric) THEN !We read from the dump file
    CALL log_info("init_agric",                                               &
                  "agric ancils will be read from the dump file.  " //        &
                  "Namelist values ignored")
  ELSE !we read from the ancil file
    !---------------------------------------------------------------------------
    ! Set values derived from namelist and verify for consistency
    !---------------------------------------------------------------------------
    ! If zero fraction is selected, then that is all we have to do
    IF ( zero_agric ) THEN
      CALL log_info("init_agric", "Zero agricultural fraction indicated")
      frac_agr_grid(:) = 0.0
      frac_past_grid(:) = 0.0
      RETURN
    END IF

    IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
      !-------------------------------------------------------------------------
      ! If we are running a single point, read fraction from the namelist
      !-------------------------------------------------------------------------
      ! Check if frac_agr was set via the namelist.
      IF ( ABS(frac_agr - mdi) < EPSILON(frac_agr) )                          &
        CALL log_fatal("init_agric",                                          &
                       "For data at a single point, agricultural fraction "// &
                       "is read from the namelist JULES_AGRIC")

      ! Now we know they have been set, copy their values into the model arrays
      CALL log_info("init_agric",                                             &
                    "Data is at a single point - reading agricultural " //    &
                    "fraction from namelist JULES_AGRIC")
      frac_agr_grid(1) = frac_agr
    ELSE
      !-------------------------------------------------------------------------
      ! If we have a grid, set fraction from the specified file
      !-------------------------------------------------------------------------
      CALL log_info("init_agric",                                             &
                    "Data is on a grid - reading agricultural fraction " //   &
                    "from file " // TRIM(file))

      ! Check that file name was provided
      IF ( LEN_TRIM(file) == 0 )                                              &
        CALL log_fatal("init_agric", "No file name provided")

      CALL fill_variables_from_file(file, (/ 'frac_agr' /), (/ agric_name /))
    END IF

    !---------------------------------------------------------------------------
    ! Check that the values seem sensible
    !---------------------------------------------------------------------------
    IF ( ANY(frac_agr_grid < 0.0) .OR. ANY(frac_agr_grid > 1.0) )             &
      CALL log_fatal("init_agric",                                            &
                     "Agricultural fraction should be in range 0.0 to 1.0, "//&
                     "given range is " //                                     &
                     TRIM(to_string(MINVAL(frac_agr_grid))) // " to " //      &
                     TRIM(to_string(MAXVAL(frac_agr_grid))))
  END IF ! ancil_dump_read%agric

!---------------------------------------------------------------------------
! Read Pasture Ancil
!---------------------------------------------------------------------------

  IF ( .NOT. ancil_dump_read%agric) THEN !we read from the ancil file
    !---------------------------------------------------------------------------
    ! Set values derived from namelist and verify for consistency
    !---------------------------------------------------------------------------
    ! If zero fraction is selected, then that is all we have to do
    IF ( zero_past ) THEN
      CALL log_info("init_agric", "Zero pasture fraction indicated")
      frac_past_grid(:) = 0.0
      RETURN
    END IF

    IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
      !-------------------------------------------------------------------------
      ! If we are running a single point, read fraction from the namelist
      !-------------------------------------------------------------------------
      ! Check if frac_past was set via the namelist.
      IF ( ABS(frac_past - mdi) < EPSILON(frac_past) )                        &
        CALL log_fatal("init_agric",                                          &
                       "For data at a single point, pasture fraction "//      &
                       "is read from the namelist JULES_AGRIC")

      ! Now we know they have been set, copy their values into the model arrays
      CALL log_info("init_agric",                                             &
                    "Data is at a single point - reading pasture " //         &
                    "fraction from namelist JULES_AGRIC")
      frac_past_grid(1) = frac_past
    ELSE
      !-------------------------------------------------------------------------
      ! If we have a grid, set fraction from the specified file
      !-------------------------------------------------------------------------
      CALL log_info("init_agric",                                             &
                    "Data is on a grid - reading pasture fraction " //        &
                    "from file " // TRIM(file_past))

      ! Check that file name was provided
      IF ( LEN_TRIM(file_past) == 0 )                                         &
        CALL log_fatal("init_agric", "No file name provided")

      CALL fill_variables_from_file(file_past, (/ 'frac_past' /), (/ past_name /))
    END IF

    !---------------------------------------------------------------------------
    ! Check that the values seem sensible
    !---------------------------------------------------------------------------
    IF ( ANY(frac_past_grid < 0.0) .OR. ANY(frac_past_grid > 1.0) )           &
      CALL log_fatal("init_agric",                                            &
                     "Pasture fraction should be in range 0.0 to 1.0, "//     &
                     "given range is " //                                     &
                     TRIM(to_string(MINVAL(frac_past_grid))) // " to " //      &
                     TRIM(to_string(MAXVAL(frac_past_grid))))

  ELSE !We read from the dump file
    CALL log_info("init_agric",                                               &
                  "agric ancils will be read from the dump file.  " //        &
                  "Namelist values ignored")

  END IF !.NOT. ancil_dump_read%agric

  RETURN

END SUBROUTINE init_agric
#endif
