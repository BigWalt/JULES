#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_surf_hgt()

  USE io_constants, ONLY : max_sdf_name_len, max_file_name_len, namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE input_mod, ONLY : fill_variables_from_file

  use jules_surface_mod, ONLY : l_aggregate

  USE c_elevate, ONLY : surf_hgt_surft, surf_hgt_io, l_elev_absolute_height

  USE ancil_info, ONLY : nsurft

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises tile heights relative to the gridbox mean.
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
! Definition of the jules_surf_hgt namelist
!-----------------------------------------------------------------------------
  LOGICAL :: zero_height  ! T - set height at all points to 0.0
                          ! F - read height from file
  LOGICAL :: use_file     ! T - the variable uses the file
                          ! F - the variable is set using a constant value
  CHARACTER(LEN=max_file_name_len) :: file  ! If input grid has more than one
                                            ! point, read heights from this
                                            ! file
  CHARACTER(LEN=max_sdf_name_len) :: surf_hgt_name  ! Use the variable with
                                                    ! this name
  CHARACTER(LEN=errormessagelength) :: iomessage

  NAMELIST /jules_surf_hgt/ zero_height, l_elev_absolute_height, use_file,    &
                            file, surf_hgt_name, surf_hgt_io

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  zero_height               = .TRUE.  ! Default is to use zero height everywhere
  l_elev_absolute_height(:) = .FALSE. ! T - tiles have absolute heights above
                                      ! sea-level
                                      ! F - tiles have heights relative to the
                                      ! gridbox mean
  use_file                  = .TRUE.  ! Default is to read variable from a file
  file                      = ''      ! Empty file name.
  surf_hgt_name             = ''      ! Empty variable name.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_surf_hgt", "Reading JULES_SURF_HGT namelist...")

! First, we read the namelist
  READ(namelist_unit, nml=jules_surf_hgt, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_surf_hgt",                                           &
                   "Error reading namelist JULES_SURF_HGT " //                &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Set values derived from namelist and verify for consistency
!-----------------------------------------------------------------------------
! Zero height must be used with the aggregate surface scheme
  IF ( l_aggregate ) THEN
    CALL log_warn("init_surf_hgt",                                            &
                  "Aggregate surface selected, so using zero height")
    zero_height = .TRUE.
  END IF

! If zero height is selected, then that is all we have to do
  IF ( zero_height ) THEN
    CALL log_info("init_surf_hgt",                                            &
                  "Zero height selected - setting all heights to 0.0")
    surf_hgt_surft(:,:) = 0.0
    RETURN
  END IF

!-----------------------------------------------------------------------------
! All tiles have heights relative to the gridbox mean
!-----------------------------------------------------------------------------
  IF (.NOT. ANY(l_elev_absolute_height) ) THEN

    IF ( use_file ) THEN
!-----------------------------------------------------------------------------
! If we have a grid, set heights from the specified file
!-----------------------------------------------------------------------------
    CALL log_info("init_surf_hgt",                                            &
                  "Data is on a grid - reading surf_hgt from file " //        &
                  TRIM(file))

!   Check that a file name was provided
    IF ( LEN_TRIM(file) == 0 )                                                &
      CALL log_fatal("init_surf_hgt", "No file name provided")

    CALL fill_variables_from_file(file, (/ 'surf_hgt' /), (/ surf_hgt_name /))

  ELSE
!-----------------------------------------------------------------------------
! If we are reading data at a single point, read height from the namelist
!-----------------------------------------------------------------------------
! Copy their values into the model arrays
    CALL log_info("init_surf_hgt",                                            &
                  "Data is at a single point - reading surf_hgt from " //     &
                  "surf_hgt_io in namelist JULES_SURF_HGT")
    surf_hgt_surft(1,:) = surf_hgt_io(1:nsurft)

    END IF
  END IF

  RETURN

END SUBROUTINE init_surf_hgt
#endif
