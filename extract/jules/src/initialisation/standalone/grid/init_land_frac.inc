#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_land_frac()

  USE io_constants, ONLY : max_sdf_name_len, max_file_name_len, namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE coastal, ONLY : flandg

  USE input_mod, ONLY : input_grid => grid, fill_variables_from_file
  
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in the land fraction on the full grid and checks for consistency
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
  CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Definition of the jules_land namelist - this combines variables
! from input_mod with some local variables
!-----------------------------------------------------------------------------
  CHARACTER(LEN=max_file_name_len) :: file
  CHARACTER(LEN=max_sdf_name_len) :: land_frac_name
  NAMELIST /jules_land_frac/ file, land_frac_name

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  file           = ''  !  Empty file name.
  land_frac_name = ''  !  Empty variable name.

  CALL log_info("init_land_frac", "Reading JULES_LAND_FRAC namelist...")

! First, we read the namelist
  READ(namelist_unit, nml=jules_land_frac, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_land_frac",                                          &
                   "Error reading namelist JULES_LAND_FRAC " //               &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Verify for consistency
!-----------------------------------------------------------------------------
  CALL log_info("init_land_frac",                                             &
                "Getting land fraction for the full input grid...")

! For now, we allocate flandg on the full input grid
  ALLOCATE(flandg(input_grid%nx,input_grid%ny), STAT=error)
  IF ( error > 0 )                                                            &
    CALL log_fatal("init_land_frac", "Error allocating flandg")

  IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
! For single point data, just assume the single point is 100% land
    CALL log_info("init_land_frac",                                           &
                  "Data is at a single point - setting land fraction to 1.0")
    flandg(1,1) = 1.0
  ELSE
! For any other grid, read land fraction from file
    CALL log_info("init_land_frac",                                           &
                  "Data is on a grid - reading land fraction from file " //   &
                  TRIM(file))
!   Check that a file name was provided.
    IF ( LEN_TRIM(file) == 0 )                                                &
      CALL log_fatal("init_land_frac", "No file name provided")

    CALL fill_variables_from_file(file, (/ 'land_fraction' /),                &
                                        (/  land_frac_name   /))
  END IF

! For now, make sure that land fraction is either 1.0 or 0.0 - until JULES
! can deal with coastal tiling
  WHERE ( flandg > EPSILON(1.0) )
    flandg = 1.0
  ELSEWHERE
    flandg = 0.0
  END WHERE

  RETURN

END SUBROUTINE init_land_frac
#endif
