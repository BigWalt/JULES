#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_z_land()

  USE io_constants, ONLY : max_sdf_name_len, max_file_name_len, namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE input_mod, ONLY : fill_variables_from_file

  USE missing_data_mod, ONLY: rmdi

  USE jules_surface_mod, ONLY : l_aggregate

  USE c_elevate, ONLY : l_elev_absolute_height,z_land_ij, z_land_io,          &
                        z_land_land, surf_hgt_surft, surf_hgt_band

  USE ancil_info, ONLY : nsurft, land_pts, land_index

  USE theta_field_sizes, ONLY: t_i_length

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the elevation of the forcing data elevation if any tile has
!   an absolute height above sea-level.
!   Tile heights are set to be spatially invarient.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
  INTEGER :: error  ! Error indicator

!-----------------------------------------------------------------------------
! Definition of the jules_z_land namelist
!-----------------------------------------------------------------------------
  LOGICAL :: use_file
                        !   T - the variable uses the file
                        !   F - the variable is set using a constant value

  CHARACTER(LEN=max_file_name_len) :: file  ! If input grid has more than one
                                            ! point, read heights from this
                                            ! file
  CHARACTER(LEN=max_sdf_name_len) :: z_land_name    ! Use the variable with
                                                    ! this name for forcing
                                                    ! altitude
  CHARACTER(LEN=errormessagelength) :: iomessage

  NAMELIST /jules_z_land/ use_file, file, z_land_io, z_land_name,             &
                          surf_hgt_band

  INTEGER :: i, j, l, n

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  use_file = .TRUE.      ! Default is for every variable to be read from file
  file        = ''       ! Empty file name
  z_land_name = 'z_land' ! Default variable name

!-----------------------------------------------------------------------------
! If some tiles have absolute heights then the gridbox mean height must also
! be provided. For a single point, this can come from the namelist, otherwise
! we need to use the file option
!-----------------------------------------------------------------------------
  z_land_ij(:,:) = 0.0

  IF ( ANY(l_elev_absolute_height) ) THEN

!-----------------------------------------------------------------------------
!   Read namelist
!-----------------------------------------------------------------------------
    CALL log_info("init_z_land", "Reading JULES_Z_LAND namelist...")

!   First, we read the namelist
    READ(namelist_unit, nml=jules_z_land, IOSTAT=error, IOMSG=iomessage)
    IF ( error /= 0 )                                                         &
      CALL log_fatal("init_z_land",                                           &
                   "Error reading namelist JULES_Z_LAND " //                  &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
!   Set values derived from namelist and verify for consistency
!-----------------------------------------------------------------------------
    CALL log_info("init_z_land","Some tiles have  " //                        &
           "absolute heights above sea-level, see l_elev_absolute_height " // &
           "for which. Where l_elev_absolute_height is false, surf_hgt "   // &
           "offsets can only be applied as global to that tile "           // &
           "type (usually these are 0, indicating no offset from "         // &
           "the gridbox mean).")

!-----------------------------------------------------------------------------
!   Set the heights (relative or absolute) to be constant across a domain.
!-----------------------------------------------------------------------------
    DO n=1,nsurft
      surf_hgt_surft(:,n) = surf_hgt_band(n)
    END DO

!-----------------------------------------------------------------------------
!   If we have a grid, set gridbox mean heights from the specified file
!-----------------------------------------------------------------------------
    IF ( use_file ) THEN

      CALL log_info("init_z_land",                                            &
                    "Data is on a grid - reading z_land from file " //        &
                    TRIM(file))

      ALLOCATE(z_land_land(land_pts), STAT=error)

      IF ( error > 0 )                                                        &
        CALL log_fatal("init_z_land", "Error allocating z_land")

!     Check that a file name was provided
      IF ( LEN_TRIM(file) == 0 )                                              &
        CALL log_fatal("init_z_land", "No file name provided for gridbox " // &
                       "mean heights")

      CALL fill_variables_from_file(file,                                     &
                                    (/ 'z_land_land' /), (/ z_land_name /))

      DO l = 1,land_pts
        j = ( land_index(l) - 1 ) / t_i_length + 1
        i = land_index(l) - (j - 1) * t_i_length
        z_land_ij(i,j) = z_land_land(l)
      END DO

      DEALLOCATE(z_land_land)

!-----------------------------------------------------------------------------
!     If we are reading data at a single point, read height from the namelist
!-----------------------------------------------------------------------------
    ELSE

      CALL log_info("init_z_land",                                            &
                    "Data is at a single point - reading z_land from " //     &
                    "z_land_io in namelist JULES_Z_LAND")
      z_land_ij(1,1) = z_land_io

!-----------------------------------------------------------------------------
!     Check that a value for the gridbox height has been set
!-----------------------------------------------------------------------------
      IF ( z_land_io == rmdi ) &
        CALL log_fatal("init_z_land", "Some tiles have absolute "   //        &
                       "heights above sea-level but no value for "  //        &
                       "z_land has been provided. Set a value for " //        &
                       "z_land in the JULES_Z_LAND namelist")

!-----------------------------------------------------------------------------
!     Check that a value for the elevation bands have been set
!-----------------------------------------------------------------------------
      IF ( ANY(surf_hgt_band(1:nsurft)  == rmdi)  )                           &
        CALL log_fatal("init_z_land", "Some tiles have absolute "   //        &
                       "heights above sea-level but no values for " //        &
                       "elevation bands have been provided. Set a " //        &
                       "value for surf_hgt_band in the "//                    &
                       "JULES_Z_LAND namelist")

    END IF

  END IF

  RETURN

END SUBROUTINE init_z_land
#endif
