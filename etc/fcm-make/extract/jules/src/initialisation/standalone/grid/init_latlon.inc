#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_latlon()

  USE io_constants, ONLY : mdi, max_sdf_name_len, max_file_name_len, namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE model_grid_mod, ONLY : grid_lat => latitude, grid_lon => longitude

  USE input_mod, ONLY : input_grid => grid, fill_variables_from_file
  
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the latitude and longitude
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
  INTEGER :: error, error_sum  ! Error indicators

!-----------------------------------------------------------------------------
! Definition of the jules_latlon namelist
! For data at a single point, a single latitude and longitude are specified in
! the namelist
! In all other cases, latitude and longitude are read from a file
!-----------------------------------------------------------------------------
  REAL :: latitude, longitude  ! Single point latitude and longitude
                               ! Initialised to non-sensical values below so
                               ! we can tell when they have been set
  CHARACTER(LEN=max_file_name_len) :: file
                               ! The file to read lat and lon from
  CHARACTER(LEN=max_sdf_name_len) :: lat_name, lon_name
                               ! The names of lat and lon in the file
  CHARACTER(LEN=errormessagelength) :: iomessage
  NAMELIST /jules_latlon/ latitude, longitude, file, lat_name, lon_name


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  latitude  = mdi
  longitude = mdi
  file      = ''  !  Empty file name.
  lat_name  = ''  !  Empty variable name.
  lon_name  = ''  !  Empty variable name.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_latlon", "Reading JULES_LATLON namelist...")

! First, we read the namelist
  READ(namelist_unit, nml=jules_latlon, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_latlon",                                             &
                   "Error reading namelist JULES_LATLON " //                  &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Set values derived from namelist and verify for consistency
!-----------------------------------------------------------------------------
  CALL log_info("init_latlon",                                                &
                "Getting latitude and longitude for the full input grid...")

! First, allocate the model lat and lon arrays to the full input grid (for now)
  error_sum = 0
  ALLOCATE(grid_lat(input_grid%nx,input_grid%ny), STAT=error)
  error_sum = error_sum + error
  ALLOCATE(grid_lon(input_grid%nx,input_grid%ny), STAT=error)
  error_sum = error_sum + error
  IF ( error_sum > 0 )                                                        &
    CALL log_fatal("init_latlon",                                             &
                   "Error allocating arrays for latitude and longitude")

  IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
!-----------------------------------------------------------------------------
! If we have data at a single point, read lat and lon from the namelist
!-----------------------------------------------------------------------------
! Check if the namelist lat and lon have been set
    IF ( ABS(latitude - mdi) < EPSILON(latitude) .OR.                         &
         ABS(longitude - mdi) < EPSILON(latitude) )                           &
      CALL log_fatal("init_latlon",                                           &
                     "For data at a single point, latitude and longitude " // &
                     "are read from the namelist JULES_LATLON")

! Now we know they have been set, copy their values into the model arrays
    CALL log_info("init_latlon",                                              &
                  "Data is at a single point - reading latitude and " //      &
                  "longitude from namelist JULES_LATLON")
    grid_lat(1,1) = latitude
    grid_lon(1,1) = longitude
  ELSE
!-----------------------------------------------------------------------------
! If we have a grid, set lat and lon from the specified file
!-----------------------------------------------------------------------------
    CALL log_info("init_latlon",                                              &
                  "Data is on a grid - reading latitude and longitude " //    &
                  "from file " // TRIM(file))
!   Check that a file name was provided.
    IF ( LEN_TRIM(file) == 0 )                                                &
      CALL log_fatal("init_latlon", "No file name provided")

    CALL fill_variables_from_file(file, (/ 'latitude ', 'longitude' /),       &
                                        (/  lat_name  ,  lon_name   /))
  END IF

!-----------------------------------------------------------------------------
! Check that the values seem sensible
!-----------------------------------------------------------------------------
  IF ( ANY(grid_lat < -90.0) .OR. ANY(grid_lat > 90.0) )                      &
    CALL log_fatal("init_latlon",                                             &
                   "Latitude is out of range - allowed range is -90.0 to " // &
                   "90.0, given range is " //                                 &
                   TRIM(to_string(MINVAL(grid_lat))) // " to " //             &
                   TRIM(to_string(MAXVAL(grid_lat))))

  IF ( ANY(grid_lon < -180.0) .OR. ANY(grid_lon > 360.0) )                    &
    CALL log_fatal("init_latlon",                                             &
                   "Longitude is out of range - allowed range is -180.0 " //  &
                   "to 360.0, given range is " //                             &
                   TRIM(to_string(MINVAL(grid_lon))) // " to " //             &
                   TRIM(to_string(MAXVAL(grid_lon))))

  RETURN

END SUBROUTINE init_latlon
#endif
