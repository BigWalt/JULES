#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION map_from_land(cube_land) RESULT(cube_grid)

  USE io_constants, ONLY : mdi

  USE data_cube_mod, ONLY : data_cube, cube_create

  USE ancil_info, ONLY : land_pts, land_index

  USE theta_field_sizes, ONLY : t_i_length, t_j_length

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a data cube on land points only and maps it onto the full model grid
!   Only sets values at the land points - other values are left untouched
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(data_cube), INTENT(IN) :: cube_land  ! The land point data to map to the
                                            ! full model grid
                                            ! It is assumed that the cube will
                                            ! have a 'land points' dimension
                                            ! with all other dimensions being
                                            ! levels dimensions
                                            ! Vertical levels are preserved in
                                            ! the mapping


! Return type
  TYPE(data_cube) :: cube_grid  ! The land data mapped onto the full model
                                ! grid
                                ! This cube will have two grid dimensions plus
                                ! the levels dimensions from data_land


! Work variables
  REAL, ALLOCATABLE :: data_land(:,:), data_grid(:,:,:)
                                ! Variables to hold the data that use a
                                ! 'collapsed levels dimension', i.e. all the
                                ! vertical levels dimensions are represented
                                ! by one dimension

  INTEGER :: nlevs  ! The size of the combined levels dimension

  INTEGER :: i, j, l  ! Indexing variables


!-----------------------------------------------------------------------------


  IF ( cube_land%shape(1) /= land_pts )                                       &
    CALL log_fatal("map_from_land", "Input data must be on land points")

! Allocate the data_land and data_grid arrays
  nlevs = PRODUCT(cube_land%shape(2:))
  ALLOCATE(data_land(land_pts, nlevs))
  ALLOCATE(data_grid(t_i_length, t_j_length, nlevs))

! Copy the data from the cube into the data array
! Note that we can't use cube_get_data as our array has the combined z dimension
  data_land(:,:) = RESHAPE(cube_land%values, SHAPE(data_land))

! Initialise data_grid to missing data - this is the value it will take at
! non-land points
  data_grid(:,:,:) = mdi

! Do the mapping
  DO l = 1,land_pts
    j = (land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j - 1) * t_i_length

    data_grid(i,j,:) = data_land(l,:)
  END DO

! Copy the gridded data into the return cube
  cube_grid = cube_create((/ t_i_length, t_j_length, cube_land%shape(2:) /))
  cube_grid%values(:) = RESHAPE(data_grid, (/ SIZE(data_grid) /))

! Deallocate the arrays
  DEALLOCATE(data_land)
  DEALLOCATE(data_grid)

  RETURN

END FUNCTION map_from_land
#endif
