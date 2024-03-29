#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE model_grid_mod

USE grid_utils_mod, ONLY: grid_info

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Variables for the sizes of the full grid
!-----------------------------------------------------------------------------
TYPE(grid_info), SAVE :: model_grid  ! The full model grid (i.e. combined grid
                                     ! for all MPI tasks)
                                     ! gfortran requires the SAVE attribute

INTEGER :: global_land_pts  ! The number of land points in the full model
                            ! grid

!-----------------------------------------------------------------------------
! Variables defined on the FULL model grid (i.e. all tasks combined)
!-----------------------------------------------------------------------------
LOGICAL, ALLOCATABLE :: global_land_mask(:,:)
                                      ! Land mask for all points in the
                                      ! FULL model grid (i.e. all tasks
                                      ! combined)

!-----------------------------------------------------------------------------
! Variables defined on only the points modelled by the current task
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE :: latitude(:,:)  ! The latitude of model points for the
                                    ! current task
REAL, ALLOCATABLE :: longitude(:,:) ! The longitude of model points for the
                                    ! current task

REAL, ALLOCATABLE :: latitude_of_land_pts(:)
  ! The latitude of model land points for the current task
REAL, ALLOCATABLE :: longitude_of_land_pts(:)
  ! The longitude of model land points for the current task

END MODULE model_grid_mod
#endif
