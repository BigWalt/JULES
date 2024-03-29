#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE input_mod

USE io_constants, ONLY: max_sdf_name_len

USE grid_utils_mod, ONLY: grid_info, subgrid_info

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   The module contains information about the grid and levels dimensions
!   to use for input, and routines for reading non-time-varying input
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Definition of the input grid
!
! gfortran requires the SAVE attribute on the grid objects, and it makes
! no difference to other compilers
!-----------------------------------------------------------------------------
TYPE(grid_info), SAVE :: grid  ! The input grid definition

LOGICAL :: use_subgrid = .FALSE.
    ! T => the model grid is a subset of the input grid
    ! F => the model grid is the input grid

TYPE(subgrid_info), SAVE :: subgrid  ! If use_subgrid=T, this describes the
                                     ! subgrid to extract

!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                      &
! Grid definition variables
    grid, use_subgrid, subgrid,                                               &
! Routines
    fill_variables_from_file


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "fill_variables_from_file.inc"

END MODULE input_mod
#endif
