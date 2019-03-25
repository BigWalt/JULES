










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE spinup_mod

USE model_interface_mod, ONLY: identifier_len

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: max_spinup_vars = 4

!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE spinup_field

  CHARACTER(LEN=identifier_len) :: identifier
                 ! The identifier of the model variable associated with this
                 ! spinup field

  LOGICAL :: use_percent = .FALSE.  ! T - tolerance is specified in %
                                    ! F - tolerance is specified as an
                                    !     absolute value
  REAL :: tolerance = 0.0  ! The tolerance to use for this field

  REAL, POINTER :: DATA(:,:,:) => NULL()  ! The data from the last comparison

END TYPE spinup_field

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
INTEGER :: nvars = 0
TYPE(spinup_field), SAVE :: spinup_vars(max_spinup_vars)


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC max_spinup_vars, nvars, spinup_vars,                                 &
       spinup_init, spinup_check


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE spinup_init()

  USE ancil_info, ONLY :                                                      &
    ! imported scalars (IN)
    dim_cslayer, land_pts, nsoilt

  USE jules_soil_biogeochem_mod, ONLY :                                       &
    ! imported scalar parameters
    soil_model_ecosse, soil_model_rothc,                                      &
    ! imported scalars (IN)
    soil_bgc_model
     

  USE jules_soil_mod, ONLY :                                                  &
    ! imported scalars (IN)
    sm_levels

  USE jules_vegetation_mod, ONLY :                                            &
    ! imported scalars (IN)
    l_triffid

  USE prognostics, ONLY :                                                     &
    ! imported arrays (IN)
    cs_pool_soilt, smcl_soilt, t_soil_soilt

  USE trifctl, ONLY :                                                         &
    ! imported arrays (IN)
    cv_gb

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Allocates variables required for spinup and stores initial state
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local parameters
  CHARACTER(LEN=*), PARAMETER :: RoutineName = 'spinup_init'

! Work variables
  INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Allocate space to store the state of the spinup variables for comparison
!-----------------------------------------------------------------------------
  DO i = 1,nvars

    !-------------------------------------------------------------------------
    ! Variables that are available in all configurations.
    !-------------------------------------------------------------------------
    SELECT CASE ( spinup_vars(i)%identifier )
      CASE ( 'smcl' )
        ALLOCATE(spinup_vars(i)%data(land_pts,nsoilt,sm_levels))
        spinup_vars(i)%data(:,:,:) = smcl_soilt(:,:,:)

      CASE ( 't_soil' )
        ALLOCATE(spinup_vars(i)%data(land_pts,nsoilt,sm_levels))
        spinup_vars(i)%data(:,:,:) = t_soil_soilt(:,:,:)

    !-------------------------------------------------------------------------
    ! Variables that are only available with TRIFFID.
    !-------------------------------------------------------------------------
      CASE ( 'c_veg' )
        IF ( .NOT. l_triffid ) THEN
          CALL log_fatal( RoutineName,                                        &
                          TRIM(spinup_vars(i)%identifier)               //    &
                          " can only be used to determine spin up if"   //    &
                          " l_triffid=.TRUE." )
        END IF
        ALLOCATE( spinup_vars(i)%data(land_pts,1,1) )
        spinup_vars(i)%data(:,1,1) = cv_gb(:)

     !------------------------------------------------------------------------
     ! Variables that are only available with RothC or ECOSSE.
     !------------------------------------------------------------------------
      CASE ( 'c_soil' )
        ! Total C in each layer.
        SELECT CASE ( soil_bgc_model )
          CASE ( soil_model_ecosse, soil_model_rothc )
            ! OK. Nothing to do.
          CASE default
            CALL log_fatal( RoutineName,                                      &
                            TRIM(spinup_vars(i)%identifier)             //    &
                            " can only be used to determine spin up if" //    &
                            " the RothC or ECOSSE model is used." )
        END SELECT
        ALLOCATE(spinup_vars(i)%data(land_pts,nsoilt,dim_cslayer))
        ! Sum the C pools in each layer.
        spinup_vars(i)%data(:,:,:) = SUM( cs_pool_soilt(:,:,:,:), 4 )

      CASE DEFAULT
        CALL log_fatal(RoutineName,                                           &
                       "Unrecognised variable for spinup - " //               &
                       TRIM(spinup_vars(i)%identifier))
    END SELECT
  END DO

  RETURN

END SUBROUTINE spinup_init
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


LOGICAL FUNCTION spinup_check()

  USE mpi

  USE string_utils_mod, ONLY : to_string

  USE prognostics, ONLY :                                                     &
    ! imported arrays (IN)
    cs_pool_soilt, smcl_soilt, t_soil_soilt

  USE trifctl, ONLY :                                                         &
    ! imported arrays (IN)
    cv_gb

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks if spinup was successful for the whole model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Local parameters
  CHARACTER(LEN=*), PARAMETER :: RoutineName = 'spinup_check'

! Work variables
  INTEGER :: data_shape(3)
    ! The shape of the data.
  REAL, ALLOCATABLE :: data(:,:,:)
    ! The data we will be checking against.
  REAL, ALLOCATABLE :: tol(:,:,:)
    ! The actual tolerance we will be using for each element of the data
    ! (after % tolerances have been resolved).
  LOGICAL, ALLOCATABLE :: passed(:,:,:)
    ! Mask that will be .TRUE. at points that passed for the current variable,
    ! and .FALSE. otherwise.
  LOGICAL, ALLOCATABLE :: passed_pts(:)
    ! Passed collapsed down onto one dimension.

  LOGICAL :: local_check  ! Indicates if the current task has passed it's
                          ! local spinup

  INTEGER :: i,l  ! Loop counter

  INTEGER :: error  ! MPI error indicator
                    ! NOTE that this is not checked since the default
                    !      behaviour of most (all?) MPI implementations is
                    !      to abort on error

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Note that each task does its own spinup check on the points that it owns.
! The results from each task are then collected, and spinup for the model as
! a whole is judged to have passed if all the tasks have passed
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Work out if the current task passes spinup
!-----------------------------------------------------------------------------
! Assume we will pass until a variable fails
  local_check = .TRUE.

! Loop over each variable and check if that variable passes spinup
  DO i = 1,nvars

    ! First, allocate the data and tolerance arrays for this variable
    data_shape(:) = SHAPE(spinup_vars(i)%data)
    ALLOCATE(data(data_shape(1), data_shape(2), data_shape(3)))
    ALLOCATE(tol(data_shape(1), data_shape(2), data_shape(3)))
    ALLOCATE(passed(data_shape(1), data_shape(2), data_shape(3)))
    ALLOCATE(passed_pts(SIZE(passed, 1)))

    ! Fill the data array based on the variable
    SELECT CASE ( spinup_vars(i)%identifier )

      CASE ( 'c_soil' )
        ! Sum the C pools in each layer.
        data(:,:,:) = SUM( cs_pool_soilt(:,:,:,:), 4 )

      CASE ( 'c_veg' )
        data(:,1,1) = cv_gb(:)

      CASE ( 'smcl' )
        data(:,:,:) = smcl_soilt(:,:,:)

      CASE ( 't_soil' )
        data(:,:,:) = t_soil_soilt(:,:,:)

      CASE DEFAULT
        CALL log_fatal(RoutineName,                                           &
                       "Unrecognised variable for spinup - " //               &
                       TRIM(spinup_vars(i)%identifier))
    END SELECT

    ! Work out what tolerance we will use for each element
    IF ( spinup_vars(i)%use_percent ) THEN
      tol(:,:,:) = ABS(data(:,:,:)) * spinup_vars(i)%tolerance / 100.0
    ELSE
      tol(:,:,:) = spinup_vars(i)%tolerance
    END IF

    ! Indicate a failure of spinup if one has occured
    passed(:,:,:) = ABS(spinup_vars(i)%data(:,:,:) - data(:,:,:)) <= tol(:,:,:)
    ! Collapse the vertical levels into one value per point only
    passed_pts(:) = (/ ( ALL(passed(l,:,:)), l = 1,SIZE(passed_pts) ) /)
    IF ( .NOT. ALL(passed_pts) ) THEN
      local_check = .FALSE.
      CALL log_info("spinup_check",                                           &
                    "Variable '" // TRIM(spinup_vars(i)%identifier) //        &
                    "' has not spunup at " //                                 &
                    TRIM(to_string(COUNT(.NOT. passed_pts))) // " points")
    END IF

    ! Store the data for next time we need to check
    spinup_vars(i)%data(:,:,:) = data(:,:,:)

    DEALLOCATE(data)
    DEALLOCATE(tol)
    DEALLOCATE(passed)
    DEALLOCATE(passed_pts)
  END DO

!-----------------------------------------------------------------------------
! Exchange results with the other tasks
!
! Spinup passes only if all tasks have passed, so a reduction using the .AND.
! operation gives us what we want
!-----------------------------------------------------------------------------
  CALL mpi_allreduce(                                                         &
    local_check, spinup_check, 1, mpi_logical, mpi_land, mpi_comm_world, error&
  )

  RETURN

END FUNCTION spinup_check

END MODULE spinup_mod
