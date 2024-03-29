#if !defined(UM_JULES)
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
#endif
