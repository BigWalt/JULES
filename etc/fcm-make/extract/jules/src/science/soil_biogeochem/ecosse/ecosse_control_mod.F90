#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE ecosse_control_mod

!-----------------------------------------------------------------------------
! Description:
!   Control-level code for the ECOSSE model of soil C and N.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC ecosse_control

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ECOSSE_CONTROL_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE ecosse_control( land_pts, triffid_call, vs_pts,                    &
                           vs_index, n_amm_arg, n_nit_arg, frac_surft_start )

USE ancil_info, ONLY:                                                         &
  ! imported scalars
  dim_cs1, nsoilt, nz_soilc=>dim_cslayer

USE ecosse_decomposition_mod, ONLY:                                           &
  ! imported procedures
  calc_soil_decomposition

USE ecosse_prepare_mod, ONLY:                                                 &
  ! imported procedure
  ecosse_prepare

USE ecosse_source_sink_mod, ONLY:                                             &
  ! imported procedure
  ecosse_source_sink

USE ecosse_utils_mod, ONLY:                                                   &
  ! imported parameters
  nt,                                                                         &
  ! imported procedures
  adjust_soil

USE ereport_mod, ONLY:                                                        &
  ! imported procedures
  ereport

USE jules_soil_ecosse_mod, ONLY:                                              &
  ! imported scalars
  dt_soilc, l_soil_N

USE jules_surface_types_mod, ONLY:                                            &
  ! imported scalars
  npft, ntype

USE prognostics, ONLY:                                                        &
  ! imported arrays
  cs_pool_soilt, ns_pool_gb

USE p_s_parms, ONLY:                                                          &
  ! imported arrays
  soil_ph_soilt

USE soil_ecosse_vars_mod, ONLY:                                               &
  ! imported arrays
  co2_soil_gb

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
    ! Number of land points. 
  triffid_call,                                                               &
    ! Indicates if the dynamic vegetation model was called earlier in
    ! the current timestep.
    ! 0 = TRIFFID was called.
    ! 1 = TRIFFID was not called.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  vs_index(land_pts) ! Indices of points with veg and/or soil.

REAL, INTENT(IN) ::                                                           &
  frac_surft_start(land_pts,ntype)
    ! Fractional coverage of surface types at start of timestep.

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL, INTENT(INOUT) ::                                                        &
  n_amm_arg(land_pts,nz_soilc),                                               &
    ! N in ammonium in a soil layer (kg m-2).
  n_nit_arg(land_pts,nz_soilc) 
    ! N in nitrate in a soil layer (kg m-2).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: s = 1
    ! Soil tile number. ECOSSE is coded assuming a single soil tile. Extension
    ! to multiple soil tiles would require at least a loop over the tiles and
    ! plant fluxes (e.g. litterfall input, N uptake) consistent with the
    ! tiling.

LOGICAL, PARAMETER ::                                                         &
  l_init_false = .FALSE.
    ! For use as an argument to subroutine adjust_soil, to indicate to that
    ! routine that it is being called during the main body of the run and not
    ! during initialisation.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  errorstatus

REAL ::                                                                       &
  convert_veg_input,                                                          &
    ! Timestep for plant inputs as a fraction of veg model timestep.
  dt_input
    ! Timestep length for litter input and N uptake.

LOGICAL ::                                                                    &
  l_add_plant_inputs,                                                         &
    ! Flag indicating if plant inputs are to be added this timestep.
  l_extract_n
    ! Flag indicating if plant uptake of N is removed from soil this
    ! timestep. Also used for addition of fixation with TRIFFID-based model.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL ::                                                                       &
  !---------------------------------------------------------------------------
  ! Prognostic variables.
  ! These are copies of the multi-pool prognostic variables, but each pool
  ! is held in a separate variable.
  !---------------------------------------------------------------------------
  c_bio(land_pts,nz_soilc,nt),                                                &
    ! C in soil biomass (kg m-2).
  c_dpm(land_pts,nz_soilc,nt),                                                &
    ! C in decomposable plant material (kg m-2).
  c_hum(land_pts,nz_soilc,nt),                                                &
    ! C in soil humus (kg m-2).
  c_rpm(land_pts,nz_soilc,nt),                                                &
    ! C in resistant plant material (kg m-2).
  n_amm(land_pts,nz_soilc,nt),                                                &
    ! N in ammonium (kg m-2).
  n_bio(land_pts,nz_soilc,nt),                                                &
    ! N in soil biomass (kg m-2).
  n_dpm(land_pts,nz_soilc,nt),                                                &
    ! N in decomposable plant material (kg m-2).
  n_hum(land_pts,nz_soilc,nt),                                                &
    ! N in soil humus (kg m-2).
  n_nit(land_pts,nz_soilc,nt),                                                &
    ! N in nitrate (kg m-2).
  n_rpm(land_pts,nz_soilc,nt),                                                &
    ! N in resistant plant material (kg m-2).
  !---------------------------------------------------------------------------
  ! "Physical" variables that JULES provides on soil moisture levels and are
  ! transformed onto the soil C levels.
  !---------------------------------------------------------------------------
  smvc(land_pts,nz_soilc),                                                    &
    ! Volumetric soil moisture content (1).
  sm_total(land_pts,nz_soilc),                                                &
    ! Volumetric soil moisture content, as a fraction of saturation.
  sm_unfrozen(land_pts,nz_soilc),                                             &
    ! Unfrozen volumetric soil moisture content, as a fraction of saturation
  sm_fc(land_pts,nz_soilc),                                                   &
    ! Volumetric soil moisture content at field capacity,
    ! as a fraction of saturation.
  sm_one_bar(land_pts,nz_soilc),                                              &
    ! Volumetric soil moisture content when suction pressure is
    ! -100kPa (-1 bar), as a fraction of saturation.
  sm_wilt(land_pts,nz_soilc),                                                 &
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.
  soilT_degC(land_pts,nz_soilc),                                              &
    ! Soil temperature in ECOSSE layers (degC).
  water_flux_down(land_pts,0:nz_soilc),                                       &
    ! Downward water flux at bottom of each layer (kg m-2 s-1).
  water_flux_lateral(land_pts,nz_soilc),                                      &
    ! Lateral water flux from each soil layer (kg m-2 s-1).
  !---------------------------------------------------------------------------
  ! Plant inputs of C and N.
  !---------------------------------------------------------------------------
  plant_input_c_dpm(land_pts,nz_soilc),                                       &
    ! Carbon added to DPM pool by plant inputs (kg m-2).
  plant_input_c_rpm(land_pts,nz_soilc),                                       &
    ! Carbon added to RPM pool by plant inputs (kg m-2).
  plant_input_n_dpm(land_pts,nz_soilc),                                       &
    ! Nitrogen added to DPM pool by plant inputs (kg m-2).
  plant_input_n_rpm(land_pts,nz_soilc),                                       &
    ! Nitrogen added to RPM pool by plant inputs (kg m-2).
  !---------------------------------------------------------------------------
  ! Other variables.
  !---------------------------------------------------------------------------
  co2_from_decomp(land_pts,nz_soilc),                                         &
    ! CO2 emitted from soil decomposition, expressed as carbon (kg m-2).
  biohum_nc(land_pts,nz_soilc),                                               &
    ! Stable N:C ratio of biomass and humus pools
  n2o_full_nitrif_gb(land_pts),                                               &
    !  N in N2O lost by (full) nitrification (kg m-2 s-1).
  residual_n(land_pts,nz_soilc),                                              &
    ! Minimum-allowed (residual) inorganic N amount (kg m-2).
  resp_frac_soil(land_pts,nz_soilc),                                          &
    ! The fraction of decomposition that forms new biomass and humus, i.e.
    ! the fraction that remains in the soil.
  f_root_pft(npft,nz_soilc),                                                  &
    ! Fraction of roots in each soil layer.
  veg_cover(land_pts)
    ! Indicator of vegetation coverage (0 to 1).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ECOSSE_CONTROL'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! ECOSSE is coded assuming a single soil tile.
!-----------------------------------------------------------------------------
IF ( nsoilt > 1 ) THEN
  errorstatus = 101
  CALL ereport( TRIM(RoutineName), errorstatus,                               &
                "ECOSSE is only coded for a single soil tile." )
END IF

!-----------------------------------------------------------------------------
! Copy prognostic variables into pool-specific variables.
! Fill all time levels with the initial value.
! Use SPREAD to put nt copies of a slice of the prognostic into the 3rd
! dimension (time) of the pool-specific variable.
!-----------------------------------------------------------------------------
c_dpm(:,:,:) = SPREAD( cs_pool_soilt(:,s,:,1), 3, nt )
c_rpm(:,:,:) = SPREAD( cs_pool_soilt(:,s,:,2), 3, nt )
c_bio(:,:,:) = SPREAD( cs_pool_soilt(:,s,:,3), 3, nt )
c_hum(:,:,:) = SPREAD( cs_pool_soilt(:,s,:,4), 3, nt )

IF ( l_soil_N ) THEN
  n_dpm(:,:,:) = SPREAD( ns_pool_gb(:,:,1), 3, nt )
  n_rpm(:,:,:) = SPREAD( ns_pool_gb(:,:,2), 3, nt )
  n_bio(:,:,:) = SPREAD( ns_pool_gb(:,:,3), 3, nt )
  n_hum(:,:,:) = SPREAD( ns_pool_gb(:,:,4), 3, nt )
  n_amm(:,:,:) = SPREAD( n_amm_arg(:,:), 3, nt )
  n_nit(:,:,:) = SPREAD( n_nit_arg(:,:), 3, nt )
END IF

!-----------------------------------------------------------------------------
! Prepare for the call to ECOSSE. This includes doing things such as putting
! variables from soil moisture layers onto ECOSSE layers.
!-----------------------------------------------------------------------------
CALL ecosse_prepare( land_pts, s, triffid_call, vs_pts, vs_index,             &
                     frac_surft_start, convert_veg_input, dt_input,           &
                     l_add_plant_inputs, l_extract_n,                         &
                     biohum_nc, residual_n, resp_frac_soil, f_root_pft,       &
                     smvc, sm_total, sm_unfrozen,                             &
                     sm_fc, sm_one_bar, sm_wilt, soilT_degC,                  &
                     water_flux_down, water_flux_lateral, veg_cover,          &
                     plant_input_c_dpm, plant_input_c_rpm,                    &
                     plant_input_n_dpm, plant_input_n_rpm )

!-----------------------------------------------------------------------------
! Calculate inputs (e.g. from plants and fertiliser) and outputs (e.g.
! uptake by plants).
!-----------------------------------------------------------------------------
CALL ecosse_source_sink( land_pts, vs_pts, convert_veg_input,                 &
                         l_add_plant_inputs, vs_index,                        &
                         plant_input_c_dpm, plant_input_c_rpm,                &
                         plant_input_n_dpm, plant_input_n_rpm,                &
                         c_dpm, c_rpm,  n_dpm, n_rpm )

!-----------------------------------------------------------------------------
! Calculate decomposition and mineralisation.
!-----------------------------------------------------------------------------
CALL calc_soil_decomposition( land_pts, vs_pts, vs_index,                     &
          biohum_nc, residual_n, resp_frac_soil,                              &
          sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                            &
          soil_pH_soilt(:,s,:), soilT_degC, veg_cover,                        &
          c_bio, c_hum, c_dpm, c_rpm,                                         &
          n_bio, n_hum, n_dpm, n_rpm, n_nit, n_amm,                           &
          co2_from_decomp )

!-----------------------------------------------------------------------------
! The rest of the 'science' code will be added here in future.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Get gridbox CO2 flux.
!-----------------------------------------------------------------------------
co2_soil_gb(:) = SUM( co2_from_decomp(:,:), 2 ) / dt_soilc

!-----------------------------------------------------------------------------
! Adjust updated pools to ensure minimum store sizes and reasonable C:N.
!-----------------------------------------------------------------------------
CALL adjust_soil( land_pts, vs_pts, l_init_false, vs_index, biohum_nc,        &
                  c_dpm(:,:,nt), c_rpm(:,:,nt), c_bio(:,:,nt), c_hum(:,:,nt), &
                  n_dpm(:,:,nt), n_rpm(:,:,nt), n_bio(:,:,nt), n_hum(:,:,nt), &
                  co2_soil_gb )

!-----------------------------------------------------------------------------
! Copy updated local prognostic variables back into "main" variables.
!-----------------------------------------------------------------------------
cs_pool_soilt(:,s,:,1) = c_dpm(:,:,nt)
cs_pool_soilt(:,s,:,2) = c_rpm(:,:,nt)
cs_pool_soilt(:,s,:,3) = c_bio(:,:,nt)
cs_pool_soilt(:,s,:,4) = c_hum(:,:,nt)

IF ( l_soil_N ) THEN
  ns_pool_gb(:,:,1) = n_dpm(:,:,nt)
  ns_pool_gb(:,:,2) = n_rpm(:,:,nt)
  ns_pool_gb(:,:,3) = n_bio(:,:,nt)
  ns_pool_gb(:,:,4) = n_hum(:,:,nt)
  n_amm_arg(:,:)    = n_amm(:,:,nt)
  n_nit_arg(:,:)    = n_nit(:,:,nt)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE ecosse_control

!#############################################################################
!#############################################################################

END MODULE ecosse_control_mod
#endif

