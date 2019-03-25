










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE dump_mod

USE io_constants, ONLY: format_len, format_ascii, format_ncdf,             &
                         max_sdf_name_len

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

INTEGER, PARAMETER :: max_dim_dump = 13
INTEGER, PARAMETER :: max_var_dump = 80

CHARACTER(LEN=max_sdf_name_len), PARAMETER ::                               &
  land_dim_name    = "land",                                                &
  pft_dim_name     = "pft",                                                 &
  cpft_dim_name    = "cpft",                                                &
  sc_pool_dim_name = "scpool",                                              &
  sc_layer_dim_name = "sclayer",                                            &
  snow_dim_name    = "snow",                                                &
  soil_dim_name    = "soil",                                                &
  tile_dim_name    = "tile",                                                &
  type_dim_name    = "type",                                                &
  soilt_dim_name   = "soilt",                                               &
  scalar_dim_name  = "scalar",                                              &
  nolevs_dim_name  = "olevs",                                               &
  nfarray_dim_name = "nfarray",                                             &
  seed_dim_name    = "seed",                                                &
  bedrock_dim_name = "bedrock",                                             &
  p_rivers_dim_name = "p_rivers"

CHARACTER(LEN=format_len), PARAMETER :: dump_format = format_ncdf

  !Create a defined type to store the flags for which ancils are read
  !from the dump file
TYPE ancil_flags
  LOGICAL :: frac
  LOGICAL :: soil_props
  LOGICAL :: top
  LOGICAL :: agric
  LOGICAL :: crop_props
  LOGICAL :: irrig
  LOGICAL :: rivers_props
  LOGICAL :: co2
END TYPE

TYPE(ancil_flags) :: ancil_dump_read

!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC  max_var_dump, required_vars_for_configuration,                      &
        read_dump, write_dump,                                              &
        ancil_dump_read

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


SUBROUTINE required_vars_for_configuration(nvars, identifiers,                &
                                           nvars_from_ancil, vars_from_ancil, &
                                           l_output_mode,                     &
                                           total_snow, include_imogen_vars,   &
                                           read_or_write_dump)

  USE update_mod, ONLY : l_imogen, have_prescribed_sthuf

  USE jules_hydrology_mod, ONLY : l_top

  USE jules_radiation_mod, ONLY : l_snow_albedo, l_embedded_snow

  USE jules_soil_biogeochem_mod, ONLY : soil_model_ecosse, soil_model_rothc,  &
                                        soil_bgc_model

  USE jules_soil_ecosse_mod, ONLY : l_soil_N

  USE jules_vegetation_mod, ONLY : l_triffid, l_phenol, l_veg_compete,        &
                                   can_model, l_crop, l_irrig_dmd, l_landuse, &
                                   l_nitrogen, l_prescsow, l_trif_crop

  USE jules_snow_mod, ONLY : nsmax

  USE jules_surface_mod, ONLY : l_elev_land_ice

  USE jules_soil_mod, ONLY : l_bedrock, sm_levels, l_tile_soil

  USE fire_mod,     ONLY : fire_cntl, l_fire

  USE metstats_mod, ONLY : metstats_flag, l_metstats

  USE update_mod, ONLY: l_daily_disagg, precip_disagg_method,                 &
                        prescribed_sthuf_levels

  USE jules_rivers_mod, ONLY : l_rivers, rivers_type

  USE logging_mod, ONLY : log_warn

  USE prognostics, ONLY:                                                      &
    l_broadcast_soilt


  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns the identifiers of the prognostic variables for the current
!   configuration of the model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  INTEGER, INTENT(OUT) :: nvars  ! The number of variables
  CHARACTER(LEN=*), INTENT(OUT) :: identifiers(:)
                                 ! The model identifiers of the required
                                 ! variables

  INTEGER, INTENT(OUT)          :: nvars_from_ancil  !Number of ancil variables
  CHARACTER(LEN=*), INTENT(OUT) :: vars_from_ancil(:)
                                 ! The model identifiers of the required
                                 ! ancil variables

  LOGICAL, INTENT(IN)           :: l_output_mode
                                 ! True if the intention of the call to this
                                 ! routine is to get the variables to output
                                 ! False if for input
                                 ! This enables the broadcasting of non-soil
                                 ! tiled initial conditions for soil tiling
                                 ! runs

  LOGICAL, INTENT(IN), OPTIONAL :: total_snow
                                 ! T - return only the variables required if
                                 !     snow layer values are to be set by the
                                 !     model
                                 ! F - return all snow variables required
                                 !     by the current configuration
  LOGICAL, INTENT(IN), OPTIONAL :: include_imogen_vars
                                 ! T - include IMOGEN prognostics in the
                                 !     returned list if they are needed
                                 ! F - do not include IMOGEN prognostics in
                                 !     the returned list, even if they are
                                 !     needed
  LOGICAL, INTENT(IN), OPTIONAL :: read_or_write_dump
                                 ! T - include all variables that are needed
                                 !     when reading or writing to dump files
                                 ! F - don't include variables that are only
                                 !     inputted or outputted in dump files

! Work variables
  LOGICAL :: total_snow_local  ! Local version of total_snow
                               ! Defaults to FALSE if not present (i.e.
                               ! return all required snow vars)
  LOGICAL :: inc_imogen_local  ! Local version of include_imogen_vars
                               ! Defaults to TRUE if not present, i.e.
                               ! return IMOGEN vars if they are required
  LOGICAL :: read_or_write_dump_local
                               ! Local version of read_or_write_dump
                               ! Defaults to TRUE if not present
  LOGICAL :: l_append_soilt
                               ! Controls whether vars are appended with _soilt
!-----------------------------------------------------------------------------

! Check for presence of optional arguments
  total_snow_local          = .FALSE.
  inc_imogen_local          = .TRUE.
  read_or_write_dump_local  = .TRUE.

! Decide whether we need to append _soilt to variables
l_append_soilt = .FALSE.
IF (l_tile_soil) THEN
  IF (l_output_mode) THEN
    l_append_soilt = .TRUE.
  ELSE
    IF(.NOT. l_broadcast_soilt) THEN
      l_append_soilt = .TRUE.
    END IF
  END IF
END IF

  IF ( PRESENT(total_snow) )                                                  &
    total_snow_local         = total_snow
  IF ( PRESENT(include_imogen_vars) )                                         &
    inc_imogen_local         = include_imogen_vars
  IF ( PRESENT(read_or_write_dump) )                                          &
    read_or_write_dump_local = read_or_write_dump

! First, set up the array with variables that are required with every
! configuration
! Note that for total_snow = T, snow_surft is the only required snow variable
  nvars = 6
  identifiers(1:nvars) = (/ 'canopy    ', 'cs        ', 'gs        ',       &
                            'snow_tile ', 't_soil    ',                     &
                            'tstar_tile' /)

  !Append _soilt for the two relevant variables if appropriate
  IF (l_append_soilt) THEN
    identifiers(2) = TRIM(identifiers(2)) // "_soilt"
    identifiers(5) = TRIM(identifiers(5)) // "_soilt"
  END IF


!-------------------------------------------------------------------------------
! Variables that are needed only if certain options set.
!-----------------------------------------------------------------------------
  IF ( ( .NOT. have_prescribed_sthuf ) .OR.                                   &
       ( SIZE(prescribed_sthuf_levels) < sm_levels) ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'sthuf     '

    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF
  END IF

  IF ( l_phenol ) THEN
! With phenology on, LAI is prognostic for all PFTs
    nvars = nvars + 1
    identifiers(nvars) = 'lai'
  ELSE IF ( l_crop ) THEN
! Otherwise, if the crop scheme is on, LAI is prognostic for crop PFTs only
    nvars = nvars + 1
    identifiers(nvars) = 'croplai'
  END IF

  IF ( l_triffid ) THEN
! With TRIFFID on, canopy height is prognostic for all PFTs
    nvars = nvars + 1
    identifiers(nvars) = 'canht'
  ELSE IF ( l_crop ) THEN
! Otherwise, if the crop scheme is on, canopy height is prognostic for crop
! PFTs only
    nvars = nvars + 1
    identifiers(nvars) = 'cropcanht'
  END IF

  IF ( l_triffid .AND. l_landuse ) THEN
! With TRIFFID on,  agricultural frac and WP pools are required for LUC
    nvars = nvars + 1
    identifiers(nvars) = 'frac_agr_prev'
    nvars = nvars + 1
    identifiers(nvars) = 'wood_prod_fast'
    nvars = nvars + 1
    identifiers(nvars) = 'wood_prod_med'
    nvars = nvars + 1
    identifiers(nvars) = 'wood_prod_slow'
    IF (l_trif_crop) THEN
      nvars = nvars + 1
      identifiers(nvars) = 'frac_past_prev' 
    END IF
  END IF

  IF ( soil_bgc_model == soil_model_rothc .AND. l_nitrogen ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'n_inorg'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

    nvars = nvars + 1
    identifiers(nvars) = 'ns'
  END IF

  IF ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'ns'

    nvars = nvars + 1
    identifiers(nvars) = 'n_amm'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

    nvars = nvars + 1
    identifiers(nvars) = 'n_nit'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

  END IF

! TOPMODEL variables.
  IF ( l_top ) THEN
! Wetness in deep layer.
    nvars = nvars + 1
    identifiers(nvars) = 'sthzw'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

! Depth to water table.
    nvars = nvars + 1
    identifiers(nvars) = 'zw'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

  END IF

  IF ( l_snow_albedo .OR. l_embedded_snow ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'rgrain'
  END IF

! Additional crop scheme prognostics, required if crop scheme is on
  IF ( l_crop ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'cropdvi'

    nvars = nvars + 1
    identifiers(nvars) = 'croprootc'

    nvars = nvars + 1
    identifiers(nvars) = 'cropharvc'

    nvars = nvars + 1
    identifiers(nvars) = 'cropreservec'
  END IF

  IF ( l_irrig_dmd ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'sthu_irr'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF
  END IF

! Additional deep soil temperature if bedrock is on
  IF ( l_bedrock ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'tsoil_deep'
  END IF

! River storage if river routing on and using TRIP
  IF ( l_rivers .AND. ( rivers_type == 'trip' ) ) THEN
    IF ( read_or_write_dump_local ) THEN
      nvars = nvars + 1
      identifiers(nvars) = 'rivers_sto_rp'
    ELSE
      CALL log_warn("required_vars_for_configuration",                        &
                    "rivers_sto_rp will be initialised to zero.")      
    END IF
  END IF

! Surface and subsurface stores and flows if river routing on and using RFM
  IF ( l_rivers .AND. (rivers_type == 'rfm') ) THEN 
    IF ( read_or_write_dump_local ) THEN 
      nvars = nvars + 1
      identifiers(nvars) = 'rfm_surfstore_rp'
  
      nvars = nvars + 1
      identifiers(nvars) = 'rfm_substore_rp'

      nvars = nvars + 1
      identifiers(nvars) = 'rfm_flowin_rp'

      nvars = nvars + 1
      identifiers(nvars) = 'rfm_bflowin_rp'
    ELSE
      CALL log_warn("required_vars_for_configuration",                        &
                    "RFM river prognostics will be initialised to zero.")
    END IF
  END IF

!-----------------------------------------------------------------------------
! Add metstats prognostic variables if switched on
!-----------------------------------------------------------------------------
  IF ( l_metstats ) THEN
    IF (metstats_flag%temp_max_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'temp_max_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'temp_max_00h_r'
    END IF

    IF (metstats_flag%temp_ave_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'temp_ave_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'temp_ave_00h_r'
    END IF

    IF (metstats_flag%temp_pnt_12h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'temp_pnt_12h'
    END IF

    IF (metstats_flag%prec_tot_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'prec_tot_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'prec_tot_00h_r'
    END IF

    IF (metstats_flag%prec_tot_12h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'prec_tot_12h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'prec_tot_12h_r'
    END IF

    IF (metstats_flag%rhum_min_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'rhum_min_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'rhum_min_00h_r'
    END IF

    IF (metstats_flag%rhum_pnt_12h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'rhum_pnt_12h'
    END IF

    IF (metstats_flag%dewp_ave_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'dewp_ave_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'dewp_ave_00h_r'
    END IF

    IF (metstats_flag%wind_ave_00h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'wind_ave_00h'
      nvars              =  nvars + 1
      identifiers(nvars) = 'wind_ave_00h_r'
    END IF

    IF (metstats_flag%wind_pnt_12h) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'wind_pnt_12h'
    END IF
  END IF

!-----------------------------------------------------------------------------
! Add fire prognostic variables if switched on
!-----------------------------------------------------------------------------
  IF ( l_fire ) THEN
    IF (fire_cntl%mcarthur%flag) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_mcarthur_r_dr'
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_mcarthur_n_dr'
    END IF

    IF (fire_cntl%canadian%flag) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_canadian_ffmc'
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_canadian_ffmc_mois'
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_canadian_dmc'
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_canadian_dc'
    END IF

    IF (fire_cntl%nesterov%flag) THEN
      nvars              =  nvars + 1
      identifiers(nvars) = 'fire_nesterov'
    END IF
  END IF !Fire

!-----------------------------------------------------------------------------
! Add IMOGEN variables if IMOGEN is on and they are requested
!-----------------------------------------------------------------------------
  IF ( l_imogen .AND. inc_imogen_local ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'co2_ppmv'

    nvars = nvars + 1
    identifiers(nvars) = 'co2_change_ppmv'

    nvars = nvars + 1
    identifiers(nvars) = 'dtemp_o'

    nvars = nvars + 1
    identifiers(nvars) = 'fa_ocean'

    nvars = nvars + 1
    identifiers(nvars) = 'seed_rain'

! This should possibly be under an l_triffid switch, but is here for now
! so as not to change behaviour for non-TRIFFID runs
    nvars = nvars + 1
    identifiers(nvars) = 'cv'
  END IF

  IF ( l_daily_disagg .AND. read_or_write_dump_local .AND.                    &
       (precip_disagg_method > 1) ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'seed_rain'
  END IF

!-----------------------------------------------------------------------------
! Work out what snow variables are required
! If total_snow = T, only snow_surft is required, and is always required
! so if already in the list
! We just need to add the other required snow variables based on the scheme
! enabled if total_snow = F
!-----------------------------------------------------------------------------
  IF ( .NOT. total_snow_local ) THEN
! Snow variables not specifically for the multi-layer model
    nvars = nvars + 1
    identifiers(nvars) = 'rho_snow'

    nvars = nvars + 1
    identifiers(nvars) = 'snow_depth'

    IF ( can_model == 4 ) THEN
      nvars = nvars + 1
      identifiers(nvars) = 'snow_grnd'
    END IF

! Variables for the multi-layer snow model.
    IF ( nsmax > 0 ) THEN
      nvars = nvars + 1
      identifiers(nvars) = 'nsnow'

      nvars = nvars + 1
      identifiers(nvars) = 'snow_ds'

      nvars = nvars + 1
      identifiers(nvars) = 'snow_ice'

      nvars = nvars + 1
      identifiers(nvars) = 'snow_liq'

      nvars = nvars + 1
      identifiers(nvars) = 'tsnow'

      IF ( l_snow_albedo .OR. l_embedded_snow ) THEN
        nvars = nvars + 1
        identifiers(nvars) = 'rgrainl'
      END IF
    END IF   ! nsmax

  END IF  ! total_snow

  IF ( l_elev_land_ice) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'tsurf_elev_surft'
  END IF

!-----------------------------------------------------------------------------
! Work out which ancillary fields need to be in the dump file
! The logic here is slightly different from normal prognostic variables
!
! There are several cases to consider:
! -The var is not needed
! -The var should be populated by ancil and written to the dump (ie write only
!  as far as this module is concerned)
! -The var should be both read from the initial condition namelist and written
!  to the dump
!
! To get the information about which ancils will be read from file, we use
! ancil_dump_read%xyz:  Defaulting to false, set in init_ancillaries_mod
!-----------------------------------------------------------------------------

  !Initialise
  nvars_from_ancil = 0

  !-----------------------------------------------------------------------------
  !Set up read/write details for frac ancil
  !This is a special case: with competing vegetation on, frac is prognostic

  ! frac should always be written to the dump
  nvars = nvars + 1
  identifiers(nvars) = 'frac'

  IF ( l_veg_compete ) THEN
    ! With competing veg on, frac is a prognostic and should always be read
    ! as an initial condition
    ancil_dump_read%frac  = .TRUE.
  END IF

  IF ( .NOT. ancil_dump_read%frac ) THEN
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'frac'
  END IF

  !---------------------------------------------------------------------------
  ! Set up read/write details for soil properites ancils
  ! Most soil properties are always needed.
  nvars = nvars + 1
  identifiers(nvars) = 'b      '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'sathh  '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'satcon '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'sm_sat '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'sm_crit'
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'sm_wilt'
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'hcap   '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'hcon   '
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  nvars = nvars + 1
  identifiers(nvars) = 'albsoil'
  IF (l_append_soilt) THEN
    identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
  END IF

  IF ( soil_bgc_model == soil_model_rothc .OR.                                &
       soil_bgc_model == soil_model_ecosse) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'clay   '
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF
  END IF

  IF ( soil_bgc_model == soil_model_ecosse ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'soil_ph'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF
  END IF

  IF ( .NOT. ancil_dump_read%soil_props ) THEN
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'b      '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'sathh  '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'satcon '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'sm_sat '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'sm_crit'
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'sm_wilt'
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'hcap   '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'hcon   '
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'albsoil'
    IF ( soil_bgc_model == soil_model_rothc .OR.                              &
         soil_bgc_model == soil_model_ecosse ) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'clay   '
    END IF
    IF ( soil_bgc_model == soil_model_ecosse ) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'soil_ph'
    END IF
  END IF

  !---------------------------------------------------------------------------
  !Set up read/write details for topmodel ancils.
  !Depends on the value of l_top

  IF ( l_top ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'fexp   '
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

    nvars = nvars + 1
    identifiers(nvars) = 'ti_mean'
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF

    nvars = nvars + 1
    identifiers(nvars) = 'ti_sig '
    IF (l_append_soilt) THEN
      identifiers(nvars) = TRIM(identifiers(nvars)) // "_soilt"
    END IF


    IF ( .NOT. ancil_dump_read%top) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'fexp   '
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'ti_mean'
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'ti_sig '
    END IF
  END IF !l_top

  !---------------------------------------------------------------------------
  !Set up read/write details for agric ancil namelist
  !This is always needed

  nvars = nvars + 1
  identifiers(nvars) = 'frac_agr'

  IF ( .NOT. ancil_dump_read%agric ) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'frac_agr'
  END IF


  !---------------------------------------------------------------------------
  !Set up read/write details for soil properites ancil namelist
  !The vars required depend on a couple of switches

  IF ( l_crop ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'cropttveg'
    nvars = nvars + 1
    identifiers(nvars) = 'cropttrep'
    IF ( l_prescsow ) THEN  !we additionally need the crop sowing date
      nvars = nvars + 1
      identifiers(nvars) = 'cropsowdate'
      nvars = nvars + 1
      identifiers(nvars) = 'croplatestharvdate'
    END IF
    
    IF ( .NOT. ancil_dump_read%crop_props ) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'cropttveg'
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'cropttrep'
      IF ( l_prescsow ) THEN  !we additionally need the crop sowing date
        nvars_from_ancil = nvars_from_ancil + 1
        vars_from_ancil(nvars_from_ancil) = 'cropsowdate'
        nvars_from_ancil = nvars_from_ancil + 1
        vars_from_ancil(nvars_from_ancil) = 'croplatestharvdate'
      END IF
    END IF

  END IF !l_crop

  !---------------------------------------------------------------------------
  !Set up read/write details for irrigation ancil namelist
  !The vars required depend on l_irrig_dmd
  IF ( l_irrig_dmd ) THEN
    nvars = nvars + 1
    identifiers(nvars) = 'frac_irrig'
    nvars = nvars + 1
    identifiers(nvars) = 'frac_irr_all_tiles'
    !These only matter if frac_irr_all_tiles is false, but can't gurantee that
    !that its value will be known when reading that variable from the dump
    nvars = nvars + 1
    identifiers(nvars) = 'irrtiles'
    nvars = nvars + 1
    identifiers(nvars) = 'nirrtile'

    IF ( .NOT. ancil_dump_read%irrig ) THEN
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'frac_irrig'
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'frac_irr_all_tiles'
      !These only matter if frac_irr_all_tiles is false, but can't gurantee that
      !that its value will be known when reading that variable from the dump
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'irrtiles'
      nvars_from_ancil = nvars_from_ancil + 1
      vars_from_ancil(nvars_from_ancil) = 'nirrtile'
    END IF

  END IF

!Rivers need further work before being included
!IF ( ancil_dump_read%rivers_props ) THEN
!END IF

  !---------------------------------------------------------------------------
  !Set up read/write details for co2 ancil namelist
  !Always required, even if it just carries around the default value set in
  !the aero module
  nvars = nvars + 1
  identifiers(nvars) = 'co2_mmr'

  IF ( .NOT. ancil_dump_read%co2 ) THEN
    nvars_from_ancil = nvars_from_ancil + 1
    vars_from_ancil(nvars_from_ancil) = 'co2_mmr'
  END IF

  RETURN

END SUBROUTINE required_vars_for_configuration
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE read_dump(file_name, identifiers)

  !Science variables
  USE model_grid_mod, ONLY:                                                   &
    global_land_pts

  USE ancil_info, ONLY:                                                       &
    land_pts, dim_cs1, frac_surft, nsurft, nsoilt, dim_cslayer, nsoilt

  USE jules_surface_types_mod, ONLY:                                          &
    npft, ntype, ncpft

  USE imogen_constants, ONLY:                                                 &
    n_olevs, nfarray

  USE prognostics, ONLY:                                                      &
    canht_pft, canopy_surft, cs_pool_soilt, gs_gb, lai_pft,                   &
    nsnow_surft, rgrain_surft, rgrainl_surft,                                 &
    rho_snow_grnd_surft, sice_surft, sliq_surft,                              &
    snow_grnd_surft, snow_surft, snowdepth_surft,                             &
    t_soil_soilt, tsnow_surft, tstar_surft, seed_rain,                        &
    ds_surft, tsoil_deep_gb, tsurf_elev_surft,                                &
    frac_agr_prev_gb,                                                         &
    wood_prod_fast_gb, wood_prod_med_gb,                                      &
    wood_prod_slow_gb, n_inorg_soilt_lyrs, ns_pool_gb,                        &
    frac_past_prev_gb, l_broadcast_soilt

  USE p_s_parms, ONLY:                                                        &
    albsoil_soilt, bexp_soilt, sathh_soilt, satcon_soilt,                     &
    smvcst_soilt, smvccl_soilt, smvcwt_soilt, hcap_soilt,                     &
    hcon_soilt, sthu_soilt, clay_soilt, soil_ph_soilt

  USE crop_vars_mod, ONLY:                                                    &
    dvi_cpft, rootc_cpft, harvc_cpft, reservec_cpft, croplai_cpft,            &
    cropcanht_cpft, sthu_irr_soilt, sow_date_cpft, tt_veg_cpft, tt_rep_cpft,  &
    latestharv_date_cpft, irrtiles, frac_irr_all_tiles, nirrtile, frac_irr_all

  USE imogen_progs, ONLY:                                                     &
    co2_ppmv, co2_change_ppmv, dtemp_o, fa_ocean

  USE soil_ecosse_vars_mod, ONLY:                                             &
    n_amm_soilt, n_nit_soilt

  USE trifctl, ONLY:                                                          &
    cv_gb, frac_agr_gb

  USE trif_vars_mod, ONLY:                                                    &
    frac_past_gb

  USE jules_snow_mod, ONLY:                                                   &
    nsmax

  USE jules_soil_mod, ONLY:                                                   &
    sm_levels, ns_deep, l_tile_soil

  USE top_pdm, ONLY:                                                          &
    sthzw_soilt, zw_soilt, fexp_soilt, ti_mean_soilt, ti_sig_soilt

  USE fire_mod, ONLY:                                                         &
    fire_prog

  USE metstats_mod, ONLY:                                                     &
    metstats_prog

  USE jules_rivers_mod, ONLY:                                                 &
    rivers_sto_rp, rfm_surfstore_rp,                                          &
    rfm_substore_rp, rfm_flowin_rp, rfm_bflowin_rp

  USE aero, ONLY:                                                             &
    co2_mmr

  !Others
  USE mpi
  USE io_constants, ONLY:                                                     &
    mode_read, max_dim_var

  USE parallel_mod, ONLY:                                                     &
    master_task_id, is_master_task, scatter_land_field

  USE string_utils_mod, ONLY:                                                 &
    to_string

  USE file_mod, ONLY:                                                         &
    file_handle, file_open, file_introspect,                                  &
    file_inquire_dim, file_inquire_var, file_read_var,                        &
    file_close

  USE update_mod, ONLY : sthuf_soilt

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Check that the given file is a JULES dump compatible with the current
!   run, and read the given identifiers from it.
!   Note that the reading of the dump is done by the master task and the
!   results scattered to other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  CHARACTER(LEN=*) :: file_name  ! The dump file
  CHARACTER(LEN=*) :: identifiers(:)  ! The model identifiers for the
                                      ! variables to define

! Local parameters.
  LOGICAL, PARAMETER :: l_reading_true = .TRUE.
    ! A value of .TRUE. that is passed to argument l_reading of subroutine
    ! get_dim_info to show that it is being called in connection with reading
    ! (rather than writing) a dump.

! Work variables
  TYPE(file_handle) :: file
    ! The opened file

  INTEGER :: nvars
    ! The number of variables we are processing

  INTEGER :: dim_size_file
    ! The size of the dimension currently being
    ! processed in the file

! Used when defining dimensions and variables
  INTEGER :: ndims
    ! The number of dimensions the current variable has
  CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
    ! The dimension names the current variable should use
  INTEGER :: dim_sizes(max_dim_var)
    ! The dimension sizes for the current variable
  INTEGER :: dim_ids(max_dim_var)
    ! The dimension ids for the current variable as
    ! calculated from file_inquire_dim
  LOGICAL :: is_record_dim
    ! Detects if the current dimension is a record dim
  INTEGER :: ndims_file
    ! The number of dimensions the variable has in file
    ! Compared to ndims above for each variable
  INTEGER :: dim_ids_file(max_dim_var)
    ! The ids of the dimensions the variable has in the file
    ! Compared to dim_ids above to verify the variable has the
    ! correct dimensions

  INTEGER :: var_ids(SIZE(identifiers))
    ! The ids of the variables in the dump file

  LOGICAL :: is_record_var
    ! Indicates if a variable uses the record dimension

  LOGICAL :: l_read_from_dump
    ! Used to bypass checking the dimensions of ancil variables that are
    ! not to be read from the dump.

  REAL :: frac_irr_all_tiles_real
  REAL :: irrtiles_real(npft)
  REAL :: nirrtile_real

! Real versions of integer valued variables
  REAL :: nsnow_real(land_pts, nsurft)
  REAL :: seed_rain_real(SIZE(seed_rain))

  INTEGER :: i, j, m, n, k  ! Loop counters

  INTEGER :: error  ! Variable to collect MPI errors - most MPI
                    ! implementations bail on error, so this is not checked.


! Arrays to hold global land points version of data read in master task
! before scattering
  REAL, ALLOCATABLE :: global_data_1d(:)     ! Data with no vertical levels
  REAL, ALLOCATABLE :: global_data_2d(:,:)   ! With one vertical level
  REAL, ALLOCATABLE :: global_data_3d(:,:,:) ! With two "vertical" levels
                                               ! e.g. snow variables
  REAL, ALLOCATABLE :: global_data_4d(:,:,:,:) ! With 3 "vertical" levels

!-----------------------------------------------------------------------------

  nvars = SIZE(identifiers)

!-----------------------------------------------------------------------------
! In the master task only, we open the file and check that the correct
! dimensions exist and are of a size compatible with this run
!-----------------------------------------------------------------------------
  IF ( is_master_task() ) THEN
!-----------------------------------------------------------------------------
! We use the lowest level file API here, as we don't want to impose the input
! grid
!-----------------------------------------------------------------------------
    file = file_open(file_name, mode_read)

! We want to auto-detect the dimensions and variables in the file
    CALL file_introspect(file)

    DO i = 1,nvars

      !-----------------------------------------------------------------------
      ! Get information about the dimensions used by the variable.
      ! The argument l_reading_true shows that we are reading (not writing) a
      ! dump.
      !-----------------------------------------------------------------------
      CALL get_dim_info( l_reading_true, identifiers(i), ndims,  dim_sizes,   &
                         dim_names, l_read_from_dump )

!-----------------------------------------------------------------------------
! Check the dimensions exist and have the correct size
!-----------------------------------------------------------------------------
      IF ( l_read_from_dump ) THEN
        DO j = 1,ndims
  ! Retrive information about the dimension from the file we store the id
  ! for use outside this loop
          CALL file_inquire_dim(                                              &
            file, dim_names(j), dim_ids(j), dim_size_file, is_record_dim      &
          )

  ! Check that we found a dimension
          IF ( dim_ids(j) < 0 )                                               &
            CALL log_fatal("read_dump",                                       &
                           "Could not find expected dimension '" //           &
                           TRIM(dim_names(j)) // "' in dump file")

  ! Check that the dimension is not a record dimension (there shouldn't be one
  ! in dump files)
          IF ( is_record_dim )                                                &
            CALL log_fatal("read_dump",                                       &
                           "Dimension '" // TRIM(dim_names(j)) // "' is a " //&
                           "record dimension - should not exist in dump file")

  ! Check that the dimension has the correct size
          IF ( dim_size_file /= dim_sizes(j) )                                &
            CALL log_fatal("read_dump",                                       &
                           "Dimension '" // TRIM(dim_names(j)) // "' has " // &
                           "size incompatible with current run (required: " //&
                           TRIM(to_string(dim_sizes(j))) // ", found: " //    &
                           TRIM(to_string(dim_size_file)) // ")")
        END DO  ! dims

!-----------------------------------------------------------------------------
! Check that the variable exists and has the correct dimensions
!-----------------------------------------------------------------------------
! Retrieve information about the variable from the file
      CALL file_inquire_var(                                                  &
        file, identifiers(i), var_ids(i), ndims_file, dim_ids_file,           &
        is_record_var                                                         &
      )

! Check that we found a variable
      IF ( var_ids(i) < 1 )                                                   &
        CALL log_fatal("read_dump",                                           &
                       "Failed to find requested variable '" //               &
                       TRIM(identifiers(i)) // "' in dump file")

! Check that the number of dimensions match
      IF ( ndims_file /= ndims )                                              &
        CALL log_fatal("read_dump",                                           &
                       "Variable '" // TRIM(identifiers(i)) // "' has " //    &
                       "incorrect number of dimensions in dump file (" //     &
                       "expected: " // TRIM(to_string(ndims)) // ", " //      &
                       "found: " // TRIM(to_string(ndims_file)) // ")")

! Check that the dimension ids match
      IF ( .NOT. ALL(dim_ids(1:ndims) == dim_ids_file(1:ndims)) )             &
        CALL log_fatal("read_dump",                                           &
                       "Variable '" // TRIM(identifiers(i)) // "' has " //    &
                       "incorrect dimensions in dump file")

      END IF  !  l_read_from_dump

    END DO  ! vars

  END IF  ! MASTER TASK

!-----------------------------------------------------------------------------
! Set the requested variables from the file
!
! This is done by reading the value of the variable on global land points
! in the master task, then scattering it to the other tasks
!
! We assume that if the file passed all the checks on dimensions above, then
! it will be fine to fill variables here (i.e. we don't check the dimensions
! associated with the variables)
!-----------------------------------------------------------------------------
! Allocate the global data arrays
  IF ( is_master_task() ) THEN
    ALLOCATE(global_data_1d(global_land_pts))
    ALLOCATE(global_data_2d(global_land_pts, MAX(npft, dim_cs1, sm_levels,  &
                                                 nsurft, ntype, ns_deep,    &
                                                 nsoilt)))
    ALLOCATE(global_data_3d(global_land_pts,                                &
                            MAX(nsurft, dim_cslayer, nsoilt),               &
                            MAX(nsmax, dim_cs1, sm_levels)))
    ALLOCATE(global_data_4d(global_land_pts, nsoilt,                        &
                            MAX(nsurft, dim_cslayer),                       &
                            MAX(nsmax, dim_cs1)))
  ELSE
    ALLOCATE(global_data_1d(1))
    ALLOCATE(global_data_2d(1,1))
    ALLOCATE(global_data_3d(1,1,1))
    ALLOCATE(global_data_4d(1,1,1,1))
  END IF

  DO i = 1,nvars

!-----------------------------------------------------------------------------
! In the master task, read the global data
!-----------------------------------------------------------------------------
    IF ( is_master_task() ) THEN
      SELECT CASE ( identifiers(i) )
! If it is a land_pts array with no levels associated, read into the
! global_data_1d array
        CASE ( 'gs', 'sthzw', 'zw', 'cv', 'frac_agr_prev', 'frac_past_prev', &
               'wood_prod_fast', 'wood_prod_med', 'wood_prod_slow',          &
               'temp_max_00h_r', 'temp_ave_00h_r', 'prec_tot_00h_r',         &
               'prec_tot_12h_r', 'rhum_min_00h_r', 'dewp_ave_00h_r',         &
               'wind_ave_00h_r', 'temp_max_00h',   'temp_ave_00h',           &
               'temp_pnt_12h',   'prec_tot_00h',   'prec_tot_12h',           &
               'rhum_min_00h',   'rhum_pnt_12h',   'dewp_ave_00h',           &
               'wind_ave_00h',   'wind_pnt_12h',                             &
               'fire_mcarthur_r_dr', 'fire_mcarthur_n_dr',                   &
               'fire_canadian_ffmc', 'fire_canadian_ffmc_mois',              &
               'fire_canadian_dmc',  'fire_canadian_dc',                     &
               'fire_nesterov')
          CALL file_read_var(file, var_ids(i), global_data_1d)

        CASE ( 'sthzw_soilt', 'zw_soilt' )
          CALL file_read_var(file, var_ids(i), global_data_2d(:,1:nsoilt))

! If it is a variable with one or more vertical levels,
! read the appropriate number of levels into global_data_2d
        CASE ( 'canht', 'lai' )
          CALL file_read_var(file, var_ids(i), global_data_2d(:,1:npft))

        CASE ( 'cropdvi', 'croprootc', 'cropharvc', 'cropreservec',          &
               'croplai', 'cropcanht' )
          CALL file_read_var(file, var_ids(i), global_data_2d(:,1:ncpft))

        CASE ( 'sthuf', 't_soil', 'sthu_irr' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_2d(:,1:sm_levels))

        CASE ( 'sthuf_soilt', 't_soil_soilt', 'sthu_irr_soilt')
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_3d(:,1:nsoilt,1:sm_levels))

        CASE ( 'n_amm', 'n_nit', 'n_inorg' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_2d(:,1:dim_cslayer))

        CASE ( 'n_amm_soilt', 'n_nit_soilt', 'n_inorg_soilt' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_3d(:,1:nsoilt,1:dim_cslayer))

       CASE ( 'tsoil_deep' )
          CALL file_read_var(file, var_ids(i), global_data_2d(:,1:ns_deep))

        CASE ( 'canopy', 'nsnow', 'rgrain', 'rho_snow', 'snow_tile',         &
               'snow_depth', 'snow_grnd', 'tstar_tile', 'tsurf_elev_surft' )
          CALL file_read_var(file, var_ids(i), global_data_2d(:,1:nsurft))

        CASE ( 'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_3d(:,1:nsurft,1:nsmax))

        CASE ( 'cs','ns' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_3d(:,1:dim_cslayer,1:dim_cs1))

        CASE ( 'cs_soilt', 'ns_soilt' )
          CALL file_read_var(file, var_ids(i),                               &
                             global_data_4d(:,1:nsoilt,1:dim_cslayer,1:dim_cs1))

! Cases for IMOGEN variables
! Each task runs its own version of IMOGEN - these variables are broadcast to
! all tasks below
        CASE ( 'co2_ppmv' )
          CALL file_read_var(file, var_ids(i), co2_ppmv)

        CASE ( 'co2_change_ppmv' )
          CALL file_read_var(file, var_ids(i), co2_change_ppmv)

        CASE ( 'dtemp_o' )
          CALL file_read_var(file, var_ids(i), dtemp_o)

        CASE ( 'fa_ocean' )
          CALL file_read_var(file, var_ids(i), fa_ocean)

        CASE ( 'seed_rain' )
          CALL file_read_var(file, var_ids(i), seed_rain_real)

        ! River routing variables
        CASE ( 'rivers_sto_rp' )
          CALL file_read_var(file, var_ids(i), rivers_sto_rp)

        CASE ( 'rfm_surfstore_rp' )
          CALL file_read_var(file, var_ids(i), rfm_surfstore_rp)

        CASE ( 'rfm_substore_rp' )
          CALL file_read_var(file, var_ids(i), rfm_substore_rp)

        CASE ( 'rfm_flowin_rp' )
          CALL file_read_var(file, var_ids(i), rfm_flowin_rp)

        CASE ( 'rfm_bflowin_rp' )
           CALL file_read_var(file, var_ids(i), rfm_bflowin_rp)

!-----------------------------------------------------------------------------
! Ancilliary variables
!-----------------------------------------------------------------------------

        !Frac ancil namelist
        CASE ( 'frac' )
          IF ( ancil_dump_read%frac ) THEN
            CALL file_read_var(file, var_ids(i), global_data_2d(:,1:ntype))
          END IF

        !Soil properties ancil namelist
        CASE ( 'b      ', 'sathh  ', 'satcon ', 'sm_sat ', 'sm_crit',         &
               'sm_wilt', 'hcap   ', 'hcon   ' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i),                              &
                               global_data_2d(:,1:sm_levels))
          END IF

        CASE ( 'b_soilt', 'sathh_soilt', 'satcon_soilt', 'sm_sat_soilt',      &
               'sm_crit_soilt', 'sm_wilt_soilt', 'hcap_soilt', 'hcon_soilt' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i),                              &
                               global_data_3d(:,1:nsoilt,1:sm_levels))
          END IF

        CASE ( 'albsoil' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i), global_data_1d)
          END IF

        CASE ( 'albsoil_soilt' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i), global_data_2d(:,1:nsoilt))
          END IF

        CASE ( 'clay', 'soil_ph' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i),                              &
                               global_data_2d(:,1:dim_cslayer))
          END IF

        CASE ( 'clay_soilt', 'soil_ph_soilt' )
          IF ( ancil_dump_read%soil_props ) THEN
            CALL file_read_var(file, var_ids(i),                              &
                               global_data_3d(:,1:nsoilt,1:dim_cslayer))
          END IF

        !Topmodel ancil namelist
        CASE ( 'fexp   ', 'ti_mean', 'ti_sig ' )
          IF ( ancil_dump_read%top ) THEN
            CALL file_read_var(file, var_ids(i), global_data_1d)
          END IF

        CASE ( 'fexp_soilt', 'ti_mean_soilt', 'ti_sig_soilt' )
          IF ( ancil_dump_read%top ) THEN
            CALL file_read_var(file, var_ids(i), global_data_2d(:,1:nsoilt))
          END IF

        !Agric ancil namelist
        CASE ( 'frac_agr', 'frac_past' )
          IF ( ancil_dump_read%agric ) THEN
            CALL file_read_var(file, var_ids(i), global_data_1d)
          END IF

        !Crop props ancillaries namelist
        CASE ( 'cropsowdate', 'cropttveg  ', 'cropttrep  ','croplatestharvdate')
          IF ( ancil_dump_read%crop_props ) THEN
            CALL file_read_var(file, var_ids(i), global_data_2d(:,1:ncpft))
          END IF

        !Irrigation ancillaries namelist
        CASE ( 'frac_irrig' )
          IF ( ancil_dump_read%irrig ) THEN
            CALL file_read_var(file, var_ids(i), global_data_1d)
          END IF

        CASE ( 'frac_irr_all_tiles' )
          IF ( ancil_dump_read%irrig ) THEN
            CALL file_read_var(file, var_ids(i), frac_irr_all_tiles_real)
          END IF

        CASE ( 'irrtiles' )
          IF ( ancil_dump_read%irrig ) THEN
            CALL file_read_var(file, var_ids(i), irrtiles_real)
          END IF

        CASE ( 'nirrtile' )
          IF ( ancil_dump_read%irrig ) THEN
            CALL file_read_var(file, var_ids(i), nirrtile_real)
          END IF

        !CO2 ancil namelist
        CASE ( 'co2_mmr' )
          IF ( ancil_dump_read%co2 ) THEN
            CALL file_read_var(file, var_ids(i), co2_mmr)
          END IF

        CASE DEFAULT
          CALL log_fatal("read_dump",                                         &
                         "Unexpected variable in dump - " //                  &
                         TRIM(identifiers(i)))
      END SELECT
    END IF  ! MASTER TASK

! Now scatter the variables into their final destinations
! Note that scatter_land_field can only scatter one land_pts array at a time,
! so to scatter variables with multiple levels we must loop
    SELECT CASE ( identifiers(i) )
      CASE ( 'gs' )
        CALL scatter_land_field(global_data_1d, gs_gb)

      CASE ( 'sthzw' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m=1, nsoilt
            CALL scatter_land_field(global_data_1d, sthzw_soilt(:,m))
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          CALL scatter_land_field(global_data_1d, sthzw_soilt(:,1))
        END IF

      CASE ( 'sthzw_soilt' )
        DO m=1, nsoilt
          CALL scatter_land_field(global_data_2d(:,m), sthzw_soilt(:,m))
        END DO

      CASE ( 'zw' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m=1, nsoilt
            CALL scatter_land_field(global_data_1d, zw_soilt(:,m))
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          CALL scatter_land_field(global_data_1d, zw_soilt(:,1))
        END IF

      CASE ( 'zw_soilt' )
        DO m=1,nsoilt
          CALL scatter_land_field(global_data_2d(:,m), zw_soilt(:,m))
        END DO

      CASE ( 'cv' )
        CALL scatter_land_field(global_data_1d, cv_gb)

      CASE ( 'frac_agr_prev' )
        CALL scatter_land_field(global_data_1d, frac_agr_prev_gb)

      CASE ( 'frac_past_prev' )
        CALL scatter_land_field(global_data_1d, frac_past_prev_gb)

      CASE ( 'wood_prod_fast' )
        CALL scatter_land_field(global_data_1d, wood_prod_fast_gb)

      CASE ( 'wood_prod_med' )
        CALL scatter_land_field(global_data_1d, wood_prod_med_gb)

      CASE ( 'wood_prod_slow' )
        CALL scatter_land_field(global_data_1d, wood_prod_slow_gb)

      CASE ( 'n_inorg' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m=1, nsoilt
            DO n = 1,dim_cslayer
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      n_inorg_soilt_lyrs(:,m,n))
            END DO
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_2d(:,n),                      &
                                    n_inorg_soilt_lyrs(:,1,n))
          END DO
        END IF

      CASE ( 'n_inorg_soilt' )
        DO m=1, nsoilt
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    n_inorg_soilt_lyrs(:,m,n))
          END DO
        END DO

      CASE ( 'canht' )
        DO n = 1,npft
          CALL scatter_land_field(global_data_2d(:,n), canht_pft(:,n))
        END DO

      CASE ( 'lai' )
        DO n = 1,npft
          CALL scatter_land_field(global_data_2d(:,n), lai_pft(:,n))
        END DO

      CASE ( 'cropdvi' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), dvi_cpft(:,n))
        END DO

      CASE ( 'croprootc' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), rootc_cpft(:,n))
        END DO

      CASE ( 'cropharvc' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), harvc_cpft(:,n))
        END DO

      CASE ( 'cropreservec' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), reservec_cpft(:,n))
        END DO

      CASE ( 'croplai' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), croplai_cpft(:,n))
        END DO

      CASE ( 'cropcanht' )
        DO n = 1,ncpft
          CALL scatter_land_field(global_data_2d(:,n), cropcanht_cpft(:,n))
        END DO

      CASE ( 'cs' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO k = 1,nsoilt !using k rather than m as usual
            DO n = 1,dim_cs1
              DO m = 1,dim_cslayer
                CALL scatter_land_field(global_data_3d(:,m,n),                &
                                        cs_pool_soilt(:,k,m,n))
              END DO
            END DO
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          DO n = 1,dim_cs1
            DO m = 1,dim_cslayer
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      cs_pool_soilt(:,1,m,n))
            END DO
          END DO
        END IF

      CASE ( 'cs_soilt' )
        DO k = 1,nsoilt !using k rather than m as usual
          DO n = 1,dim_cs1
            DO m = 1,dim_cslayer
              CALL scatter_land_field(global_data_4d(:,k,m,n),                &
                                      cs_pool_soilt(:,k,m,n))
            END DO
          END DO
        END DO

      CASE ( 'ns' )
        DO n = 1,dim_cs1
          DO m = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    ns_pool_gb(:,m,n))
          END DO
        END DO

      CASE ( 'sthuf' )
        ! sthuf is held in sthu until it is processed
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m = 1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n), sthuf_soilt(:,m,n))
            END DO
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n), sthuf_soilt(:,1,n))
          END DO
        END IF

      CASE ( 'sthuf_soilt' )
        ! sthuf is held in sthu until it is processed
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_3d(:,m,n), sthuf_soilt(:,m,n))
          END DO
        END DO

      CASE ( 't_soil' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m = 1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      t_soil_soilt(:,m,n))
            END DO
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n), t_soil_soilt(:,1,n))
          END DO
        END IF

      CASE ( 't_soil_soilt' )
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    t_soil_soilt(:,m,n))
          END DO
        END DO

      CASE ( 'tsoil_deep' )
        DO n = 1,ns_deep
          CALL scatter_land_field(global_data_2d(:,n), tsoil_deep_gb(:,n))
        END DO

      CASE ( 'sthu_irr' )
        IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
          DO m = 1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      sthu_irr_soilt(:,m,n))
            END DO
          END DO
        ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                      &
                                    sthu_irr_soilt(:,1,n))
          END DO
        END IF

      CASE ( 'sthu_irr_soilt' )
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    sthu_irr_soilt(:,m,n))
          END DO
        END DO

      CASE ( 'canopy' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), canopy_surft(:,n))
        END DO

      CASE ( 'nsnow' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), nsnow_real(:,n))
        END DO
        nsnow_surft(:,:) = NINT(nsnow_real(:,:))

      CASE ( 'rgrain' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), rgrain_surft(:,n))
        END DO

      CASE ( 'rho_snow' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n),                        &
                                  rho_snow_grnd_surft(:,n))
        END DO

      CASE ( 'snow_tile' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), snow_surft(:,n))
        END DO

      CASE ( 'snow_depth' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n),                        &
                                  snowdepth_surft(:,n))
        END DO

      CASE ( 'snow_grnd' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), snow_grnd_surft(:,n))
        END DO

      CASE ( 'tstar_tile' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n), tstar_surft(:,n))
        END DO

      CASE ( 'tsurf_elev_surft' )
        DO n = 1,nsurft
          CALL scatter_land_field(global_data_2d(:,n),                        &
                                  tsurf_elev_surft(:,n))
        END DO

      CASE ( 'rgrainl' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    rgrainl_surft(:,m,n))
          END DO
        END DO

      CASE ( 'snow_ds' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL scatter_land_field(global_data_3d(:,m,n), ds_surft(:,m,n))
          END DO
        END DO

      CASE ( 'snow_ice' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    sice_surft(:,m,n))
          END DO
        END DO


      CASE ( 'snow_liq' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    sliq_surft(:,m,n))
          END DO
        END DO


      CASE ( 'tsnow' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL scatter_land_field(global_data_3d(:,m,n),                    &
                                    tsnow_surft(:,m,n))
          END DO
        END DO

! Fire and metstats
      CASE ( 'temp_max_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%temp_max_00h%run)

      CASE ( 'temp_ave_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%temp_ave_00h%run)

      CASE ( 'prec_tot_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%prec_tot_00h%run)

      CASE ( 'prec_tot_12h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%prec_tot_12h%run)

      CASE ( 'rhum_min_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%rhum_min_00h%run)

      CASE ( 'dewp_ave_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%dewp_ave_00h%run)

      CASE ( 'wind_ave_00h_r' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%wind_ave_00h%run)

      CASE ( 'temp_max_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%temp_max_00h%fin)

      CASE ( 'temp_ave_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%temp_ave_00h%fin)

      CASE ( 'temp_pnt_12h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%temp_pnt_12h%fin)

      CASE ( 'prec_tot_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%prec_tot_00h%fin)

      CASE ( 'prec_tot_12h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%prec_tot_12h%fin)

      CASE ( 'rhum_min_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%rhum_min_00h%fin)

      CASE ( 'rhum_pnt_12h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%rhum_pnt_12h%fin)

      CASE ( 'dewp_ave_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%dewp_ave_00h%fin)

      CASE ( 'wind_ave_00h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%wind_ave_00h%fin)

      CASE ( 'wind_pnt_12h' )
        CALL scatter_land_field(global_data_1d,                               &
                                metstats_prog(:)%wind_pnt_12h%fin)

! Fire module variables- land points only
      CASE ( 'fire_mcarthur_r_dr' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%mcarthur%r_dr)

      CASE ( 'fire_mcarthur_n_dr' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%mcarthur%n_dr)

      CASE ( 'fire_canadian_ffmc' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%ffmc)

      CASE ( 'fire_canadian_ffmc_mois' )
        CALL scatter_land_field(global_data_1d,                               &
                                fire_prog(:)%canadian%ffmc_mois)

      CASE ( 'fire_canadian_dmc' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%dmc)

      CASE ( 'fire_canadian_dc' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%dc)

      CASE ( 'fire_nesterov' )
        CALL scatter_land_field(global_data_1d, fire_prog(:)%nesterov%findex)

! ECOSSE variables.
      CASE ( 'n_amm' )
        DO n = 1,dim_cslayer
          CALL scatter_land_field(global_data_2d(:,n), n_amm_soilt(:,1,n))
        END DO

      CASE ( 'n_amm_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n), n_amm_soilt(:,m,n))
          END DO
        END DO

      CASE ( 'n_nit' )
        DO n = 1,dim_cslayer
          CALL scatter_land_field(global_data_2d(:,n), n_nit_soilt(:,1,n))
        END DO

      CASE ( 'n_nit_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n), n_nit_soilt(:,m,n))
          END DO
        END DO

! IMOGEN variables are just broadcast to all tasks
      CASE ( 'co2_ppmv' )
        CALL mpi_bcast(co2_ppmv, 1, mpi_real,                                 &
                       master_task_id, mpi_comm_world, error)

      CASE ( 'co2_change_ppmv' )
        CALL mpi_bcast(co2_change_ppmv, 1, mpi_real,                          &
                       master_task_id, mpi_comm_world, error)

      CASE ( 'dtemp_o' )
        CALL mpi_bcast(dtemp_o, n_olevs, mpi_real,                            &
                       master_task_id, mpi_comm_world, error)

      CASE ( 'fa_ocean' )
        CALL mpi_bcast(fa_ocean, nfarray, mpi_real,                           &
                       master_task_id, mpi_comm_world, error)

      CASE ( 'seed_rain' )
        CALL mpi_bcast(seed_rain_real, SIZE(seed_rain_real), mpi_real,        &
                       master_task_id, mpi_comm_world, error)
        seed_rain(:) = NINT(seed_rain_real(:))

      ! River routing variables
      CASE ( 'rivers_sto_rp', 'rfm_surfstore_rp', 'rfm_substore_rp',          &
             'rfm_flowin_rp', 'rfm_bflowin_rp' )
        ! nothing to do

!-----------------------------------------------------------------------------
! Ancillary variables
!-----------------------------------------------------------------------------

      !Frac ancil namelist
      CASE ( 'frac' )
        IF ( ancil_dump_read%frac ) THEN
          DO n = 1,ntype
            CALL scatter_land_field(global_data_2d(:,n), frac_surft(:,n))
          END DO
        END IF

      !Soil properties ancil namelist
      !Cases if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1

      CASE ( 'b      ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n), bexp_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n), bexp_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'b_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n), bexp_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'sathh  ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        sathh_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n), sathh_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'sathh_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      sathh_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'satcon ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        satcon_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      satcon_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'satcon_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      satcon_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'sm_sat ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        smvcst_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      smvcst_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'sm_sat_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      smvcst_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'sm_crit' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        smvccl_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      smvccl_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'sm_crit_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      smvccl_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'sm_wilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        smvcwt_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n),                    &
                                      smvcwt_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'sm_wilt_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      smvcwt_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'hcap   ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        hcap_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n), hcap_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'hcap_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      hcap_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'hcon   ' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              DO n = 1,sm_levels
                CALL scatter_land_field(global_data_2d(:,n),                  &
                                        hcon_soilt(:,m,n))
              END DO
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_2d(:,n), hcon_soilt(:,1,n))
            END DO
          END IF
        END IF

      CASE ( 'hcon_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            DO n = 1,sm_levels
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      hcon_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'albsoil' )
        IF ( ancil_dump_read%soil_props ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              CALL scatter_land_field(global_data_1d, albsoil_soilt(:,m))
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            CALL scatter_land_field(global_data_1d, albsoil_soilt(:,1))
          END IF
        END IF

      CASE ( 'albsoil_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m=1,nsoilt
            CALL scatter_land_field(global_data_2d(:,m), albsoil_soilt(:,m))
          END DO
        END IF

      CASE ( 'clay' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_2d(:,n),                      &
                                    clay_soilt(:,1,n))
          END DO
        END IF

      CASE ( 'clay_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m = 1,nsoilt
            DO n = 1,dim_cslayer
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      clay_soilt(:,m,n))
            END DO
          END DO
        END IF

      CASE ( 'soil_ph' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO n = 1,dim_cslayer
            CALL scatter_land_field(global_data_2d(:,n), soil_ph_soilt(:,1,n))
          END DO
        END IF

      CASE ( 'soil_ph_soilt' )
        IF ( ancil_dump_read%soil_props ) THEN
          DO m = 1,nsoilt
            DO n = 1,dim_cslayer
              CALL scatter_land_field(global_data_3d(:,m,n),                  &
                                      soil_ph_soilt(:,m,n))
            END DO
          END DO
        END IF

      !Topmodel ancil namelist
      CASE ( 'fexp   ' )
        IF ( ancil_dump_read%top ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              CALL scatter_land_field(global_data_1d, fexp_soilt(:,m))
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            CALL scatter_land_field(global_data_1d, fexp_soilt(:,1))
          END IF
        END IF

      CASE ( 'fexp_soilt' )
        IF ( ancil_dump_read%top ) THEN
          DO m=1,nsoilt
            CALL scatter_land_field(global_data_2d(:,m), fexp_soilt(:,m))
          END DO
        END IF

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'ti_mean' )
        IF ( ancil_dump_read%top ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              CALL scatter_land_field(global_data_1d, ti_mean_soilt(:,m))
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            CALL scatter_land_field(global_data_1d, ti_mean_soilt(:,1))
          END IF
        END IF

      CASE ( 'ti_mean_soilt' )
        IF ( ancil_dump_read%top ) THEN
          DO m=1,nsoilt
            CALL scatter_land_field(global_data_2d(:,m), ti_mean_soilt(:,m))
          END DO
        END IF

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'ti_sig ' )
        IF ( ancil_dump_read%top ) THEN
          IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
            DO m = 1,nsoilt
              CALL scatter_land_field(global_data_1d, ti_sig_soilt(:,m))
            END DO
          ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
            CALL scatter_land_field(global_data_1d, ti_sig_soilt(:,1))
          END IF
        END IF

      CASE ( 'ti_sig_soilt' )
        IF ( ancil_dump_read%top ) THEN
          DO m=1,nsoilt
            CALL scatter_land_field(global_data_2d(:,m), ti_sig_soilt(:,m))
          END DO
        END IF

      !Agric ancil namelist
      CASE ( 'frac_agr' )
        IF ( ancil_dump_read%agric ) THEN
          CALL scatter_land_field(global_data_1d, frac_agr_gb)
        END IF

      CASE ( 'frac_past' )
        IF ( ancil_dump_read%agric ) THEN
          CALL scatter_land_field(global_data_1d, frac_past_gb)
        END IF

      !Crop props ancillaries namelist
      CASE ( 'cropsowdate       ' )
        IF ( ancil_dump_read%crop_props ) THEN
          DO n = 1,ncpft
            CALL scatter_land_field(global_data_2d(:,n), sow_date_cpft(:,n))
          END DO
        END IF

      CASE ( 'croplatestharvdate' )
        IF ( ancil_dump_read%crop_props ) THEN
          DO n = 1,ncpft
            CALL scatter_land_field(global_data_2d(:,n),                      &
                                    latestharv_date_cpft(:,n))
          END DO
        END IF

      CASE ( 'cropttveg         ' )
        IF ( ancil_dump_read%crop_props ) THEN
          DO n = 1,ncpft
            CALL scatter_land_field(global_data_2d(:,n), tt_veg_cpft(:,n))
          END DO
        END IF

      CASE ( 'cropttrep         ' )
        IF ( ancil_dump_read%crop_props ) THEN
          DO n = 1,ncpft
            CALL scatter_land_field(global_data_2d(:,n), tt_rep_cpft(:,n))
          END DO
        END IF

      !Irrigation ancillaries namelist
      CASE ( 'frac_irrig' )
        IF ( ancil_dump_read%irrig ) THEN
          CALL scatter_land_field(global_data_1d, frac_irr_all(:,1))
        END IF

      CASE ( 'frac_irr_all_tiles' )
        IF ( ancil_dump_read%irrig ) THEN
          IF ( frac_irr_all_tiles_real > 0.5 ) THEN
            frac_irr_all_tiles = .TRUE.
          ELSE
            frac_irr_all_tiles = .FALSE.
          END IF
          CALL mpi_bcast(frac_irr_all_tiles, 1, mpi_logical,                  &
               master_task_id, mpi_comm_world, error)
        END IF

      CASE ( 'irrtiles' )
        IF ( ancil_dump_read%irrig ) THEN
          CALL mpi_bcast(NINT(irrtiles_real), npft, mpi_integer,              &
               master_task_id, mpi_comm_world, error)
        END IF

      CASE ( 'nirrtile' )
        IF ( ancil_dump_read%irrig ) THEN
          CALL mpi_bcast(NINT(nirrtile_real), 1, mpi_integer,                 &
               master_task_id, mpi_comm_world, error)
        END IF

      !CO2 ancil namelist
      CASE ( 'co2_mmr' )
        IF ( ancil_dump_read%co2 ) THEN
          CALL mpi_bcast(co2_mmr, 1, mpi_real,                                &
               master_task_id, mpi_comm_world, error)
        END IF

      CASE DEFAULT
        CALL log_fatal("read_dump",                                           &
                       "Unexpected variable in dump - " //                    &
                       TRIM(identifiers(i)))
    END SELECT

  END DO

! We are done with the file
  IF ( is_master_task() ) CALL file_close(file)


  IF ( ALLOCATED(global_data_1d) ) DEALLOCATE(global_data_1d)
  IF ( ALLOCATED(global_data_2d) ) DEALLOCATE(global_data_2d)
  IF ( ALLOCATED(global_data_3d) ) DEALLOCATE(global_data_3d)
  IF ( ALLOCATED(global_data_4d) ) DEALLOCATE(global_data_4d)




  RETURN

END SUBROUTINE read_dump
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE write_dump()

!Science variables
  USE model_grid_mod, ONLY:                                                   &
    global_land_pts, latitude, longitude

  USE ancil_info, ONLY:                                                       &
    dim_cs1, frac_surft, land_pts, nsurft, nsoilt, dim_cslayer

  USE jules_surface_types_mod, ONLY:                                          &
    npft, ntype, ncpft

  USE prognostics, ONLY:                                                      &
    canht_pft, canopy_surft, cs_pool_soilt, gs_gb, lai_pft,                   &
    nsnow_surft, rgrain_surft, rgrainl_surft,                                 &
    rho_snow_grnd_surft, sice_surft, sliq_surft,                              &
    snow_grnd_surft, snow_surft, snowdepth_surft,                             &
    t_soil_soilt, tsnow_surft, tstar_surft, seed_rain,                        &
    ds_surft, tsoil_deep_gb, tsurf_elev_surft,                                &
    frac_agr_prev_gb,                                                         &
    wood_prod_slow_gb, wood_prod_med_gb,                                      &
    wood_prod_fast_gb, n_inorg_soilt_lyrs, ns_pool_gb,                        &
    frac_past_prev_gb

  USE soil_ecosse_vars_mod, ONLY:                                             &
    n_amm_soilt, n_nit_soilt

  USE crop_vars_mod, ONLY:                                                    &
    dvi_cpft, rootc_cpft, harvc_cpft, reservec_cpft,                          &
    croplai_cpft, cropcanht_cpft, sthu_irr_soilt, sow_date_cpft, tt_veg_cpft, &
    tt_rep_cpft, latestharv_date_cpft,                                        &
    irrtiles, frac_irr_all_tiles, nirrtile, frac_irr_all

  USE imogen_progs, ONLY:                                                     &
    co2_ppmv, co2_change_ppmv, dtemp_o, fa_ocean

  USE trifctl, ONLY:                                                          &
    cv_gb, frac_agr_gb

  USE trif_vars_mod, ONLY:                                                    &
    frac_past_gb

  USE jules_snow_mod, ONLY:                                                   &
    nsmax

  USE jules_soil_mod, ONLY:                                                   &
    sm_levels, ns_deep

  USE p_s_parms, ONLY:                                                        &
    sthu_soilt, sthf_soilt, albsoil_soilt, bexp_soilt, sathh_soilt,           &
     satcon_soilt, smvcst_soilt, smvccl_soilt, smvcwt_soilt, hcap_soilt,      &
    hcon_soilt, clay_soilt, soil_ph_soilt

  USE top_pdm, ONLY:                                                          &
    sthzw_soilt, zw_soilt, fexp_soilt, ti_mean_soilt, ti_sig_soilt

  USE fire_mod, ONLY:                                                         &
    fire_prog

  USE metstats_mod, ONLY:                                                     &
    metstats_prog

  USE aero, ONLY:                                                             &
    co2_mmr

  USE jules_rivers_mod, ONLY:                                                 &
    rivers_sto_rp, rfm_surfstore_rp,                                          &
    rfm_substore_rp, rfm_flowin_rp, rfm_bflowin_rp

!Others
  USE io_constants, ONLY:                                                     &
    max_file_name_len, max_dim_var, mode_write

  USE parallel_mod, ONLY:                                                     &
    master_task_id, is_master_task, gather_land_field

  USE model_interface_mod, ONLY:                                              &
    identifier_len

  USE dictionary_mod, ONLY:                                                   &
    dict, dict_create, dict_set, dict_get, dict_has_key, dict_free

  USE string_utils_mod, ONLY:                                                 &
    to_string

  USE file_mod, ONLY:                                                         &
    file_handle, file_open, file_def_dim, file_def_var,                       &
    file_enddef, file_write_var, file_close

  USE output_mod, ONLY:                                                       &
    output_dir, run_id

  USE model_time_mod, ONLY:                                                   &
    current_time, is_spinup, spinup_cycle

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a dump file for the current timestep
!   Note that the writing of the dump is done by the master task with the
!   values gathered from other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Local parameters.
  LOGICAL, PARAMETER :: l_reading_false = .FALSE.
    ! A value of .FALSE. that is passed to argument l_reading of subroutine
    ! get_dim_info to show that it is being called in connection with writing
    ! (not reading) a dump.

! Work variables
  CHARACTER(LEN=max_file_name_len) :: file_name
                                      ! The filename to use for the dump file
  CHARACTER(LEN=max_file_name_len) :: dt_string
                                      ! The datetime string to use in the file
                                      ! name
  CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                                      ! The model identifiers for the variables
                                      ! to put in the dump
  CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                               ! The variable identifiers of the ancil
                               ! variables (not used in this subroutine)

  TYPE(file_handle) :: file  ! The dump file

  INTEGER :: nvars  ! The number of variables we are processing
  INTEGER :: nvars_from_ancil

! Variables used when defining dimensions
  TYPE(dict) :: file_dim_ids  ! Dictionary of the dimensions that have been
                              ! defined
                              ! Maps dim_name => dim_id

  INTEGER :: ndims  ! The number of levels dims for the current variable
  CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
                    ! The levels dimension names for the current variable
  INTEGER :: dim_sizes(max_dim_var)
                    ! The sizes of the levels dims for the current variable
  INTEGER :: dim_ids(max_dim_var)
                    ! The ids in file of the levels dims for the current
                    ! variable
  INTEGER :: var_ids(max_var_dump)
                        ! The ids of the variables in the dump file

  INTEGER :: i, j, m, n, mi  ! Loop counters

  REAL :: frac_irr_all_tiles_real
  REAL :: irrtiles_real(npft)
  REAL :: nirrtile_real

! Arrays to hold global land points version of data gathered in master task
! before writing
  REAL, ALLOCATABLE :: global_data_1d(:)  ! For data with no vertical levels
  REAL, ALLOCATABLE :: global_data_2d(:,:)   ! With one vertical level
  REAL, ALLOCATABLE :: global_data_3d(:,:,:) ! With two "vertical" levels
                                          ! I.E. snow variables or soil C/N
  REAL, ALLOCATABLE :: global_data_4d(:,:,:,:)

  LOGICAL, PARAMETER :: l_output_mode = .TRUE.

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Get the list of identifiers that we are going to output
!-----------------------------------------------------------------------------
  CALL required_vars_for_configuration(nvars, identifiers,                    &
                                       nvars_from_ancil, vars_from_ancil,     &
                                       l_output_mode)

! Add latitude and longitude to the list to help offline inspection of the
! dump file, ie they are diagnostics, not prognostics or ancillaries.
! Lat & lon will not be read in from the dump to prevent confusion with the
! model grid namelists

  nvars = nvars + 1
  identifiers(nvars) = 'latitude'
  nvars = nvars + 1
  identifiers(nvars) = 'longitude'

!-----------------------------------------------------------------------------
! In the master task only, we open a new file and define the required
! dimensions and variables
!-----------------------------------------------------------------------------
  IF ( is_master_task() ) THEN
!-----------------------------------------------------------------------------
! Generate the file name that we want to use and open the file
!-----------------------------------------------------------------------------
! File name starts with run id + indicator of a dump file
    file_name = TRIM(run_id) // ".dump."

! Include the current spinup cycle if there is one
    IF ( is_spinup )                                                          &
      file_name = TRIM(file_name) //                                          &
                  "spin" // TRIM(to_string(spinup_cycle)) // "."

! Then current date and time
    WRITE(dt_string, '(I4.4,I2.2,I2.2)') current_time%year,                   &
                                         current_time%month,                  &
                                         current_time%day
    dt_string = TRIM(dt_string) // "." // TRIM(to_string(current_time%time))
    file_name = TRIM(file_name) // TRIM(dt_string)

! Add the extension based on dump format
    SELECT CASE ( dump_format )
      CASE ( format_ascii )
        file_name = TRIM(file_name) // ".asc"

      CASE ( format_ncdf )
        file_name = TRIM(file_name) // ".nc"

      CASE DEFAULT
        CALL log_fatal("write_dump",                                          &
                       "Unrecognised file format - " // TRIM(dump_format))
    END SELECT

! Prepend the output directory
    file_name = TRIM(output_dir) // "/" // TRIM(file_name)

! We use the lowest level file API here, as we don't want to impose a grid
    file = file_open(file_name, mode_write)

!-----------------------------------------------------------------------------
! Create the dimensions and variables
!-----------------------------------------------------------------------------
    file_dim_ids = dict_create(max_dim_dump, INT(1))

    DO i = 1,nvars

      !-----------------------------------------------------------------------
      ! Get information about the dimensions used by the variable.
      ! The argument l_reading_false shows that we are writing (not reading) a
      ! dump.
      !-----------------------------------------------------------------------
      CALL get_dim_info( l_reading_false, identifiers(i), ndims,  dim_sizes,  &
                         dim_names )

!-----------------------------------------------------------------------------
! Define the dimensions if they have not already been defined
! We use a dictionary to keep track of defined dimension ids
!
! At the same time, gather up the dimension ids needed by the current variable
!-----------------------------------------------------------------------------
      DO j = 1,ndims
! If it has not yet been defined, define the dimension, storing its id
        IF ( .NOT. dict_has_key(file_dim_ids, dim_names(j)) )                 &
          CALL dict_set(                                                      &
            file_dim_ids, dim_names(j),                                       &
            file_def_dim(file, dim_names(j), dim_sizes(j))                    &
          )

! Get the dimension id from the dict and add it to the list for this variable
        CALL dict_get(file_dim_ids, dim_names(j), dim_ids(j))
      END DO

!-----------------------------------------------------------------------------
! Define the variable, saving the id in the file for later
!-----------------------------------------------------------------------------
      var_ids(i) = file_def_var(file, identifiers(i), dim_ids(1:ndims),       &
                                .FALSE.)

    END DO

!-----------------------------------------------------------------------------
! We have finished defining things
!-----------------------------------------------------------------------------
    CALL file_enddef(file)
    CALL dict_free(file_dim_ids)

  END IF  ! MASTER TASK


!-----------------------------------------------------------------------------
! Gather data from other tasks and write it to file
!-----------------------------------------------------------------------------
! Allocate the global data arrays
  IF ( is_master_task() ) THEN
    ALLOCATE(global_data_1d(global_land_pts))
    ALLOCATE(global_data_2d(global_land_pts, MAX(npft, dim_cs1, sm_levels,  &
                                                 nsurft, ntype, ns_deep,    &
                                                 nsoilt)))
    ALLOCATE(global_data_3d(global_land_pts,                                &
                            MAX(nsurft, dim_cslayer,nsoilt),                &
                            MAX(nsmax, dim_cs1, sm_levels)))
    ALLOCATE(global_data_4d(global_land_pts, nsoilt,                        &
                            MAX(nsurft, dim_cslayer),                       &
                            MAX(nsmax, dim_cs1,sm_levels)))
  ELSE
    ALLOCATE(global_data_1d(1))
    ALLOCATE(global_data_2d(1,1))
    ALLOCATE(global_data_3d(1,1,1))
  END IF

  DO i = 1,nvars
! Gather the variables into a global array to write to file
! Note that gather_land_field can only gather one land_pts array at a time,
! so to gather variables with multiple levels we must loop

    CALL log_info("write_dump", identifiers(i))

    SELECT CASE ( identifiers(i) )
      CASE ( 'gs' )
        CALL gather_land_field(gs_gb, global_data_1d)

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'sthzw' )
        CALL gather_land_field(sthzw_soilt(:,1), global_data_1d)

      CASE ( 'sthzw_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(sthzw_soilt(:,m), global_data_2d(:,m))
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'zw' )
        CALL gather_land_field(zw_soilt(:,1), global_data_1d)

      CASE ( 'zw_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(zw_soilt(:,m), global_data_2d(:,m))
        END DO

      CASE ( 'cv' )
        CALL gather_land_field(cv_gb, global_data_1d)

      CASE ( 'frac_agr_prev' )
        CALL gather_land_field(frac_agr_prev_gb, global_data_1d)

      CASE ( 'frac_past_prev' )
        CALL gather_land_field(frac_past_prev_gb, global_data_1d)

      CASE ( 'wood_prod_fast' )
        CALL gather_land_field(wood_prod_fast_gb, global_data_1d)

      CASE ( 'wood_prod_med' )
        CALL gather_land_field(wood_prod_med_gb, global_data_1d)

      CASE ( 'wood_prod_slow' )
        CALL gather_land_field(wood_prod_slow_gb, global_data_1d)

      CASE ( 'latitude' )
        CALL gather_land_field(latitude, global_data_1d)

      CASE ( 'longitude' )
        CALL gather_land_field(longitude, global_data_1d)

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'n_inorg' )
        DO n = 1,dim_cslayer
          CALL gather_land_field(n_inorg_soilt_lyrs(:,1,n),                   &
                                 global_data_2d(:,n))
        END DO

      CASE ( 'n_inorg_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL gather_land_field(n_inorg_soilt_lyrs(:,m,n),                 &
                                   global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'canht' )
        DO n = 1,npft
          CALL gather_land_field(canht_pft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'lai' )
        DO n = 1,npft
          CALL gather_land_field(lai_pft(:,n), global_data_2d(:,n))
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'cs' )
        DO n = 1,dim_cs1
          DO m = 1,dim_cslayer
            CALL gather_land_field(cs_pool_soilt(:,1,m,n),                    &
                                   global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'cs_soilt' )
        DO mi = 1,nsoilt
          DO n = 1,dim_cs1
            DO m = 1,dim_cslayer
              CALL gather_land_field(cs_pool_soilt(:,mi,m,n),                 &
                                     global_data_4d(:,mi,m,n))
            END DO
          END DO
        END DO

      CASE ( 'cropdvi' )
        DO n = 1,ncpft
          CALL gather_land_field(dvi_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'croprootc' )
        DO n = 1,ncpft
          CALL gather_land_field(rootc_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'cropharvc' )
        DO n = 1,ncpft
          CALL gather_land_field(harvc_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'cropreservec' )
        DO n = 1,ncpft
          CALL gather_land_field(reservec_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'croplai' )
        DO n = 1,ncpft
          CALL gather_land_field(croplai_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'cropcanht' )
        DO n = 1,ncpft
          CALL gather_land_field(cropcanht_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'ns' )
        DO n = 1,dim_cs1
          DO m = 1,dim_cslayer
            CALL gather_land_field(ns_pool_gb(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'sthuf' )
! sthuf is held in sthu until it is processed
        DO n = 1,sm_levels
          CALL gather_land_field(sthu_soilt(:,1,n) + sthf_soilt(:,1,n),       &
                                 global_data_2d(:,n))
        END DO

      CASE ( 'sthuf_soilt' )
! sthuf is held in sthu until it is processed
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(sthu_soilt(:,m,n) + sthf_soilt(:,m,n),     &
                                  global_data_3d(:,m,n))
          END DO
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 't_soil' )
        DO n = 1,sm_levels
          CALL gather_land_field(t_soil_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 't_soil_soilt' )
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(t_soil_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'tsoil_deep' )
        DO n = 1,ns_deep
          CALL gather_land_field(tsoil_deep_gb(:,n), global_data_2d(:,n))
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'sthu_irr' )
        DO n = 1,sm_levels
          CALL gather_land_field(sthu_irr_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'sthu_irr_soilt' )
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(sthu_irr_soilt(:,m,n),                     &
                                   global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'canopy' )
        DO n = 1,nsurft
          CALL gather_land_field(canopy_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'nsnow' )
        DO n = 1,nsurft
          CALL gather_land_field(REAL(nsnow_surft(:,n)), global_data_2d(:,n))
        END DO

      CASE ( 'rgrain' )
        DO n = 1,nsurft
          CALL gather_land_field(rgrain_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'rho_snow' )
        DO n = 1,nsurft
          CALL gather_land_field(rho_snow_grnd_surft(:,n),                    &
                                 global_data_2d(:,n))
        END DO

      CASE ( 'snow_tile' )
        DO n = 1,nsurft
          CALL gather_land_field(snow_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'snow_depth' )
        DO n = 1,nsurft
          CALL gather_land_field(snowdepth_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'snow_grnd' )
        DO n = 1,nsurft
          CALL gather_land_field(snow_grnd_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'tstar_tile' )
        DO n = 1,nsurft
          CALL gather_land_field(tstar_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'tsurf_elev_surft' )
        DO n = 1,nsurft
          CALL gather_land_field(tsurf_elev_surft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'rgrainl' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL gather_land_field(rgrainl_surft(:,m,n),                      &
                                   global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'snow_ds' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL gather_land_field(ds_surft(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'snow_ice' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL gather_land_field(sice_surft(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO


      CASE ( 'snow_liq' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL gather_land_field(sliq_surft(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO


      CASE ( 'tsnow' )
        DO n = 1,nsmax
          DO m = 1,nsurft
            CALL gather_land_field(tsnow_surft(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      ! Cases for metstats variables
      CASE('temp_max_00h_r')
        CALL gather_land_field(metstats_prog(:)%temp_max_00h%run,             &
                               global_data_1d)

      CASE('temp_ave_00h_r')
        CALL gather_land_field(metstats_prog(:)%temp_ave_00h%run,             &
                               global_data_1d)

      CASE('prec_tot_00h_r')
        CALL gather_land_field(metstats_prog(:)%prec_tot_00h%run,             &
                               global_data_1d)

      CASE('prec_tot_12h_r')
        CALL gather_land_field(metstats_prog(:)%prec_tot_12h%run,             &
                               global_data_1d)

      CASE('rhum_min_00h_r')
        CALL gather_land_field(metstats_prog(:)%rhum_min_00h%run,             &
                               global_data_1d)

      CASE('dewp_ave_00h_r')
        CALL gather_land_field(metstats_prog(:)%dewp_ave_00h%run,             &
                               global_data_1d)

      CASE('wind_ave_00h_r')
        CALL gather_land_field(metstats_prog(:)%wind_ave_00h%run,             &
                               global_data_1d)

      CASE('temp_max_00h')
        CALL gather_land_field(metstats_prog(:)%temp_max_00h%fin,             &
                               global_data_1d)

      CASE('temp_ave_00h')
        CALL gather_land_field(metstats_prog(:)%temp_ave_00h%fin,             &
                               global_data_1d)

      CASE('temp_pnt_12h')
        CALL gather_land_field(metstats_prog(:)%temp_pnt_12h%fin,             &
                               global_data_1d)

      CASE('prec_tot_00h')
        CALL gather_land_field(metstats_prog(:)%prec_tot_00h%fin,             &
                               global_data_1d)

      CASE('prec_tot_12h')
        CALL gather_land_field(metstats_prog(:)%prec_tot_12h%fin,             &
                               global_data_1d)

      CASE('rhum_min_00h')
        CALL gather_land_field(metstats_prog(:)%rhum_min_00h%fin,             &
                               global_data_1d)

      CASE('rhum_pnt_12h')
        CALL gather_land_field(metstats_prog(:)%rhum_pnt_12h%fin,             &
                               global_data_1d)

      CASE('dewp_ave_00h')
        CALL gather_land_field(metstats_prog(:)%dewp_ave_00h%fin,             &
                               global_data_1d)

      CASE('wind_ave_00h')
        CALL gather_land_field(metstats_prog(:)%wind_ave_00h%fin,             &
                               global_data_1d)

      CASE('wind_pnt_12h')
        CALL gather_land_field(metstats_prog(:)%wind_pnt_12h%fin,             &
                               global_data_1d)

! Cases for Fire variables
      CASE( 'fire_mcarthur_r_dr' )
        CALL gather_land_field(fire_prog(:)%mcarthur%r_dr, global_data_1d)

      CASE( 'fire_mcarthur_n_dr' )
        CALL gather_land_field(fire_prog(:)%mcarthur%n_dr, global_data_1d)

      CASE( 'fire_canadian_ffmc' )
        CALL gather_land_field(fire_prog(:)%canadian%ffmc, global_data_1d)

      CASE( 'fire_canadian_ffmc_mois' )
        CALL gather_land_field(fire_prog(:)%canadian%ffmc_mois,               &
                               global_data_1d)

      CASE( 'fire_canadian_dmc' )
        CALL gather_land_field(fire_prog(:)%canadian%dmc, global_data_1d)

      CASE( 'fire_canadian_dc' )
        CALL gather_land_field(fire_prog(:)%canadian%dc, global_data_1d)

      CASE( 'fire_nesterov' )
        CALL gather_land_field(fire_prog(:)%nesterov%findex, global_data_1d)

! ECOSSE variables
      CASE ( 'n_amm' )
        DO n = 1,dim_cslayer
          CALL gather_land_field(n_amm_soilt(:,1,n),global_data_2d(:,n))
        END DO

      CASE ( 'n_amm_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL gather_land_field(n_amm_soilt(:,m,n),global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'n_nit' )
        DO n = 1,dim_cslayer
          CALL gather_land_field(n_nit_soilt(:,1,n),global_data_2d(:,n))
        END DO

      CASE ( 'n_nit_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL gather_land_field(n_nit_soilt(:,m,n),global_data_3d(:,m,n))
          END DO
        END DO

! Since each task runs its own version of IMOGEN, we just use the values from
! the master task
      CASE ( 'co2_ppmv', 'co2_change_ppmv', 'dtemp_o', 'fa_ocean',            &
             'seed_rain' )
! Nothing to do

      CASE ( 'rivers_sto_rp', 'rfm_surfstore_rp', 'rfm_substore_rp',          &
             'rfm_flowin_rp', 'rfm_bflowin_rp' )
! Nothing to do

!-----------------------------------------------------------------------------
! Ancillary variables
!-----------------------------------------------------------------------------

      !Frac ancil namelist
      CASE ( 'frac' )
        DO n = 1,ntype
          CALL gather_land_field(frac_surft(:,n), global_data_2d(:,n))
        END DO

      !Soil properties ancil namelist
      !Cases if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'b      ')
        DO n = 1,sm_levels
          CALL gather_land_field(bexp_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'b_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(bexp_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'sathh  ')
        DO n = 1,sm_levels
          CALL gather_land_field(sathh_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'sathh_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(sathh_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'satcon ')
        DO n = 1,sm_levels
          CALL gather_land_field(satcon_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'satcon_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(satcon_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'sm_sat ')
        DO n = 1,sm_levels
          CALL gather_land_field(smvcst_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'sm_sat_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(smvcst_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'sm_crit')
        DO n = 1,sm_levels
          CALL gather_land_field(smvccl_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'sm_crit_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(smvccl_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'sm_wilt')
        DO n = 1,sm_levels
          CALL gather_land_field(smvcwt_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'sm_wilt_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(smvcwt_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'hcap   ')
        DO n = 1,sm_levels
          CALL gather_land_field(hcap_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'hcap_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(hcap_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'hcon   ' )
        DO n = 1,sm_levels
          CALL gather_land_field(hcon_soilt(:,1,n), global_data_2d(:,n))
        END DO

      CASE ( 'hcon_soilt')
        DO m=1,nsoilt
          DO n = 1,sm_levels
            CALL gather_land_field(hcon_soilt(:,m,n), global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'albsoil' )
        CALL gather_land_field(albsoil_soilt(:,1), global_data_1d)

      CASE ( 'albsoil_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(albsoil_soilt(:,m), global_data_2d(:,m))
        END DO

      CASE ( 'clay' )
        DO n = 1,dim_cslayer
          CALL gather_land_field(clay_soilt(:,1,n),global_data_2d(:,n))
        END DO

      CASE ( 'clay_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL gather_land_field(clay_soilt(:,m,n),                         &
                                   global_data_3d(:,m,n))
          END DO
        END DO

      CASE ( 'soil_ph' )
        DO n = 1,dim_cslayer
          CALL gather_land_field(soil_ph_soilt(:,1,n),global_data_2d(:,n))
        END DO

      CASE ( 'soil_ph_soilt' )
        DO m = 1,nsoilt
          DO n = 1,dim_cslayer
            CALL gather_land_field(soil_ph_soilt(:,m,n),global_data_3d(:,m,n))
          END DO
        END DO

      !Topmodel ancillaries namelist
      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'fexp   ' )
        CALL gather_land_field(fexp_soilt(:,1), global_data_1d)

      CASE ( 'fexp_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(fexp_soilt(:,m), global_data_2d(:,m))
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'ti_mean' )
        CALL gather_land_field(ti_mean_soilt(:,1), global_data_1d)

      CASE ( 'ti_mean_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(ti_mean_soilt(:,m), global_data_2d(:,m))
        END DO

      !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
      CASE ( 'ti_sig ' )
        CALL gather_land_field(ti_sig_soilt(:,1), global_data_1d)

      CASE ( 'ti_sig_soilt' )
        DO m=1,nsoilt
          CALL gather_land_field(ti_sig_soilt(:,m), global_data_2d(:,m))
        END DO

      !Agric ancillaries namelist
      CASE ( 'frac_agr' )
        CALL gather_land_field(frac_agr_gb, global_data_1d)

      CASE ( 'frac_past' )
        CALL gather_land_field(frac_past_gb, global_data_1d)

      !Crop props ancillaries namelist
      CASE ( 'cropsowdate       ' )
        DO n = 1,ncpft
          CALL gather_land_field(sow_date_cpft(:,n), global_data_2d(:,n))
        END DO 

      CASE ( 'croplatestharvdate' )
        DO n = 1,ncpft
          CALL gather_land_field(latestharv_date_cpft(:,n),                   &
                                 global_data_2d(:,n))
        END DO

      CASE ( 'cropttveg         ' )
        DO n = 1,ncpft
          CALL gather_land_field(tt_veg_cpft(:,n), global_data_2d(:,n))
        END DO

      CASE ( 'cropttrep         ')
        DO n = 1,ncpft
          CALL gather_land_field(tt_rep_cpft(:,n), global_data_2d(:,n))
        END DO

      !Irrigation ancillaries namelist
        CASE ( 'frac_irrig' )
          CALL gather_land_field(frac_irr_all(:,1), global_data_1d)

        CASE ( 'frac_irr_all_tiles' )
          ! Convert to a real
          IF ( frac_irr_all_tiles ) THEN
            frac_irr_all_tiles_real = 1.0
          ELSE
            frac_irr_all_tiles_real = 0.0
          END IF

        CASE ( 'irrtiles' )
          irrtiles_real = REAL(irrtiles(1:npft))

        CASE ( 'nirrtile' )
          nirrtile_real = REAL(nirrtile)

        !CO2 ancil namelist
        CASE ( 'co2_mmr' )
          !Nothing to do

      CASE DEFAULT
        CALL log_fatal("write_dump",                                          &
                       "No code to gather variable for dump - " //            &
                       TRIM(identifiers(i)))
    END SELECT

!-----------------------------------------------------------------------------
! In the master task, write the global data to file
!-----------------------------------------------------------------------------
    IF ( is_master_task() ) THEN

      SELECT CASE ( identifiers(i) )
! If it is a land_pts array with no levels associated,
! write the global_data_1d array
        CASE ( 'gs', 'sthzw', 'zw', 'cv', 'frac_agr_prev', 'frac_past_prev', &
               'wood_prod_fast', 'wood_prod_med', 'wood_prod_slow',          &
               'temp_max_00h_r', 'temp_ave_00h_r', 'prec_tot_00h_r',         &
               'prec_tot_12h_r', 'rhum_min_00h_r', 'dewp_ave_00h_r',         &
               'wind_ave_00h_r', 'temp_max_00h',   'temp_ave_00h',           &
               'temp_pnt_12h',   'prec_tot_00h',   'prec_tot_12h',           &
               'rhum_min_00h',   'rhum_pnt_12h',   'dewp_ave_00h',           &
               'wind_ave_00h',   'wind_pnt_12h',                             &
               'fire_mcarthur_r_dr', 'fire_mcarthur_n_dr',                   &
               'fire_canadian_ffmc', 'fire_canadian_ffmc_mois',              &
               'fire_canadian_dmc',  'fire_canadian_dc',                     &
               'fire_nesterov', 'latitude', 'longitude')
          CALL file_write_var(file, var_ids(i), global_data_1d)

        CASE ( 'sthzw_soilt', 'zw_soilt' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:nsoilt))

! If it is a variable with one levels dimension, write the appropriate number
! of levels to global_data_2d.
        CASE ( 'canht', 'lai' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:npft))

        CASE ( 'cropdvi', 'croprootc', 'cropharvc', 'cropreservec',           &
               'croplai', 'cropcanht' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:ncpft))

        CASE ( 'sthuf', 't_soil', 'sthu_irr' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_2d(:,1:sm_levels))

        CASE ( 'sthuf_soilt', 't_soil_soilt', 'sthu_irr_soilt' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_3d(:,1:nsoilt,1:sm_levels))

        CASE ( 'n_amm', 'n_nit', 'n_inorg')
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_2d(:,1:dim_cslayer))

        CASE ( 'n_amm_soilt', 'n_nit_soilt', 'n_inorg_soilt')
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_3d(:,1:nsoilt,1:dim_cslayer))

        CASE ( 'tsoil_deep' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:ns_deep))

        CASE ( 'canopy', 'nsnow', 'rgrain', 'rho_snow', 'snow_tile',          &
               'snow_depth', 'snow_grnd', 'tstar_tile', 'tsurf_elev_surft' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:nsurft))

! Snow and soil C/N variables with 2 levels dimensions.
        CASE ( 'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_3d(:,1:nsurft,1:nsmax))
        CASE ( 'cs','ns' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_3d(:,1:dim_cslayer,1:dim_cs1))

        CASE ( 'cs_soilt' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_4d(:,1:nsoilt,1:dim_cslayer,        &
                                             1:dim_cs1))

! Cases for IMOGEN variables
! Each task runs its own version of IMOGEN - we just write the master task's
! versions
        CASE ( 'co2_ppmv' )
          CALL file_write_var(file, var_ids(i), co2_ppmv)

        CASE ( 'co2_change_ppmv' )
          CALL file_write_var(file, var_ids(i), co2_change_ppmv)

        CASE ( 'dtemp_o' )
          CALL file_write_var(file, var_ids(i), dtemp_o)

        CASE ( 'fa_ocean' )
          CALL file_write_var(file, var_ids(i), fa_ocean)

        CASE ( 'seed_rain' )
          CALL file_write_var(file, var_ids(i), REAL(seed_rain))

! Cases for river routing variables
        CASE ( 'rivers_sto_rp' )
          CALL file_write_var(file, var_ids(i), rivers_sto_rp)

        CASE ( 'rfm_surfstore_rp' )
          CALL file_write_var(file, var_ids(i), rfm_surfstore_rp)

        CASE ( 'rfm_substore_rp' )
          CALL file_write_var(file, var_ids(i), rfm_substore_rp)

        CASE ( 'rfm_flowin_rp' )
          CALL file_write_var(file, var_ids(i), rfm_flowin_rp)

        CASE ( 'rfm_bflowin_rp' )
          CALL file_write_var(file, var_ids(i), rfm_bflowin_rp)

!-----------------------------------------------------------------------------
! Ancillary variables
!-----------------------------------------------------------------------------

        !Frac ancil namelist
        CASE ( 'frac' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:ntype))

        !Soil properties ancil namelist
        CASE ( 'b      ', 'sathh  ', 'satcon ', 'sm_sat ', 'sm_crit',         &
               'sm_wilt', 'hcap   ', 'hcon   ' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:sm_levels))

        CASE ( 'b_soilt', 'sathh_soilt', 'satcon_soilt', 'sm_sat_soilt',      &
               'sm_crit_soilt', 'sm_wilt_soilt', 'hcap_soilt', 'hcon_soilt' )
          CALL file_write_var(file, var_ids(i),                               &
                              global_data_3d(:,1:nsoilt,1:sm_levels))

        CASE ( 'albsoil' )
          CALL file_write_var(file, var_ids(i), global_data_1d)

        CASE ( 'albsoil_soilt' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:nsoilt))

        CASE ( 'clay', 'soil_ph' )
          CALL file_write_var(file, var_ids(i),                               &
               global_data_2d(:,1:dim_cslayer))

        CASE ( 'clay_soilt', 'soil_ph_soilt' )
          CALL file_write_var(file, var_ids(i),                               &
               global_data_3d(:,1:nsoilt,1:dim_cslayer))

        !Topmodel ancil namelist
        CASE ( 'fexp   ', 'ti_mean', 'ti_sig ' )
          CALL file_write_var(file, var_ids(i), global_data_1d)

        CASE ( 'fexp_soilt', 'ti_mean_soilt', 'ti_sig_soilt' )
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:nsoilt))

        !Agric ancil namelist
        CASE ( 'frac_agr' )
          CALL file_write_var(file, var_ids(i), global_data_1d)

        !Crop props ancillaries namelist
        CASE ( 'cropsowdate', 'cropttveg  ', 'cropttrep  ','croplatestharvdate')
          CALL file_write_var(file, var_ids(i), global_data_2d(:,1:ncpft))

        !Irrigation ancil namelist
        CASE ( 'frac_irrig' )
          CALL file_write_var(file, var_ids(i), global_data_1d)

        CASE ( 'frac_irr_all_tiles' )
          CALL file_write_var(file, var_ids(i), frac_irr_all_tiles_real)

        CASE ( 'irrtiles' )
          CALL file_write_var(file, var_ids(i), irrtiles_real)

        CASE ( 'nirrtile' )
          CALL file_write_var(file, var_ids(i), nirrtile_real)

        !CO2 ancil namelist
        CASE ( 'co2_mmr' )
          CALL file_write_var(file, var_ids(i), co2_mmr)

        CASE DEFAULT
          CALL log_fatal("write_dump",                                        &
                         "Unrecognised variable for dump - " //               &
                         TRIM(identifiers(i)))
      END SELECT
    END IF  ! MASTER TASK
  END DO

! We are done with the file and dictionaries
  IF ( is_master_task() ) CALL file_close(file)

  IF ( ALLOCATED(global_data_1d) ) DEALLOCATE(global_data_1d)
  IF ( ALLOCATED(global_data_2d) ) DEALLOCATE(global_data_2d)
  IF ( ALLOCATED(global_data_3d) ) DEALLOCATE(global_data_3d)
  IF ( ALLOCATED(global_data_4d) ) DEALLOCATE(global_data_4d)

  RETURN

END SUBROUTINE write_dump
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE get_dim_info( l_reading, identifier, ndims, dim_sizes, dim_names,  &
                         l_read_from_dump )

!-----------------------------------------------------------------------------
! Description:
!   Given a variable's identifier, return the number, sizes and names of its
!   dimensions. Used when reading or writing a dump file.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                         &
  dim_cs1, land_pts, nsurft, nsoilt, dim_cslayer

USE imogen_constants, ONLY:                                                   &
  n_olevs, nfarray

USE jules_rivers_mod, ONLY:                                                   &
  np_rivers

USE jules_soil_mod, ONLY:                                                     &
  sm_levels, ns_deep

USE jules_snow_mod, ONLY:                                                     &
  nsmax

USE jules_surface_types_mod, ONLY:                                            &
  npft, ntype, ncpft

USE model_grid_mod, ONLY:                                                     &
  global_land_pts

USE prognostics, ONLY:                                                        &
  seed_rain

USE string_utils_mod, ONLY:                                                   &
  to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                        &
  l_reading
    ! Flag indicating where this routine has been called from.
    ! T means the call is from read_dump.
    ! F means the call is from write_dump.

CHARACTER(len=*), INTENT(IN) ::                                               &
  identifier  ! The identifier.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                       &
  ndims,                                                                      &
    ! Number of dimensions for the variable.
  dim_sizes(:)
    ! Size of each dimension.

CHARACTER(len=*), INTENT(OUT) ::                                              &
  dim_names(:)
    ! Name of each dimension.

!-----------------------------------------------------------------------------
! Optional arguments.
!-----------------------------------------------------------------------------
LOGICAL, INTENT(OUT), OPTIONAL ::                                             &
  l_read_from_dump
    ! Flag passed to read_dump to indicate that a variable should be read from
    ! the dump file. This is TRUE for most variables, FALSE only for ancillary
    ! variables that were read from ancillary files (not the dump).
 
!-----------------------------------------------------------------------------
! If this is call from read_dump, check we have the optional argument.
! If it is present, set the default to TRUE.
!-----------------------------------------------------------------------------
IF ( l_reading ) THEN
  IF ( .NOT. PRESENT(l_read_from_dump) ) THEN
    CALL log_fatal("get_dim_info",                                            &
                   "l_read_from_dump must be present when called from " //    &
                   "read_dump.")
  ELSE
    l_read_from_dump = .TRUE.
  END IF
END IF

!-----------------------------------------------------------------------------

SELECT CASE ( identifier )

CASE ( 'gs', 'cv', 'frac_agr_prev', 'frac_past_prev',                         &
       'wood_prod_fast', 'wood_prod_med', 'wood_prod_slow',                   &
       'temp_max_00h_r', 'temp_ave_00h_r', 'prec_tot_00h_r',                  &
       'prec_tot_12h_r', 'rhum_min_00h_r', 'dewp_ave_00h_r',                  &
       'wind_ave_00h_r', 'temp_max_00h',   'temp_ave_00h',                    &
       'temp_pnt_12h',   'prec_tot_00h',   'prec_tot_12h',                    &
       'rhum_min_00h',   'rhum_pnt_12h',   'dewp_ave_00h',                    &
       'wind_ave_00h',   'wind_pnt_12h',                                      &
       'fire_mcarthur_r_dr', 'fire_mcarthur_n_dr',                            &
       'fire_canadian_ffmc', 'fire_canadian_ffmc_mois',                       &
       'fire_canadian_dmc',  'fire_canadian_dc',                              &
       'fire_nesterov', 'latitude', 'longitude')
  ndims = 1
  dim_names(1) = land_dim_name
  dim_sizes(1) = global_land_pts

CASE ( 'sthzw', 'zw' )
  ndims = 1
  dim_names(1) = land_dim_name
  dim_sizes(1) = global_land_pts

CASE ( 'sthzw_soilt', 'zw_soilt' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsoilt /)

CASE ( 'canht', 'lai' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, pft_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, npft /)

CASE ( 'cropdvi', 'croprootc', 'cropharvc', 'cropreservec',                   &
       'croplai', 'cropcanht' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, cpft_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, ncpft /)

CASE ( 'cs', 'ns' )
  ndims = 3
  dim_names(1:ndims) = (/ land_dim_name, sc_layer_dim_name,                   &
                      sc_pool_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, dim_cslayer, dim_cs1 /)

CASE ( 'cs_soilt', 'ns_soilt' )
  ndims = 4
  dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name,                      &
                    sc_layer_dim_name, sc_pool_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsoilt, dim_cslayer, dim_cs1 /)

CASE ( 'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
  ndims = 3
  dim_names(1:ndims) = (/ land_dim_name, tile_dim_name, snow_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsurft, nsmax /)

CASE ( 'sthuf', 't_soil', 'sthu_irr' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, soil_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, sm_levels /)

CASE ( 'sthuf_soilt', 't_soil_soilt', 'sthu_irr_soilt' )
  ndims = 3
  dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name, soil_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsoilt, sm_levels /)

CASE ( 'n_amm', 'n_nit', 'n_inorg' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, sc_layer_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, dim_cslayer /)

CASE ( 'n_amm_soilt', 'n_nit_soilt', 'n_inorg_soilt' )
  ndims = 3
  dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name,                      &
                    sc_layer_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsoilt, dim_cslayer /)

CASE ( 'tsoil_deep' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, bedrock_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, ns_deep /)

CASE ( 'canopy', 'nsnow', 'rgrain', 'rho_snow', 'snow_tile',                  &
       'snow_depth', 'snow_grnd', 'tstar_tile', 'tsurf_elev_surft' )
  ndims = 2
  dim_names(1:ndims) = (/ land_dim_name, tile_dim_name /)
  dim_sizes(1:ndims) = (/ global_land_pts, nsurft /)

! Cases for IMOGEN variables
CASE ( 'co2_ppmv', 'co2_change_ppmv' )
  ! Scalar variables are represented by an array of dimension 1.
  ndims = 1
  dim_names(1) = scalar_dim_name
  dim_sizes(1) = 1

CASE ( 'dtemp_o' )
  ndims = 1
  dim_names(1) = nolevs_dim_name
  dim_sizes(1) = n_olevs

CASE ( 'fa_ocean' )
  ndims = 1
  dim_names(1) = nfarray_dim_name
  dim_sizes(1) = nfarray

CASE ( 'seed_rain' )
  ndims = 1
  dim_names(1) = seed_dim_name
  dim_sizes(1) = SIZE(seed_rain)

! River routing variables.
CASE ( 'rivers_sto_rp', 'rfm_surfstore_rp', 'rfm_substore_rp',                &
       'rfm_flowin_rp', 'rfm_bflowin_rp' )
  ndims = 1
  dim_names(1) = p_rivers_dim_name
  dim_sizes(1) = np_rivers

!-----------------------------------------------------------------------------
! Ancillary variables.
! If this is a call from write_dump (l_reading=F) or a call from
! read_dump that indicates the variable is to be read from the dump
! (l_reading=T and, e.g. ancil_dump_read%frac=T) we return the
! information about the dimensions.
! If this is call about a variable that was read from an ancillary
! file (l_reading=T and, e.g. ancil_dump_read%frac=F) we set
! l_read_from_dump=F to show that the field should not be read from the dump.
! The decision about which of these paths should be followed is made by the
! function need_dims.
!-----------------------------------------------------------------------------

! Frac ancil namelist
CASE ( 'frac' )
  IF ( need_dims( l_reading, ancil_dump_read%frac ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, type_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, ntype /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! Soil properties ancil namelist
CASE ( 'b      ', 'sathh  ', 'satcon ', 'sm_sat ', 'sm_crit',                 &
       'sm_wilt', 'hcap   ', 'hcon   ' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, soil_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, sm_levels /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'b_soilt', 'sathh_soilt', 'satcon_soilt', 'sm_sat_soilt',              &
       'sm_crit_soilt', 'sm_wilt_soilt', 'hcap_soilt', 'hcon_soilt' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 3
    dim_names(1:ndims) = (/ land_dim_name,soilt_dim_name,soil_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, nsoilt, sm_levels /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'albsoil' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 1
    dim_names(1) = land_dim_name
    dim_sizes(1) = global_land_pts
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'albsoil_soilt' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, nsoilt /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'clay', 'soil_ph' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, sc_layer_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, dim_cslayer /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'clay_soilt', 'soil_ph_soilt' )
  IF ( need_dims( l_reading, ancil_dump_read%soil_props ) ) THEN
    ndims = 3
    dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name,                    &
                      sc_layer_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, nsoilt, dim_cslayer /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! Topmodel ancil namelist
CASE ( 'fexp   ', 'ti_mean', 'ti_sig ' )
  IF ( need_dims( l_reading, ancil_dump_read%top ) ) THEN
    ndims = 1
    dim_names(1) = land_dim_name
    dim_sizes(1) = global_land_pts
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'fexp_soilt', 'ti_mean_soilt', 'ti_sig_soilt' )
  IF ( need_dims( l_reading, ancil_dump_read%top ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, soilt_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, nsoilt /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! Agric ancillaries namelist
CASE ( 'frac_agr', 'frac_past' )
  IF ( need_dims( l_reading, ancil_dump_read%agric ) ) THEN
    ndims = 1
    dim_names(1) = land_dim_name
    dim_sizes(1) = global_land_pts
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! Crop props ancillaries namelist
CASE ( 'cropsowdate', 'cropttveg  ', 'cropttrep  ','croplatestharvdate' )
  IF ( need_dims( l_reading, ancil_dump_read%crop_props ) ) THEN
    ndims = 2
    dim_names(1:ndims) = (/ land_dim_name, cpft_dim_name /)
    dim_sizes(1:ndims) = (/ global_land_pts, ncpft /)
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! Irrigation ancillaries namelist
CASE ( 'frac_irrig' )
  IF ( need_dims( l_reading, ancil_dump_read%irrig ) ) THEN
    ndims = 1
    dim_names(1) = land_dim_name
    dim_sizes(1) = global_land_pts
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'frac_irr_all_tiles', 'nirrtile' )
  IF ( need_dims( l_reading, ancil_dump_read%irrig ) ) THEN
    ! Scalar variables are represented by an array of dimension 1.
    ndims = 1
    dim_names(1) = scalar_dim_name
    dim_sizes(1) = 1
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE ( 'irrtiles' )
  IF ( need_dims( l_reading, ancil_dump_read%irrig ) ) THEN
    ndims = 1
    dim_names(1) = pft_dim_name
    dim_sizes(1) = npft
  ELSE
    l_read_from_dump = .FALSE.
  END IF

! CO2 ancil namelist
CASE ( 'co2_mmr' )
  IF ( need_dims( l_reading, ancil_dump_read%co2 ) ) THEN
    ! Scalar variables are represented by an array of dimension 1.
    ndims = 1
    dim_names(1) = scalar_dim_name
    dim_sizes(1) = 1
  ELSE
    l_read_from_dump = .FALSE.
  END IF

CASE DEFAULT
  CALL log_fatal("get_dim_info",                                              &
                 "Unrecognised variable: " // TRIM( identifier ) //           &
                 " l_reading=" // to_string(l_reading) )

END SELECT

RETURN

END SUBROUTINE get_dim_info

!#############################################################################
!#############################################################################

FUNCTION need_dims( l_reading, l_read_ancil_from_dump ) RESULT( l_need_dims )

!-----------------------------------------------------------------------------
! Function to indicate whether the calling routine needs to get information
! about the dimensions of an ancillary variable during the reading or writing
! of a dump file.
!-----------------------------------------------------------------------------

IMPLICIT NONE

LOGICAL, INTENT(IN) ::                                                        &
  l_reading,                                                                  &
    ! T means we are reading a dump file.
    ! F means we are writing a dump file.
  l_read_ancil_from_dump
    ! T means this variable should be read from the dump (rather than having
    ! already been read from another file). Only used when l_reading=T.

! Function result.
LOGICAL ::                                                                    &
  l_need_dims
    ! T indicates that the calling routine should get information about the
    !   dimensions of the current ancillary variable.
    ! F indicates that no information is required (because this ancillary
    !   variable has already been read from a file).

!-----------------------------------------------------------------------------
! If this is a call while writing a dump (l_reading=F), or a call while
! reading a dump that indicates this is an ancillary variable that is to be
! read from the dump (l_reading=T and l_read_ancil_from_dump=T) we set
! l_need_dims=T to show that the program will need to get information about
! the variable's dimensions.
! If this is a call while reading a dump but for a variable that was previously
! read from an ancillary file (l_reading=T and l_read_ancil_from_dump=F) we set
! l_need_dims=F to show that there is no need to get information about
! the variable's dimensions.
!-----------------------------------------------------------------------------
IF ( ( l_reading .AND. l_read_ancil_from_dump ) .OR.  .NOT. l_reading ) THEN
  l_need_dims = .TRUE.
ELSE
  l_need_dims = .FALSE.
END IF

END FUNCTION need_dims


END MODULE dump_mod
