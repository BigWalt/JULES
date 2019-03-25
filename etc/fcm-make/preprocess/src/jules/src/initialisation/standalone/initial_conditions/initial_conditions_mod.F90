










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE initial_conditions_mod

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

PRIVATE
PUBLIC init_ic

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


SUBROUTINE init_ic(nml_dir)

  !Use in relevant subroutines
  USE freeze_soil_mod, ONLY: freeze_soil
  USE tilepts_mod,     ONLY: tilepts

  USE missing_data_mod, ONLY :                                                &
!  imported scalar parameters
     rmdi

  USE MPI

  USE dictionary_mod, ONLY : dict, dict_get, dict_free

  USE io_constants, ONLY : max_sdf_name_len, max_file_name_len, namelist_unit

  USE conversions_mod, ONLY : zerodegc

  USE water_constants_mod, ONLY : rho_water, dpsidt

  USE string_utils_mod, ONLY : to_string

  USE veg_param, ONLY : litc_norm

  USE dump_mod, ONLY : max_var_dump, required_vars_for_configuration,         &
                       read_dump

  USE input_mod, ONLY : fill_variables_from_file

  USE templating_mod, ONLY : tpl_has_var_name, tpl_substitute_var

  USE model_interface_mod, ONLY : identifier_len, populate_var, get_var_id

  USE ecosse_init_mod, ONLY : ecosse_init

  USE jules_hydrology_mod, ONLY : l_top

  USE jules_vegetation_mod, ONLY : l_phenol, l_triffid, frac_min, l_crop,     &
                                   l_recon, l_irrig_dmd, l_irrig_limit,       &
                                   l_nitrogen

  USE update_mod, ONLY : l_daily_disagg, precip_disagg_method, sthuf_soilt

  USE jules_surface_mod, ONLY : l_urban2t

  USE jules_soil_biogeochem_mod, ONLY :                                       &
    ! imported scalar parameters
    soil_model_ecosse, soil_model_rothc,                                      &
    ! imported scalar variables (IN)
    bio_hum_cn, l_layeredC, soil_bgc_model, tau_lit

  USE jules_soil_ecosse_mod, ONLY :                                           &
    ! imported array variables (IN)
    dz_soilc

  USE jules_surface_mod, ONLY : l_elev_land_ice

  USE jules_surface_types_mod, ONLY : npft, ncpft, nnpft,                     &
                                      ice, urban_canyon, urban_roof

  USE ancil_info, ONLY : land_pts, lice_pts, soil_pts, frac_surft,            &
                         lice_index, soil_index, surft_pts, surft_index,      &
                         l_lice_point, l_soil_point, nsoilt, soilt_pts,       &
                         soilt_index, frac_soilt, dim_cslayer

  USE prognostics, ONLY : smcl_soilt, t_soil_soilt, lai_pft, canht_pft,       &
                          seed_rain, n_inorg_avail_pft, n_inorg_soilt_lyrs,   &
                          ns_pool_gb, cs_pool_soilt, n_inorg_gb,              &
                          l_broadcast_soilt_in_mod => l_broadcast_soilt

  USE crop_vars_mod, ONLY : croplai_cpft, cropcanht_cpft, rootc_cpft

  USE crop_utils_mod, ONLY : croplai_min, cropcanht_min, croprootc_min

  USE p_s_parms, ONLY : sthu_soilt, sthf_soilt, smvcst_soilt, bexp_soilt,     &
                        sathh_soilt, hcon_soilt, satcon_soilt, sthu_min_soilt,&
                        clay_soilt

  USE pftparm, ONLY : rootd_ft

  USE jules_soil_mod, ONLY : dzsoil, sm_levels, l_tile_soil

  USE urban_param, ONLY : wrr_gb

  USE update_mod, ONLY : assign_irrig_fraction

  USE time_info_mod, ONLY : current_model_time

  USE root_frac_mod, ONLY: root_frac

  USE crop_vars_mod, ONLY : irrtiles, frac_irr_all_tiles, nirrtile,           &
                            startyr, startmon, startday, starttime

  USE jules_rivers_trip_mod, ONLY : regrid_routestore

  USE parallel_mod, ONLY : master_task_id, is_master_task

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the initial conditions for the run
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                           ! namelists

! Work variables
  INTEGER :: nvars_required     ! The number of variables that are
                                ! required in this configuration

  CHARACTER(LEN=identifier_len) :: required_vars(max_var_dump)
                                 ! The variable identifiers of the required
                                 ! variables

  INTEGER :: nvars_from_ancil
  CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                                 ! The variable identifiers of the ancil
                                 ! variables

  TYPE(dict) :: default_values  ! Dictionary mapping identifier => default value
                                ! Values should be REAL
  REAL :: default_value  ! Variable to contain values read from the dict

  INTEGER :: nvars_file   ! The number of variables that will be set
                          ! from the given file (template?)

  LOGICAL :: reset_done  ! Indicates if a reset of frac to frac_min was
                         ! performed

  LOGICAL :: firstcall = .TRUE.

  REAL :: urban_fraction  ! Used in partitioning of urban fraction into
                          ! canyon and roof

  REAL :: f_root_pft(sm_levels) ! Root fraction in each soil layer
                                ! used to initialise plant available inorg N

  INTEGER :: i,j,l,n,m  ! Loop counters

  INTEGER :: n_counted ! Counter for assigning soil_pts

  INTEGER :: year, month, day, time

  INTEGER :: error  ! Error indicator
  CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Definition of the jules_initial namelist
!-----------------------------------------------------------------------------
  LOGICAL :: dump_file  ! T - the given file is a dump file
                        ! F - the given file is not a dump file

  LOGICAL :: total_snow
                        ! Switch indicating how the snow model is initialised
                        !   T - only snow_tile is needed
                        !       If nsmax>0, the layer values are determined
                        !       by the model
                        !   F - all snow variables are supplied directly (what
                        !       these are depends on nsmax)

  CHARACTER(LEN=max_file_name_len) :: file
                        ! The name of the file (or variable name template) to
                        ! use for variables that need to be filled from file

  INTEGER :: nvars      ! The number of variables in this section
  CHARACTER(LEN=identifier_len) :: var(max_var_dump)
                        ! The variable identifiers of the variables
  LOGICAL :: use_file(max_var_dump)
                        !   T - the variable uses the file
                        !   F - the variable is set using a constant value
                        ! Defaults to T for every variable
  CHARACTER(LEN=max_sdf_name_len) :: var_name(max_var_dump)
                        ! The name of each variable in the file
  CHARACTER(LEN=max_sdf_name_len) :: tpl_name(max_var_dump)
                        ! The name to substitute in a template for each
                        ! variable
  REAL :: const_val(max_var_dump)
                        ! The constant value to use for each variable if
                        ! use_file = F for that variable
  LOGICAL :: in_init_nml, loaded_from_ancil
                        !Used for checking whether all req vars have been
                        !accounted for

  LOGICAL :: l_broadcast_soilt = .FALSE.
                        !IO local variable.
                        !Value then held in prognostics module.
                        !Switch to broadcast model state around all soil tiles.
                        !Only has an effect if l_tile_soil is true.
                        !Does not do anything with ancils- this is done by
                        !l_broadcast_ancils

  LOGICAL, PARAMETER :: l_output_mode = .FALSE.

  NAMELIST /jules_initial/ total_snow, dump_file, file,                       &
                           nvars, var, use_file, var_name, tpl_name,          &
                           const_val, l_broadcast_soilt

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
  nvars_required = 0
  nvars_file     = 0
  dump_file      = .FALSE.
  total_snow     = .FALSE.
  nvars          = 0
  use_file(:)    = .TRUE.  ! Default is to set every variable from file
  file           = ''      ! Empty file name.
  var(:)         = ''      ! Empty identifiers.
  var_name(:)    = ''      ! Empty variable names.
  tpl_name(:)    = ''      ! Empty template names.
  const_val(:)   = rmdi    ! Missing data value. This might later be replaced
                           ! by a default value from a dictionary.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_ic", "Reading JULES_INITIAL namelist...")

  OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'initial_conditions.nml'),&
                 STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=error,&
                 IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_ic",                                                 &
                   "Error opening namelist file initial_conditions.nml " //   &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  READ(namelist_unit, nml=jules_initial, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_ic",                                                 &
                   "Error reading namelist JULES_INITIAL " //                 &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  CLOSE(namelist_unit, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_ic",                                                 &
                   "Error closing namelist file initial_conditions.nml " //   &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Check that variable identifiers are not empty.
! Although we might later decide that the identifier is not required, for
! clarity we check here whether the claimed amount of information was
! provided.
!-----------------------------------------------------------------------------
  DO i=1,nvars
    IF ( LEN_TRIM(var(i)) == 0 )                                              &
      CALL log_fatal("init_ic",                                               &
                     "Insufficient values for var. " //                       &
                     "No name provided for var at position #" //              &
                     TRIM(to_string(i)) )
  END DO

!Copy across _io variable to it's proper place
l_broadcast_soilt_in_mod = l_broadcast_soilt

!-----------------------------------------------------------------------------
! Check switches against soil tiling
!-----------------------------------------------------------------------------
  IF ( l_broadcast_soilt ) THEN
    IF ( l_tile_soil ) THEN
      CALL log_info("init_ic",                                                &
                    "Non-soil tiled initial conditions will be broadcast " // &
                    "to all soil tiles. Users should consider appropriate" // &
                    " spinup period")
    ELSE
      CALL log_warn("init_ic",                                                &
                    "l_broadcast_soilt will have no effect: l_tile_soilt = F")
    END IF
  END IF

!-----------------------------------------------------------------------------
! Set up initial conditions using namelist values
!-----------------------------------------------------------------------------

! Set up the required variables - we get the list by calling a procedure in
! dump_mod that tells us the required prognostic variables for the current
! model configuration
! We indicate whether or not we are using total_snow, as this affects the
! required variables
! We also indicate that we do not want IMOGEN prognostics in the list, even
! if they are required, since they are initialised in init_imogen
! We do request the ancillaries
  CALL required_vars_for_configuration( nvars_required, required_vars,        &
                                        nvars_from_ancil, vars_from_ancil,    &
                                        l_output_mode,                        &
                                        total_snow, .FALSE., dump_file)

!Get a dictionary of default values in case they are needed
  default_values = get_default_ic_values(total_snow)

! If we are initialising from a dump and no variables were specified, then
! we assume that all variables will be initialised from the dump file
  IF ( dump_file .AND. nvars < 1 ) THEN
    CALL log_info("init_ic",                                                  &
                  "No variables given - will attempt to initialise all " //   &
                  "required variables from specified dump file")
    nvars = nvars_required
    var(1:nvars) = required_vars(1:nvars)
! Every variable will use the file
    use_file(:) = .TRUE.
! We don't need to set var_name, tpl_name or const_val since they are never
! used in the case of a dump file anyway

  ELSE
    !Check that all required variables are accounted for and fill in with
    !default values as needed
    !** NB- Default values are not available when using a dump file **
    DO i = 1, nvars_required

      !Test if the required variable is in the initial conditions namelist
      IF ( ANY(var(1:nvars) == required_vars(i)) ) THEN
        in_init_nml = .TRUE.
      ELSE
        in_init_nml = .FALSE.
      END IF

      !Test if the required variable has been loaded from an ancil
      IF ( ANY(vars_from_ancil(1:nvars_from_ancil) == required_vars(i)) ) THEN
        loaded_from_ancil = .TRUE.
      ELSE
        loaded_from_ancil = .FALSE.
      END IF

      !Log our assessment of whether the variable is accounted for
      IF ( in_init_nml .OR. loaded_from_ancil ) THEN
        CALL log_info("init_ic",                                              &
             "'" // TRIM(required_vars(i)) // "' accounted for- OK")
      ELSE
        CALL log_info("init_ic",                                              &
             "'" // TRIM(required_vars(i)) // "' not accounted for- " //      &
             "will attempt to set to default value")

        !Add the req variable to the var array along with with its default value
        nvars            = nvars + 1
        var(nvars)       = required_vars(i)
        use_file(nvars)  = .FALSE.
        CALL dict_get(default_values, required_vars(i), default_value)
        const_val(nvars) = default_value

      END IF !in_init_nml .OR. loaded_from_ancil
    END DO !nvars_required
  END IF !dump_file .AND. nvars < 1

!-----------------------------------------------------------------------------
! Check which variables we will be using and partition them into variables
! set to constant values and variables set from file
!-----------------------------------------------------------------------------
  DO i = 1,nvars
!-----------------------------------------------------------------------------
! If the variable is one of the required vars, then we will be using it
!-----------------------------------------------------------------------------
    IF ( ANY(required_vars(1:nvars_required) == var(i)) ) THEN
      IF ( use_file(i) ) THEN
        CALL log_info("init_ic",                                              &
                      "'" // TRIM(var(i)) // "' will be read from file")

! If the variable will be filled from file, register it here
        nvars_file = nvars_file + 1
! Since nvars_file <= i (so we will not overwrite unprocessed values)
! and we do not need the values from these arrays for any non-file variables
! from now on, we can just compress them down onto variables that are in the
! file
        var(nvars_file) = var(i)
        var_name(nvars_file) = var_name(i)
        tpl_name(nvars_file) = tpl_name(i)
      ELSE
        ! If the variable is being set as a constant, populate it here.
        ! First check that a value has been provided.
        IF ( ABS( const_val(i) - rmdi ) < EPSILON(1.0) )                      &
          CALL log_fatal("init_ic",                                           &
                         "No constant value provided for variable '"          &
                         // TRIM(var(i)) // "'" )

        CALL log_info("init_ic",                                              &
                      "'" // TRIM(var(i)) // "' will be set to a " //         &
                      "constant = " // to_string(const_val(i)))

        CALL populate_var(get_var_id(var(i)), CONST_VAL=const_val(i))
      END IF
    ELSE
! If the variable is not a required variable, warn about not using it
      CALL log_warn("init_ic",                                                &
                    "Provided variable '" // TRIM(var(i)) //                  &
                    "' is not required, so will be ignored")
    END IF
  END DO

!-----------------------------------------------------------------------------
! Set variables from file
!-----------------------------------------------------------------------------
  IF ( nvars_file > 0 ) THEN
!   Check that a file name was provided.
    IF ( LEN_TRIM(file) == 0 )                                                &
      CALL log_fatal("init_ic", "No file name provided")

    IF ( dump_file ) THEN
! If we are using a dump file, use read_dump to fill the variables
      CALL read_dump(file, var(1:nvars_file))
    ELSE IF ( tpl_has_var_name(file) ) THEN
! If we are using a non-dump file with a variable name template, loop through
! the variables setting one from each file
      DO i = 1,nvars_file
        ! Check that a template string was provided for this variable.
        IF ( LEN_TRIM(tpl_name(i)) == 0 )                                     &
          CALL log_fatal( "init_ic",                                          &
                          "No variable name template substitution " //        &
                          "provided for " // TRIM(var(i)) )
        CALL fill_variables_from_file(                                        &
          tpl_substitute_var(file, tpl_name(i)),                              &
          (/ var(i) /), (/ var_name(i) /)                                     &
        )
      END DO
    ELSE
! We are not using a file name template, so set all variables from the same
! file
      CALL fill_variables_from_file(                                          &
        file, var(1:nvars_file), var_name(1:nvars_file)                       &
      )
    END IF
  END IF

! Free the dictionary of default values as it is no longer required
  CALL dict_free(default_values)

!*****************************************************************************
! Further processing depending on options specified
!*****************************************************************************

!-----------------------------------------------------------------------------
! Set up derived soil values
!-----------------------------------------------------------------------------
! Set surface values.
  hcon_soilt(:,:,0)   = hcon_soilt(:,:,1)
  satcon_soilt(:,:,0) = satcon_soilt(:,:,1)

! Check that sathh_soilt>=0 - a common error!
  IF ( ANY( sathh_soilt(:,:,:) < 0.0 ) )                                      &
    CALL log_fatal("init_soil",                                               &
                   "sathh < 0.0 detected - for JULES, sathh is " //           &
                   "abs(saturated head)")

! Check that clay_soilt>=0 and <=1 - a common error!
SELECT CASE ( soil_bgc_model )
CASE( soil_model_ecosse, soil_model_rothc )
  IF ( ANY( clay_soilt(:,:,:) < 0.0 ) .OR.                                  &
       ANY( clay_soilt(:,:,:) > 1.0 ) ) THEN
    CALL log_fatal("init_soil", "clay <0 and/or >1.0 detected - for JULES")
  END IF
END SELECT

!-----------------------------------------------------------------------------
! Detect soil points.
! If top layer saturation moisture content > 0, this is a soil point.
! Note that land ice points are no longer assigned here.
!-----------------------------------------------------------------------------
  soil_pts        = 0
  soil_index(:)   = 0
  l_soil_point(:) = .FALSE.
  DO l = 1,land_pts
    IF (nsoilt == 1) THEN
      m = 1
      !Use exising logic
      IF ( smvcst_soilt(l,m,1) > EPSILON(smvcst_soilt(l,m,1)) ) THEN
        soil_pts             = soil_pts + 1
        soil_index(soil_pts) = l
        l_soil_point(l)      = .TRUE.
      END IF
    ELSE
      !Use soil-tile aware logic

      !Test every soil tile for the smvcst condition
      n_counted = 0
      DO m=1,nsoilt
        IF (smvcst_soilt(l,m,1) > EPSILON(smvcst_soilt(l,m,1))) THEN
          n_counted = n_counted + 1
        END IF
      END DO

      !If there are any tiles that pass the above, make sure it is all of them
      IF (n_counted == nsoilt) THEN
        soil_pts             = soil_pts + 1
        soil_index(soil_pts) = l
        l_soil_point(l)      = .TRUE.
      ELSE IF (n_counted > 0 .AND. n_counted /= nsoilt) THEN
        CALL log_fatal("init_ic",                                             &
                       "Gridboxes must be entirely soil or land ice")
      !ELSE
        !It's not a soil point. Do nothing
      END IF
    END IF !nsoilt == 1
  END DO !land_pts

  CALL log_info("init_soil",                                                  &
                "Number of soil points = " // TRIM(to_string(soil_pts)))

  IF ( soil_pts <= 0 )                                                        &
    CALL log_warn("init_soil",                                                &
                  "There are no soil points - any land points are land ice")

  !Set up up soilt tile points, index and frac
  IF ( nsoilt == 1 ) THEN
    m = 1
    !Direct mapping of soil tile points to soil points
    soilt_pts(1)     = soil_pts
    soilt_index(:,m) = soil_index

    !All fractions must equal 1 by definition
    frac_soilt(:,m)  = 1.0

  ELSE !nsoilt == nsurft

    DO m = 1,nsoilt

      !Set to zero initially to ensure we don't make a mess of gbm calculations
      soilt_pts(m) = 0
      DO l = 1, land_pts
        frac_soilt(l,m)  = 0.0
      END DO

      !Now work through each soil point, updating the number of points, index
      !and fraction as appropriate. 
      !Assumes all non-zero fractions are non-negligible
      DO l = 1, soil_pts
        i = soil_index(l)
        IF (frac_surft(i,m) > 0.0) THEN
          soilt_pts(m)     = soilt_pts(m) + 1
          soilt_index(i,m) = i
          frac_soilt(i,m)  = frac_surft(i,m)
        END IF
      END DO

    END DO
  END IF


! This accounts for some of the water than remains unfrozen when the soil is
! very cold
  DO i=1,soil_pts
    l = soil_index(i)
    sthu_min_soilt(l,:,:) = ( dpsidt * zerodegc / sathh_soilt(l,:,:) ) **     &
                            ( -1.0 / bexp_soilt(l,:,:) )
  END DO

! Calculate soil moisture content from wetness
  DO i = 1,sm_levels
    smcl_soilt(:,:,i) = rho_water * dzsoil(i) *                               &
                        sthuf_soilt(:,:,i) * smvcst_soilt(:,:,i)
  END DO

!-----------------------------------------------------------------------------
! Set the normalisation factor for vertical profile of litter input.
!-----------------------------------------------------------------------------
  IF ( l_layeredc .AND. soil_bgc_model == soil_model_rothc ) THEN
    ! Calculate dz * exp(-tau*z) for mid-point depth in each layer.
    litc_norm =  dzsoil(1) * EXP( -tau_lit * 0.5 * dzsoil(1) )
    DO j=2,sm_levels
      litc_norm = litc_norm + dzsoil(j) *                                     &
                  EXP( -tau_lit * ( SUM(dzsoil(1:j-1)) + 0.5 * dzsoil(j) ) )
    END DO
  ELSE IF ( soil_bgc_model == soil_model_ecosse ) THEN
    ! Calculate dz * exp(-tau*z) for mid-point depth in each layer.
    litc_norm = dz_soilc(1) * EXP(-tau_lit * dz_soilc(1) * 0.5) 
    DO j=2,dim_cslayer
      litc_norm = litc_norm + dz_soilc(j) *                                   &
                  EXP( -tau_lit *                                             &
                  ( SUM(dz_soilc(1:j-1)) + 0.5 * dz_soilc(j) ) )
    END DO
  END IF

!-----------------------------------------------------------------------------
! If using the two-tile urban schemes and only a combined urban fraction is
! given then split the fraction between the canyon and roof. This has to be
! done here instead of in init_urban to be consistent with triffid.
!-----------------------------------------------------------------------------
  IF ( l_urban2t ) THEN
    CALL log_info("init_ic",                                                  &
                  "Either URBAN-2T or MORUSES is in use - splitting urban " //&
                  "tile into canyon/roof if needed")

    DO l = 1, land_pts
      IF ( frac_surft(l,urban_canyon) > 0.0 .AND.                             &
           frac_surft(l,urban_roof) == 0.0 ) THEN
        urban_fraction             = frac_surft(l,urban_canyon)
        frac_surft(l,urban_canyon) = urban_fraction * wrr_gb(l)
        frac_surft(l,urban_roof)   = urban_fraction -                         &
                                     frac_surft(l,urban_canyon)
      ELSE IF ( frac_surft(l,urban_canyon) > 0.0 .AND.                        &
                frac_surft(l,urban_roof) > 0.0 .AND.                          &
                firstcall ) THEN
        CALL log_warn("init_ic",                                              &
           "WARNING: init_ic: URBAN-2T/M dump being used to initialise? " //  &
           " Splitting not done: Roof fraction already exists")
        firstcall = .FALSE.
      END IF
    END DO
  END IF

!-----------------------------------------------------------------------------
!   If using TRIFFID (with or without competing veg), ensure that fractions of
!   PFTs are not below minimum. Only do this over soil points - land ice
!   points should have zero fractions.
!-----------------------------------------------------------------------------
  IF ( l_triffid .AND. l_recon ) THEN
! Set up a flag to see if any points were reset to frac_min
    reset_done = .FALSE.

    DO j = 1,soil_pts
      i = soil_index(j)
      IF ( ANY( frac_surft(i,:) < frac_min ) ) THEN
! Reset all small values. Renormalisation is done later, but will fail if
! frac_min is sufficiently large. We only reset natural PFT tiles
        WHERE ( frac_surft(i,1:nnpft) < frac_min )
          frac_surft(i,1:nnpft) = frac_min
        END WHERE
        reset_done = .TRUE.
      END IF
    END DO

    IF ( reset_done )                                                         &
      CALL log_warn("init_ic",                                                &
                    "frac < frac_min at one or more points - reset to " //    &
                    "frac_min at those points")
  END IF

!-----------------------------------------------------------------------------
! Initialise soil bio and hum pools from soil carbon.
!-----------------------------------------------------------------------------
  IF ( soil_bgc_model == soil_model_rothc ) THEN
    !Triffid is incompatible with soil tiling at present. Hard code soil tile
    !index to 1 using m = 1
    m = 1
    DO n = 1,dim_cslayer
      ns_pool_gb(:,n,3) = cs_pool_soilt(:,m,n,3) / bio_hum_cn
      ns_pool_gb(:,n,4) = cs_pool_soilt(:,m,n,4) / bio_hum_cn
    END DO
  END IF

!-----------------------------------------------------------------------------
! Reconfigure ECOSSE.
! For now we always call this (regardless of l_recon) so as to deal with
! zero (or tiny) pools.
!-----------------------------------------------------------------------------
  IF ( soil_bgc_model == soil_model_ecosse ) CALL ecosse_init

!-----------------------------------------------------------------------------
! If using crop model, fill lai and canht from croplai_cpft and cropcanht if
! lai and
! canht_pft were not prognostics (whether they're prognostics is specified in
! required_vars_for_configuration)
!-----------------------------------------------------------------------------
  IF ( l_crop ) THEN
     IF ( l_phenol ) THEN
        DO n = 1,ncpft
           croplai_cpft(:,n) = lai_pft(:,n+nnpft)
        END DO
     ELSE
        DO n = 1,ncpft
           lai_pft(:,n+nnpft) = croplai_cpft(:,n)
        END DO
     END IF

     IF ( l_triffid ) THEN
        DO n = 1,ncpft
           cropcanht_cpft(:,n) = canht_pft(:,n+nnpft)
        END DO
     ELSE
        DO n = 1,ncpft
           canht_pft(:,n+nnpft) = cropcanht_cpft(:,n)
        END DO
     END IF

     ! ensure that crop leaf area index is not below minimum
     reset_done = .FALSE.
     DO i = 1,land_pts
        IF ( ANY( croplai_cpft(i,:) < croplai_min ) ) THEN
          ! Reset all small values. We only reset crop PFT tiles
          WHERE ( croplai_cpft(i,:) < croplai_min )
            croplai_cpft(i,:) = croplai_min
          END WHERE
          reset_done = .TRUE.
        END IF
     END DO

     IF ( reset_done ) THEN
        CALL log_warn("init_ic",                                              &
                    "croplai_cpft < croplai_min at one or more points - " //  &
                    "reset to croplai_min at those points")
        DO n = 1,ncpft
           lai_pft(:,n+nnpft) = croplai_cpft(:,n)
        END DO
     END IF

     ! ensure that crop canopy height is not below minimum
     reset_done = .FALSE.
     DO i = 1,land_pts
        IF ( ANY( cropcanht_cpft(i,:) < cropcanht_min ) ) THEN
          ! Reset all small values. We only reset crop PFT tiles
          WHERE ( cropcanht_cpft(i,:) < cropcanht_min )
            cropcanht_cpft(i,:) = cropcanht_min
          END WHERE
          reset_done = .TRUE.
        END IF
     END DO

     IF ( reset_done ) THEN
        CALL log_warn("init_ic",                                              &
                    "cropcanht < cropcanht_min at one or more points - " //   &
                    "reset to cropcanht_min at those points")
        DO n = 1,ncpft
           canht_pft(:,n+nnpft) = cropcanht_cpft(:,n)
        END DO
     END IF

     ! ensure that crop root carbon is not below minimum
     reset_done = .FALSE.
     DO i = 1,land_pts
        IF ( ANY( rootc_cpft(i,:) < croprootc_min ) ) THEN
          ! Reset all small values.
          WHERE ( rootc_cpft(i,:) < croprootc_min )
            rootc_cpft(i,:) = croprootc_min
          END WHERE
          reset_done = .TRUE.
        END IF
     END DO

     IF ( reset_done ) THEN
        CALL log_warn("init_ic",                                              &
                    "rootc_cpft < croprootc_min at one or more points - " //  &
                    "reset to croprootc_min at those points")
     END IF
  END IF

!-----------------------------------------------------------------------------
! Check that frac_surft sums to 1.0 (with a bit of leeway).
!-----------------------------------------------------------------------------
  IF ( l_recon ) THEN
    DO i = 1,land_pts
      IF ( ABS( SUM(frac_surft(i,:))-1.0 ) >= 1.0e-2 ) THEN
! If the discrepancy is big enough, bail
        CALL log_fatal("init_ic",                                             &
                       "frac does not sum to 1 at point " //                  &
                       TRIM(to_string(i)) //                                  &
                       " and the discrepancy is too big to be removed")
      ELSE IF ( ABS( SUM(frac_surft(i,:)) - 1.0 ) > 1.0e-4 ) THEN
! Correct a small discrepancy
        CALL log_warn("init_ic",                                              &
                      "frac does not sum to 1 at point " //                   &
                       TRIM(to_string(i)) //                                  &
                       " - removing small discrepancy")
! Ignore small discrepancies and (re)normalise
        frac_surft(i,:) = frac_surft(i,:) / SUM(frac_surft(i,:))
      END IF

! Ignore any discrepancy below the threshold completely
    END DO
  END IF

!-----------------------------------------------------------------------------
! Process the ice fraction field.
! Identify land ice points, only if the ice surface type is specified.
!-----------------------------------------------------------------------------
  lice_pts        = 0
  lice_index(:)   = 0
  l_lice_point(:) = .FALSE.

  IF ( ice > 0 .AND. .NOT.l_elev_land_ice ) THEN
    DO l = 1,land_pts
      IF ( frac_surft(l,ice) > 0.0 ) THEN
        ! This is a land ice point.
        lice_pts             = lice_pts + 1
        lice_index(lice_pts) = l
        l_lice_point(l)      = .TRUE.
        
        ! If reconfiguring, ensure pools are set (mostly to zero) over ice.
        IF ( l_recon ) THEN
          IF ( l_triffid ) THEN
            canht_pft(l,:)            = 0.0
            lai_pft(l,:)              = 0.0
            frac_surft(l,1:nnpft)     = 0.0
          END IF
          IF ( soil_bgc_model == soil_model_rothc ) THEN
            n_inorg_gb(l)             = 0.0
            n_inorg_soilt_lyrs(l,:,:) = 0.0
          END IF
          IF ( soil_bgc_model == soil_model_ecosse .OR.                       &
               soil_bgc_model == soil_model_rothc ) THEN
            cs_pool_soilt(l,:,:,:)    = 1.0e-6
            ns_pool_gb(l,:,:)         = 1.0e-6
          END IF
        END IF
        
! At present, land ice and soil points are mutually exclusive.
! Check this is not a soil point
        IF ( ANY(soil_index == l) ) THEN
          CALL log_fatal("init_ic",                                           &
                         "Land ice points and soil points are mutually " //   &
                         "exclusive")
        END IF

! Check that ice fraction is one (cannot have partial ice coverage).
        IF ( ABS(frac_surft(l,ice) - 1.0) > EPSILON(1.0) ) THEN
          CALL log_error("init_ic",                                           &
                         "Ice fraction must be 1 at an ice point")
        END IF

      END IF  !  land ice points
    END DO

  END IF  !  ice > 0 .AND. .NOT.l_elev_land_ice

!-----------------------------------------------------------------------------
! fall back to the old smvcst (NOT soil) criteria when using the tiled land ice
! subsurface - consistency with the UM
!-----------------------------------------------------------------------------
  IF ( l_elev_land_ice ) THEN
    !Only need to check the 1st soil tile as any problems will have been
    !picked up by the soil_pts test above. Set m = 1
    m = 1
    DO l = 1,land_pts
      IF ( smvcst_soilt(l,m,1) <= EPSILON(smvcst_soilt(l,m,1)) ) THEN
        lice_pts = lice_pts + 1
        lice_index(lice_pts) = l
        l_lice_point(l)      = .TRUE.
      END IF
!
!Reverse of soil criteria, so automatically exclusive.
!Partial ice fractions (e.g in each elevation class) allowed for land ice
!points here
!
    END DO
  END IF

  CALL log_info("init_ic",                                                    &
                "Number of land ice points = " // TRIM(to_string(lice_pts)))

!-----------------------------------------------------------------------------
! Check that all land points have been identified as either soil or ice
!-----------------------------------------------------------------------------
  IF ( soil_pts + lice_pts /= land_pts )                                      &
    CALL log_fatal("init_ic",                                                 &
                   "All points should be either soil or land ice points - " //&
                   "have land_pts = " // TRIM(to_string(land_pts)) //         &
                   " and soil_pts + lice_pts = " //                           &
                   TRIM(to_string(soil_pts + lice_pts)))

  ! Additional check using l_soil_point and l_lice_point
  DO l=1, land_pts
    IF (l_soil_point(l) .AND. l_lice_point(l))  THEN
      CALL log_fatal("init_ic",                                               &
                     "Point " // TRIM(to_string(l)) // " has been assigned" //&
                     " as both a soil and land ice point. Please review the"//&
                     " consistency for the soil and frac ancils.")
    ELSE IF (.NOT. l_soil_point(l) .AND. .NOT. l_lice_point(l))  THEN
      CALL log_fatal("init_ic",                                               &
                     "Point " // TRIM(to_string(l)) // " has been assigned" //&
                     " as neither a soil or land ice point. Please review"  //&
                     " the consistency for the soil and frac ancils.")
    END IF
  END DO

!-----------------------------------------------------------------------------
! Set up tile index
!-----------------------------------------------------------------------------
  CALL tilepts( land_pts, frac_surft, surft_pts, surft_index )

! For URBAN-2T or MORUSES: Check that urban canyons also have roofs
  IF ( l_urban2t ) THEN
    IF ( surft_pts(urban_canyon) /= surft_pts(urban_roof) )                   &
      CALL log_fatal("init_ic",                                               &
                     "URBAN-2T or MORUSES - # canyons /= # roofs")
  END IF

!-----------------------------------------------------------------------------
! Deal with "simple" initialisation of snow variables.
!-----------------------------------------------------------------------------
  IF ( total_snow ) CALL total_snow_init()

!-----------------------------------------------------------------------------
! Calculate frozen and unfrozen fractions of soil moisture.
! freeze_soil assumes bexp_soilt, sathh_soilt, smvcst_soilt are
! constant in soil column, so just give top layer values.
!-----------------------------------------------------------------------------
  DO m=1,nsoilt
    CALL freeze_soil (land_pts, sm_levels, bexp_soilt(:,m,1), dzsoil,         &
                      sathh_soilt(:,m,1), smcl_soilt(:,m,:),                  &
                      t_soil_soilt(:,m,:), smvcst_soilt(:,m,1),               &
                      sthu_soilt(:,m,:), sthf_soilt(:,m,:))
  END DO

!-----------------------------------------------------------------------------
! Finish initialising TOPMODEL
!-----------------------------------------------------------------------------
  IF ( l_top ) THEN
    !Calculate fitting parameters
    CALL calc_fit_fsat()
    CALL topmodel_init()
  END IF

!---------------------------------------------------------------------------
! Finish initialising irrigation
! Process pft names to be assigned irr fraction (if not all)
!---------------------------------------------------------------------------
  CALL current_model_time(year,month,day,time)
  startyr   = year
  startmon  = month
  startday  = day
  starttime = time

  IF ( l_irrig_dmd ) THEN
    CALL assign_irrig_fraction()
    IF ( l_irrig_limit ) THEN
      CALL regrid_routestore()
    END IF
  END IF
!-----------------------------------------------------------------------------
! If we are using the disaggregator with random rainfall, initialise the
! random seed
!-----------------------------------------------------------------------------
  IF ( l_daily_disagg .AND. precip_disagg_method > 1 ) THEN
    IF ( .NOT. dump_file ) THEN
! If not using a dump file, we need to initialise a new random seed on the
! master task and broadcast it to all other tasks
      IF ( is_master_task() ) THEN
        CALL RANDOM_SEED()
        CALL RANDOM_SEED( GET = seed_rain )
      END IF
      CALL mpi_bcast(seed_rain, SIZE(seed_rain), mpi_integer,                 &
                     master_task_id, mpi_comm_world, error)
    END IF

! Make sure that we put the seed, whether it came from the dump or was
! broadcast by the master task
    CALL RANDOM_SEED( PUT = seed_rain )
  END IF

  IF (l_nitrogen .AND. l_layeredC) THEN
    ! Initialise n_inorg_avail_pft so it can be leached before the first call
    ! to TRIFFID.
    DO n=1,npft
      CALL root_frac(n,sm_levels,dzsoil,rootd_ft(n),f_root_pft)
      DO i=1,soil_pts
        l = soil_index(i)
        DO j=1,dim_cslayer
          IF (nsoilt == 1) THEN
            m = 1
            n_inorg_avail_pft(l,n,j) = n_inorg_soilt_lyrs(l,m,j) * f_root_pft(j)
          ELSE
            n_inorg_avail_pft(l,n,j) = n_inorg_soilt_lyrs(l,n,j) * f_root_pft(j)
          END IF
        END DO
      END DO
    END DO
  END IF

  RETURN

END SUBROUTINE init_ic
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE total_snow_init()

  USE layersnow_mod,   ONLY: layersnow

  USE ancil_info, ONLY : land_pts, nsurft, surft_index, surft_pts,            &
                                           lice_index, lice_pts, nsoilt

  USE prognostics, ONLY : nsnow_surft, rgrain_surft, snow_surft, t_soil_soilt,&
                          rgrainl_surft, rho_snow_grnd_surft, sice_surft,     &
                          sliq_surft, snow_grnd_surft, snowdepth_surft,       &
                          tsnow_surft, ds_surft, tsurf_elev_surft

  USE jules_snow_mod, ONLY : nsmax, rho_snow_const, rho_snow_fresh, canSnowTile

  USE jules_surface_mod, ONLY : l_elev_land_ice

  USE jules_radiation_mod, ONLY : l_snow_albedo, l_embedded_snow

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the initial snow conditions from snow_surft only
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
  INTEGER :: i, j, k, n, m  ! Index variables

  REAL :: snow_on_ground(land_pts,nsurft)
                ! Snow considered to be on the ground (not in canopy) (kg m-2)
                ! This is all the snow

!-------------------------------------------------------------------------------
! Put all snow onto the ground and zero canopy snow.
! Currently all snow is held in snow_surft.
! For can_model=4 tiles, put snow into snow_grnd_surft and zero snow_surft.
!-------------------------------------------------------------------------------

! Save input value.
  snow_on_ground(:,:) = snow_surft(:,:)

! Initialise stores to zero.
  snow_grnd_surft(:,:) = 0.0
  snow_surft(:,:) = 0.0

! Initialise other variables with values that will be retained where there is
! no tile - using "sensible" values for when these are printed.
  snowdepth_surft(:,:) = 0.0
  IF ( nsmax < 1 ) THEN
    rho_snow_grnd_surft(:,:) = rho_snow_const
  ELSE
    rho_snow_grnd_surft(:,:) = rho_snow_fresh
    tsnow_surft(:,:,:) = 273.15
    ds_surft(:,:,:) = 0.0
    IF ( l_snow_albedo .OR. l_embedded_snow ) rgrainl_surft(:,:,:) = 0.0
  END IF

  DO n = 1,nsurft
    IF ( canSnowTile(n) ) THEN
      DO j = 1,surft_pts(n)
        i = surft_index(j,n)
        snow_grnd_surft(i,n) = snow_on_ground(i,n)
      END DO
    ELSE
      DO j = 1,surft_pts(n)
        i = surft_index(j,n)
        snow_surft(i,n) = snow_on_ground(i,n)
      END DO
    END IF
  END DO

!-------------------------------------------------------------------------------
! Set snow density, calculate snow depth and set temperature of snow to equal
! that of soil.
!-------------------------------------------------------------------------------

!==============================================================================
!**NOTICE REGARDING SOIL TILING**
!
!The following section facilitates the use of soil tiling. As implemented,
!there are two soil tiling options:
!
!nsoilt == 1
!Operate as with a single soil tile, functionally identical to JULES upto
! at least vn4.7 (Oct 2016)
! This means that a soilt variable being passed 'up' to the surface is
! broadcast to the surft variable (with weighting by frac if requred)
!
!nsoilt > 1
!Operate with nsoilt = nsurft, with a direct mapping between them
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!==============================================================================

  DO n=1,nsurft

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

    DO j=1,surft_pts(n)
      i = surft_index(j,n)
!     Use the constant (snowpack) density for nsmax=0 and if there is an
!     existing pack. If nsmax>0 and there is no pack, initialise the density
!     to the fresh snow value so that this value is used when/if a snowpack
!     next develops.
      IF ( nsmax == 0 .OR.                                                    &
           ( snow_on_ground(i,n) > EPSILON(snow_on_ground) ) ) THEN
        rho_snow_grnd_surft(i,n) = rho_snow_const
      ELSE
        rho_snow_grnd_surft(i,n) = rho_snow_fresh
      END IF
      snowdepth_surft(i,n) = snow_on_ground(i,n) / rho_snow_grnd_surft(i,n)
      IF ( nsmax > 0 ) THEN

        tsnow_surft(i,n,:) = t_soil_soilt(i,m,1)
        IF ( l_snow_albedo .OR. l_embedded_snow )                             &
          rgrainl_surft(i,n,:) = rgrain_surft(i,n)
      END IF
    END DO
  END DO

!==============================================================================
!**END NOTICE REGARDING SOIL TILING**
!==============================================================================

  IF (l_elev_land_ice) THEN
    DO j=1,lice_pts
      i = lice_index(j)
      IF ( nsmax > 0 ) THEN
        DO n=1,nsurft
          tsnow_surft(i,n,:) = tsurf_elev_surft(i,n)
        END DO
      END IF
    END DO
  END IF

  nsnow_surft(:,:) = 0
  IF ( nsmax > 0 ) THEN
!-------------------------------------------------------------------------------
! Calculate snow layer thicknesses.
!-------------------------------------------------------------------------------
    DO n=1,nsurft
      CALL layersnow(land_pts, surft_pts(n), surft_index(:,n),                &
                     snowdepth_surft(:,n), nsnow_surft(:,n), ds_surft(:,n,:))
    END DO

!-------------------------------------------------------------------------------
! Set layer frozen and liquid contents.
!-------------------------------------------------------------------------------
    sice_surft(:,:,:) = 0.0
    sliq_surft(:,:,:) = 0.0
    DO n = 1,nsurft
      DO j = 1,surft_pts(n)
        i = surft_index(j,n)
        DO k = 1,nsnow_surft(i,n)
          sice_surft(i,n,k) =  snow_on_ground(i,n) * ds_surft(i,n,k)          &
                                / snowdepth_surft(i,n)
        END DO
      END DO
    END DO
  END IF

  RETURN

END SUBROUTINE total_snow_init
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE topmodel_init()

  !Use in relevant subroutines
  USE calc_fsat_mod,           ONLY: calc_fsat
  USE calc_zw_inund_mod,       ONLY: calc_zw_inund
  USE calc_baseflow_jules_mod, ONLY: calc_baseflow_jules

  !Use in relevant varaibles
  USE ancil_info,           ONLY:                                             &
    land_pts, soil_pts, soil_index, nsoilt

  USE p_s_parms,            ONLY:                                             &
    bexp_soilt, satcon_soilt, sathh_soilt, sthf_soilt, sthu_soilt, smvcst_soilt

  USE jules_soil_mod,       ONLY:                                             &
    dzsoil, sm_levels

  USE top_pdm,              ONLY:                                             &
    fexp_soilt, gamtot_soilt, ti_mean_soilt, ti_sig_soilt, zw_soilt,          &
    fsat_soilt, fwetl_soilt, qbase_soilt, sthzw_soilt

  USE jules_hydrology_mod,  ONLY:                                             &
    l_wetland_unfrozen

  USE water_constants_mod,  ONLY:                                             &
    rho_water  !  density of pure water (kg/m3)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Finish initialising TOPMODEL by calculating surface saturated fraction
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
  LOGICAL, PARAMETER :: l_gamtot = .FALSE.  !  Switch for calculation of gamtot.
                                            !  .FALSE. so that calc_fsat
                                            !  calculates fsat_soilt.

  INTEGER :: i, j, n, m                    ! Index variables

  REAL :: qbase_l_soilt(land_pts,nsoilt,sm_levels+1)
    !Base flow from each layer (kg/m2/s).
  REAL :: top_crit_soilt(land_pts,nsoilt)
    !Critical topographic index required to calculate the surface saturation
    !fraction.
  REAL :: zdepth(0:sm_levels)
    !Lower soil layer boundary depth (m).
  REAL :: wutot_soilt(land_pts,nsoilt)
    !UNFROZEN to TOTAL fraction at ZW.
  REAL :: dumwutot_soilt(land_pts,nsoilt)
    !Dummy UNFROZEN to TOTAL fraction at ZW (always set to 1).
  REAL :: ksz_soilt(land_pts,nsoilt,0:sm_levels)
    !Saturated hydraulic conductivity for each layer (kg/m2/s).
  REAL :: dumsthf_soilt(land_pts,nsoilt,sm_levels)
    !Dummy Frozen soil moisture content of each layer as a fraction of
    !saturation (always set to 0).
  REAL :: smcl_soilt(land_pts,nsoilt,sm_levels)
    !Soil moisture content of each layer (kg/m2).
  REAL :: smclsat_soilt(land_pts,nsoilt,sm_levels)
    !Saturated soil moisture content of each layer (kg/m2).
  REAL :: zw_inund_soilt(land_pts,nsoilt)
    ! Adjusted Water table depth (m).

!-------------------------------------------------------------------------------
  zdepth(0) = 0.0
  DO n = 1,sm_levels
    zdepth(n) = zdepth(n - 1) + dzsoil(n)
  END DO

! Set values that are retained at non-soil points.
  fsat_soilt(:,:)     = 0.0
  fwetl_soilt(:,:)    = 0.0
  dumwutot_soilt(:,:) = 1.0

  IF ( soil_pts /= 0 ) THEN
    DO j = 1,soil_pts
      i = soil_index(j)
      DO n = 0,sm_levels
        ksz_soilt(i,:,n) = satcon_soilt(i,:,n)
      END DO
      DO n = 1,sm_levels
        smclsat_soilt(i,:,n) = rho_water * dzsoil(n) * smvcst_soilt(i,:,n)
        smcl_soilt(i,:,n)    = (sthu_soilt(i,:,n) + sthf_soilt(i,:,n))        &
                               * smclsat_soilt(i,:,n)
      END DO
    END DO

    IF (L_wetland_unfrozen)THEN
      DO m=1,nsoilt
        CALL calc_zw_inund(                                                   &
          land_pts,sm_levels,soil_pts,soil_index,zdepth,                      &
          bexp_soilt(:,m,1),sathh_soilt(:,m,1),smclsat_soilt(:,m,:),          &
          smcl_soilt(:,m,:),sthu_soilt(:,m,:),sthzw_soilt(:,m),zw_soilt(:,m), &
          zw_inund_soilt(:,m),wutot_soilt(:,m))

      END DO
      dumsthf_soilt(:,:,:) = 0.0
    ELSE
      zw_inund_soilt(:,:)  = zw_soilt(:,:)
      dumsthf_soilt(:,:,:) = sthf_soilt(:,:,:)
    END IF

!   We need top_crit_soilt - get this from calc_baseflow.
    DO m=1,nsoilt
      CALL calc_baseflow_jules(                                               &
        soil_pts, soil_index, land_pts, sm_levels,                            &
        zdepth, ksz_soilt(:,m,:), bexp_soilt(:,m,:), fexp_soilt(:,m),         &
        ti_mean_soilt(:,m), zw_inund_soilt(:,m), dumsthf_soilt(:,m,:),        &
        top_crit_soilt(:,m), qbase_soilt(:,m), qbase_l_soilt(:,m,:))

!   Call calc_fsat with 1st argument (l_gamtot)=.FALSE. so as to get fsat_soilt.
      CALL calc_fsat(                                                         &
        l_gamtot, soil_pts, soil_index, land_pts, ti_mean_soilt(:,m),         &
        ti_sig_soilt(:,m), dumwutot_soilt(:,m), top_crit_soilt(:,m),          &
        gamtot_soilt(:,m), fsat_soilt(:,m), fwetl_soilt(:,m))
    END DO

    IF (L_wetland_unfrozen)THEN
      DO j = 1,soil_pts
        i = soil_index(j)
        fsat_soilt(i,:)  = wutot_soilt(i,:) * fsat_soilt(i,:)
        fwetl_soilt(i,:) = wutot_soilt(i,:) * fwetl_soilt(i,:)
      END DO
    END IF

  END IF  !  soil_pts

  RETURN

END SUBROUTINE topmodel_init
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

FUNCTION get_default_ic_values(total_snow) RESULT(defaults_dict)

! USE statments
  USE dictionary_mod, ONLY : dict, dict_set, dict_free, dict_create
  USE logging_mod,    ONLY : log_info, log_debug, log_warn, log_error, log_fatal

  USE dump_mod,       ONLY : max_var_dump, required_vars_for_configuration, read_dump

  USE model_interface_mod, ONLY : identifier_len, populate_var, get_var_id

  USE fire_mod,        ONLY : fire_inis

  USE metstats_mod,    ONLY : metstats_inis

  IMPLICIT NONE

  !-----------------------------------------------------------------------------
  ! Description:
  !   Gets default values for initialisation
  !
  ! Code Owner: Please refer to ModuleLeaders.txt
  ! This file belongs in TECHNICAL
  !
  ! Code Description:
  !   Language: Fortran 90.
  !   This code is written to JULES coding standards v1.
  !-----------------------------------------------------------------------------
! Arguments
    LOGICAL, INTENT(IN) :: total_snow

    CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                                 ! The variable identifiers of the required
                                 ! variables

    CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                                 ! The variable identifiers of the ancil
                                 ! variables (not used in this subroutine)

! Work variables
    INTEGER :: i !Counter
    INTEGER :: nvars_required
    INTEGER :: nvars_from_ancil

! Return type
    TYPE(dict) :: defaults_dict

  LOGICAL, PARAMETER :: l_output_mode = .FALSE.

!End of header
!-----------------------------------------------------------------------------

  !Get the list of variables we need to have default variables for
  CALL required_vars_for_configuration(nvars_required, identifiers,           &
                                       nvars_from_ancil, vars_from_ancil,     &
                                       l_output_mode,                         &
                                       total_snow, .FALSE.)

  !Create the dictionary containing the default values
  defaults_dict = dict_create(nvars_required, 1.0)

  !Set the default values one by one
  DO i=1, nvars_required
    SELECT CASE ( identifiers(i) )

!      CASE ( 'canopy' )
!      CASE ( 'cs' )
!      CASE ( 'gs' )
!      CASE ( 'snow_tile' )
!      CASE ( 'sthuf' )
!      CASE ( 't_soil' )
!      CASE ( 'tstar_tile' )
!      CASE ( 'lai' )
!      CASE ( 'canht' )
!      CASE ( 'frac' )
!      CASE ( 'sthzw' )
!      CASE ( 'zw' )
!      CASE ( 'rgrain' )

      !Metstat module variables
      CASE ( 'temp_max_00h_r' )
        CALL dict_set(defaults_dict, 'temp_max_00h_r', metstats_inis%temp_max_00h)
      CASE ( 'temp_ave_00h_r' )
        CALL dict_set(defaults_dict, 'temp_ave_00h_r', metstats_inis%temp_ave_00h)
      CASE ( 'prec_tot_00h_r' )
        CALL dict_set(defaults_dict, 'prec_tot_00h_r', metstats_inis%prec_tot_00h)
      CASE ( 'prec_tot_12h_r' )
        CALL dict_set(defaults_dict, 'prec_tot_12h_r', metstats_inis%prec_tot_12h)
      CASE ( 'rhum_min_00h_r' )
        CALL dict_set(defaults_dict, 'rhum_min_00h_r', metstats_inis%rhum_min_00h)
      CASE ( 'dewp_ave_00h_r' )
        CALL dict_set(defaults_dict, 'dewp_ave_00h_r', metstats_inis%dewp_ave_00h)
      CASE ( 'wind_ave_00h_r' )
        CALL dict_set(defaults_dict, 'wind_ave_00h_r', metstats_inis%wind_ave_00h)
      CASE ( 'temp_max_00h' )
        CALL dict_set(defaults_dict, 'temp_max_00h',   metstats_inis%temp_max_00h)
      CASE ( 'temp_ave_00h' )
        CALL dict_set(defaults_dict, 'temp_ave_00h',   metstats_inis%temp_ave_00h)
      CASE ( 'temp_pnt_12h' )
        CALL dict_set(defaults_dict, 'temp_pnt_12h',   metstats_inis%temp_pnt_12h)
      CASE ( 'prec_tot_00h' )
        CALL dict_set(defaults_dict, 'prec_tot_00h',   metstats_inis%prec_tot_00h)
      CASE ( 'prec_tot_12h' )
        CALL dict_set(defaults_dict, 'prec_tot_12h',   metstats_inis%prec_tot_12h)
      CASE ( 'rhum_min_00h' )
        CALL dict_set(defaults_dict, 'rhum_min_00h',   metstats_inis%rhum_min_00h)
      CASE ( 'rhum_pnt_12h' )
        CALL dict_set(defaults_dict, 'rhum_pnt_12h',   metstats_inis%rhum_pnt_12h)
      CASE ( 'dewp_ave_00h' )
        CALL dict_set(defaults_dict, 'dewp_ave_00h',   metstats_inis%dewp_ave_00h)
      CASE ( 'wind_ave_00h' )
        CALL dict_set(defaults_dict, 'wind_ave_00h',   metstats_inis%wind_ave_00h)
      CASE ( 'wind_pnt_12h' )
        CALL dict_set(defaults_dict, 'wind_pnt_12h',   metstats_inis%wind_pnt_12h)

      !Fire module variables
      CASE ( 'fire_canadian_ffmc' )
        CALL dict_set(defaults_dict, 'fire_canadian_ffmc'     , fire_inis%canadian_ffmc )
      CASE ( 'fire_canadian_ffmc_mois' )
        CALL dict_set(defaults_dict, 'fire_canadian_ffmc_mois', fire_inis%canadian_ffmc_mois)
      CASE ( 'fire_canadian_dmc' )
        CALL dict_set(defaults_dict, 'fire_canadian_dmc'      , fire_inis%canadian_dmc )
      CASE ( 'fire_canadian_dc' )
        CALL dict_set(defaults_dict, 'fire_canadian_dc'       , fire_inis%canadian_dc )
      CASE ( 'fire_mcarthur_r_dr' )
        CALL dict_set(defaults_dict, 'fire_mcarthur_r_dr'     , fire_inis%mcarthur_r_dr )
      CASE ( 'fire_mcarthur_n_dr' )
        CALL dict_set(defaults_dict, 'fire_mcarthur_n_dr'     , fire_inis%mcarthur_n_dr)
      CASE ( 'fire_nesterov' )
        CALL dict_set(defaults_dict, 'fire_nesterov'          , fire_inis%nesterov_index )

      !Snow variables
!      CASE ( 'rho_snow' )
!      CASE ( 'snow_depth' )
!      CASE ( 'snow_grnd' )
!      CASE ( 'nsnow' )
!      CASE ( 'snow_ds' )
!      CASE ( 'snow_ice' )
!      CASE ( 'snow_liq' )
!      CASE ( 'tsnow' )

      CASE DEFAULT
        CALL log_info("get_default_ic_values", &
                       "No default value defined. Value must be defined in namelist - "&
                       // TRIM(identifiers(i)))
    END SELECT
  END DO

  RETURN

END FUNCTION get_default_ic_values
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   Subroutine CALC_FIT_FSAT-------------------------------------------
!
!   Purpose: To speed up the large scale hydrology code (LTOP=TRUE)
!            dramatically. This is done by fitting exponential
!            functions to the incomplete gamma function for each grid
!            box and the complete range of possible "water table"
!            (top_crit) cases - see documentation.
!            Estimates the fitted parameters for Fsat=function(ZW)
!            and  Fwet=function(ZW) for each land grid point.
!            (Calculating the incomplete gamma function for each grid
!            box at each time step was very time consuming).
!                                                             !
! Documentation: UNIFIED MODEL DOCUMENTATION PAPER NO 25
!
! History:
! Version   Date     Comment
! -------   ----     -------
!  6.4    10/01/07   New Deck         Nic Gedney
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.

SUBROUTINE calc_fit_fsat()

  !Use in relevant subroutines
  USE calc_fsat_mod, ONLY: calc_fsat

  !Use in relevant varaibles
  USE ancil_info,           ONLY:                                             &
    land_pts, soil_pts, soil_index, nsoilt

  USE top_pdm,              ONLY:                                             &
    a_fsat_soilt, a_fwet_soilt, c_fsat_soilt, c_fwet_soilt,  fexp_soilt,      &
    gamtot_soilt, ti_mean_soilt, ti_sig_soilt

  USE jules_hydrology_mod,  ONLY:                                             &
    l_top, zw_max, nfita

  USE jules_soil_mod,       ONLY:                                             &
    dzsoil, sm_levels

  USE jules_print_mgr,      ONLY:                                             &
    jules_message, jules_print

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Subroutine arguments - NONE.
!-----------------------------------------------------------------------------

! Work variables
  REAL :: zdepth           ! Standard Soil model DEPTH.

! Local scalars:
  INTEGER, PARAMETER :: nzw = 100  ! Number of ZW values used in fit.
                                   ! Maximum value for a significant
                                   ! improvement in the fit.

  INTEGER :: i,j,iz,N,ifita,m  ! Loop counters.

  REAL ::                                                                     &
    dzw,                                                                      &
                   ! WORK ZW increment ; defined by zw_max and nzw.
    rms,                                                                      &
                   ! WORK rms errors for given fsat fit values.
    rmsw,                                                                     &
                   ! WORK rms errors for given fwet fit values.
    rmsold,                                                                   &
                   ! WORK rms errors for given fsat fit values.
                   !      for best fit so far.
    rmswold,                                                                  &
                   ! WORK rms errors for given fwet fit values
                   !      for best fit so far.
    cfit,                                                                     &
                   ! WORK CFit value for given loop.
    cfitmax
                   ! Maximum possible value for Cfit.

  REAL, PARAMETER ::                                                          &
    thr_err = 5.0e-3,                                                         &
                   ! Error threshold value

    CFITMIN = 0.0
                   ! Minimum possible value for Cfit.

! Local arrays:
  REAL ::                                                                     &
    fsat_calc_soilt(land_pts,nsoilt,nzw),                                     &
                              ! WORK Surface saturation fraction.
    fsat_fit(nzw),                                                            &
                              ! WORK Fitted surface saturation fraction.
    fwet_calc_soilt(land_pts,nsoilt,nzw),                                     &
                              ! WORK Wetland fraction.
    fwet_fit(nzw),                                                            &
                              ! WORK Fitted wetland fraction.
    dumzw(nzw),                                                               &
                              ! WORK Dummy water table depth (m).
    dumfsat_soilt(land_pts,nsoilt),                                           &
                              ! WORK Dummy surface saturation fraction.
    dumfwetl_soilt(land_pts,nsoilt)
                              ! WORK Dummy wetland fraction.
! DBC We could use this local version of gamtot in this version,
!     but using the module version to match other versions of code.
!     &,gamtot_soilt(land_pts)       ! WORK Integrated complete Gamma function

  REAL ::                                                                     &
    top_crit_soilt(land_pts,nsoilt,nzw),                                      &
                              ! WORK LOG(QBASE_MAX/QBASE) -see document.
    top_crit1z_soilt(land_pts,nsoilt),                                        &
                              ! WORK As above but for an individual zw.
    top_min_soilt(land_pts,nsoilt),                                           &
                              ! WORK value for when zw=zw_max.
    wutot_soilt(land_pts,nsoilt)           ! WORK Dummy (set to 1.0).

  INTEGER :: ERRORSTATUS
  CHARACTER (LEN=80) :: CMESSAGE
  CHARACTER (LEN=13) :: ROUTINENAME


!-----------------------------------------------------------------------------
  ROUTINENAME='CALC_FIT_FSAT'

  IF ( l_top ) THEN

    cfitmax=0.15*nfita

! Define the water table depths to be used in the fitting process:
    dzw = 1.0 / REAL(nzw) * zw_max
    DO iz = 1,nzw
      dumzw(iz) = REAL(iz - 1) * dzw
    END DO

    DO i = 1,land_pts        ! initialise to zero
      wutot_soilt(i,:)    = 1.0
      dumfsat_soilt(i,:)  = 0.0
      dumfwetl_soilt(i,:) = 0.0
    END DO
    zdepth = 0.0

! Calculate total soil depth
    DO n = 1,sm_levels
      zdepth = zdepth + dzsoil(n)
    END DO

! Calculate Gamtot
    DO m=1,nsoilt
      gamtot_soilt(:,m) = 0.0
      CALL calc_fsat(.TRUE., soil_pts, soil_index, land_pts,                  &
                    ti_mean_soilt(:,m),                                       &
                    ti_sig_soilt(:,m), wutot_soilt(:,m),                      &
                    top_crit1z_soilt(:,m), gamtot_soilt(:,m),                 &
                    dumfsat_soilt(:,m), dumfwetl_soilt(:,m))
    END DO

! Calculate top_crit for the water table depths:
    DO iz = 1,nzw
      DO m=1, nsoilt
        DO j = 1,soil_pts
          i = soil_index(j)

          fsat_calc_soilt(i,m,iz) = 0.0
          fwet_calc_soilt(i,m,iz) = 0.0
          top_crit_soilt(i,m,iz)  = 0.0

          IF ( ti_mean_soilt(i,m) > 0.0 .AND. ti_sig_soilt(i,m) > 0.0 ) THEN
            top_min_soilt(i,m) = 1.0 / fexp_soilt(i,m)                        &
                                 * EXP(-fexp_soilt(i,m) * (zw_max - zdepth))

            IF ( dumzw(iz) <= zdepth ) THEN
              top_crit1z_soilt(i,m) = LOG(zdepth + 1.0                        &
                                        / fexp_soilt(i,m)-top_min_soilt(i,m)) &
                                    - LOG(zdepth - dumzw(iz) + 1.0            &
                                        / fexp_soilt(i,m)-top_min_soilt(i,m))
            END IF

            IF ( dumzw(iz) >  zdepth ) THEN
              top_crit1z_soilt(i,m) = LOG(zdepth + 1.0                        &
                                      / fexp_soilt(i,m)-top_min_soilt(i,m))   &
                                      - LOG(1 / fexp_soilt(i,m)               &
                                      * EXP(-fexp_soilt(i,m)                  &
                                      * (dumzw(iz) - zdepth))                 &
                                      - top_min_soilt(i,m))
            END IF
          END IF
        END DO !soil_pts

! Calculate FSAT and FWET for one ZW at all soil land_pts:
        CALL calc_fsat(.FALSE., soil_pts, soil_index, land_pts,               &
                       ti_mean_soilt(:,m),                                    &
                       ti_sig_soilt(:,m), wutot_soilt(:,m),                   &
                       top_crit1z_soilt(:,m), gamtot_soilt(:,m),              &
                       dumfsat_soilt(:,m), dumfwetl_soilt(:,m))

        DO j = 1,soil_pts
          i = soil_index(j)

          IF ( ti_mean_soilt(i,m) > 0.0 .AND. ti_sig_soilt(i,m) > 0.0 ) THEN
            fsat_calc_soilt(i,m,iz) = dumfsat_soilt(i,m)
            fwet_calc_soilt(i,m,iz) = dumfwetl_soilt(i,m)
            top_crit_soilt(i,m,iz)  = top_crit1z_soilt(i,m)

            IF ( dumzw(iz) <  dzw ) THEN ! Values at zw=0m
              a_fsat_soilt(i,m) = fsat_calc_soilt(i,m,iz)
              a_fwet_soilt(i,m) = fwet_calc_soilt(i,m,iz)
            END IF
          END IF
        END DO
      END DO !nsoilt
    END DO                  !ZW calc_fsat loop

! Now carry out fit for FSAT, where FSAT=function(ZW). (Likewise FWET)
    DO m=1, nsoilt
      DO j = 1,soil_pts
        i = soil_index(j)

        IF ( ti_mean_soilt(i,m) > 0.0 .AND. ti_sig_soilt(i,m) > 0.0 ) THEN
          rmsold  = 1.0e10
          rmswold = 1.0e10

          DO ifita = 1,nfita
            cfit = cfitmax * (ifita) / REAL(nfita)

            rms  = 0.0
            rmsw = 0.0
  ! top_crit=TI_MAX when zw=zw_max
            DO iz = 1,nzw
              fsat_fit(iz) = a_fsat_soilt(i,m)                                &
                              * EXP(-cfit * top_crit_soilt(i,m,iz))
              fwet_fit(iz) = a_fwet_soilt(i,m)                                &
                              * EXP(-cfit * top_crit_soilt(i,m,iz))
              rms  = rms  + (fsat_calc_soilt(i,m,iz) - fsat_fit(iz))**2
              rmsw = rmsw + (fwet_calc_soilt(i,m,iz) - fwet_fit(iz))**2
            END DO         !ZW

            rms  = SQRT(rms) / REAL(nzw)
            rmsw = SQRT(rmsw) / REAL(nzw)

            IF ( rms < rmsold ) THEN
              rmsold = rms
              c_fsat_soilt(i,m) = cfit
            END IF
            IF ( rmsw < rmswold ) THEN
              rmswold = rmsw
              c_fwet_soilt(i,m) = cfit
            END IF
          END DO

          DO iz = 1,nzw
            fsat_fit(iz) = a_fsat_soilt(i,m) * EXP(-c_fsat_soilt(i,m) *       &
                           top_crit_soilt(i,m,iz))
            fwet_fit(iz) = a_fwet_soilt(i,m) * EXP(-c_fwet_soilt(i,m) *       &
                           top_crit_soilt(i,m,iz))
          END DO            !ZW

          IF ( rmsold >= thr_err ) THEN
            IF ( c_fsat_soilt(i,m) <= cfitmin .OR.                            &
                c_fsat_soilt(i,m) >= cfitmax ) THEN

              WRITE(jules_message,*) 'ERROR cfit FSAT', i,                    &
                  c_fsat_soilt(i,m), cfitmin, cfitmax
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*)                                          &
                'If c_fsat=cfitmax try increasing nfita'
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'fsat_calc=',                            &
                  fsat_calc_soilt(i,m,1), fsat_calc_soilt(i,m,3),             &
                  fsat_calc_soilt(i,m,5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'fsat_fit=',                             &
                  fsat_fit(1), fsat_fit(3), fsat_fit(5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'rms=', rmsold
              CALL jules_print('calc_fit_fsat',jules_message)
              ErrorStatus = 35
              WRITE(CMessage, *) 'Error in cfit FSAT in LSH model setup'
            END IF
          END IF

          IF ( rmswold >= thr_err ) THEN
            IF ( c_fwet_soilt(i,m) <= cfitmin .OR.                            &
                c_fwet_soilt(i,m) >= cfitmax ) THEN

              WRITE(jules_message,*) 'ERROR cfit FWET', i,                    &
                  c_fwet_soilt(i,m), cfitmin, cfitmax
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'fwet_calc=',                            &
                  fwet_calc_soilt(i,m,1), fwet_calc_soilt(i,m,3),             &
                  fwet_calc_soilt(i,m,5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'fwet_fit=',                             &
                  fwet_fit(1), fwet_fit(3), fwet_fit(5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) 'rmsw=', rmswold
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) '(fsat_calc=)',                          &
                  fsat_calc_soilt(i,m,1), fsat_calc_soilt(i,m,3),             &
                  fsat_calc_soilt(i,m,5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) '(fsat_fit=)',                           &
                  fsat_fit(1), fsat_fit(3), fsat_fit(5)
              CALL jules_print('calc_fit_fsat',jules_message)
              WRITE(jules_message,*) '(rms=)', rmsold
              CALL jules_print('calc_fit_fsat',jules_message)
              ErrorStatus = 35
              ErrorStatus = 40
              WRITE(CMessage,*) 'Error in cfit FWET in LSH model setup'
            END IF
          END IF

          IF ( rmsold > thr_err ) THEN
            WRITE(jules_message,*) 'Warning LSH rms Error in fit:',           &
                rmsold, rmswold
            CALL jules_print('calc_fit_fsat',jules_message)
          END IF
        END IF

      END DO                  ! land_pts
    END DO !nsoilt
  ELSE
    DO m=1,nsoilt
      DO j = 1,soil_pts
        i = soil_index(j)

        a_fsat_soilt(i,m) = 0.0
        c_fsat_soilt(i,m) = 0.0
        a_fwet_soilt(i,m) = 0.0
        c_fwet_soilt(i,m) = 0.0
        gamtot_soilt(i,m) = 0.0
      END DO
    END DO
  END IF

  RETURN

END SUBROUTINE calc_fit_fsat

END MODULE initial_conditions_mod

