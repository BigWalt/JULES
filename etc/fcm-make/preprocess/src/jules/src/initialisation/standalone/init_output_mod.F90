










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_output_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about what output has been requested and sets up
!   the output profiles.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE ! private scope by default
PUBLIC init_output

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE init_output(nml_dir)

USE io_constants, ONLY: max_sdf_name_len, max_var_file, namelist_unit

USE datetime_mod, ONLY: datetime_str_len, datetime, datetime_from_string

USE string_utils_mod, ONLY: to_string

USE model_time_mod, ONLY: timestep_len, main_run_start, main_run_end

USE model_interface_mod, ONLY: identifier_len

USE output_mod, ONLY: dump_period, output_dir, run_id,                     &
                      register_output_profile

USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads information about what output has been requested and sets up
!   the output profiles.
!-----------------------------------------------------------------------------

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
LOGICAL :: dir_exists  ! Used to check existence of output directory

TYPE(datetime) :: output_start_dt, output_end_dt
                       ! Datetime objects created from strings given in
                       ! output profiles

INTEGER :: i  ! Index variable.

INTEGER :: error  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Definition of the jules_output namelist
!-----------------------------------------------------------------------------
INTEGER :: nprofiles  ! The number of output profiles

NAMELIST  / jules_output/ output_dir, run_id, nprofiles, dump_period

!-----------------------------------------------------------------------------
! Definition of the JULES_OUTPUT_PROFILE namelist
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_sdf_name_len) :: profile_name  ! The name of the profile
LOGICAL :: output_initial ! T - this profile should output initial data
                          !     for each section it is outputting
                          ! F - this profile should not output initial data
LOGICAL :: output_spinup  ! T - generate output during spinup
                          ! F - don't generate output during spinup
LOGICAL :: output_main_run  ! T - generate output for the part of
                            !     the main run specified by
                            !     output_start and output_end
                            ! F - don't generate output for any
                            !     of the main run
CHARACTER(LEN=datetime_str_len) :: output_start, output_end
                                      ! The start and end times for output
                                      ! during the main run as strings
                                      ! If not given, output_start is assumed
                                      ! to be the start of the main run and
                                      ! output_end is assumed to be the end
                                      ! of the main run
INTEGER :: output_period
                                      ! The period of output. The default is
                                      ! to output every timestep.
                                      ! This can be a special period
INTEGER :: sample_period
                                      ! The period for sampling data (s). The
                                      ! default is to sample every timestep.
                                      ! Must be a multiple of the model
                                      ! timestep length and a factor of the
                                      ! output period.
INTEGER :: file_period                ! The period for new files - unless this
                                      ! is period_year or period_month, it is
                                      ! ignored

INTEGER :: nvars  ! The number of variables in the output profile
CHARACTER(LEN=identifier_len) :: var(max_var_file)
                      ! The model identifiers for variables in the output
                      ! profile
CHARACTER(LEN=identifier_len) :: var_name(max_var_file)
                      ! The name to use for variables in output files
CHARACTER(LEN=1) :: output_type(max_var_file)
                      ! The type of output to use - this should be one of
                      ! 'S', 'A', 'M', 'N' or 'X'

NAMELIST  / jules_output_profile/ profile_name, output_initial, output_spinup,&
                                output_main_run, output_start, output_end,  &
                                output_period, sample_period, file_period,  &
                                nvars, var, var_name, output_type
!end of header
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
nprofiles = 0

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'output.nml'),            &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = error,&
               IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_output",                                             &
                 "Error opening namelist file output.nml " //               &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Read the JULES_OUTPUT namelist
!-----------------------------------------------------------------------------
CALL log_info("init_output", "Reading JULES_OUTPUT namelist...")
READ(namelist_unit, NML = jules_output, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal("init_output",                                             &
                 "Error reading namelist JULES_OUTPUT " //                  &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Process the information from the JULES_OUTPUT namelist
!-----------------------------------------------------------------------------
! First check that a run_id was given
IF ( LEN_TRIM(run_id) == 0 ) CALL log_fatal("init_output", "No run_id given")

! Check that the output directory exists
! The Intel compiler requires a different form of the statement for directories,
! which we swap in with an ifdef
INQUIRE(FILE = output_dir, EXIST = dir_exists)
IF ( .NOT. dir_exists ) THEN
  CALL log_fatal("init_output",                                             &
                 "Output directory does not exist - " // TRIM(output_dir))
END IF

! Ensure dump_period is reasonable.
dump_period = MAX( dump_period, 1 )

! Warn if no output is requested
! We do this after the directory check as we will always produce dumps
IF ( nprofiles < 1 ) THEN
  CALL log_warn("init_output",                                              &
                "No output profiles given - output will not be " //         &
                "generated for this run")
  RETURN
END IF

!-----------------------------------------------------------------------------
! Read and process information about each output profile in turn
!-----------------------------------------------------------------------------
DO i = 1,nprofiles
  ! Set namelist values to their defaults before reading the next profile
  profile_name    = ""
  output_initial  = .FALSE.
  output_spinup   = .FALSE.
  output_main_run = .FALSE.
  output_start    = ""
  output_end      = ""
  output_period   = timestep_len
  sample_period   = timestep_len
  file_period     = 0
  nvars           = 0
  var(:)          = ''
  var_name(:)     = ''
  output_type(:)  = 'S'

  ! Read the namelist
  CALL log_info("init_output", "Reading JULES_OUTPUT_PROFILE namelist...")
  READ(namelist_unit, NML = jules_output_profile, IOSTAT = error,               &
       IOMSG = iomessage)
  IF ( error /= 0 ) THEN
    CALL log_fatal("init_output",                                           &
                   "Error reading namelist JULES_OUTPUT_PROFILE " //        &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //     &
                   TRIM(iomessage) // ")")
  END IF

  !--------------------------------------------------------------------------
  ! Check that the variables requested are suitable for the current
  ! configuration.
  !---------------------------------------------------------------------------
  CALL check_output_vars( nvars, var, var_name, output_type )

  !---------------------------------------------------------------------------
  ! If this profile is not providing any variables, we can skip it
  !---------------------------------------------------------------------------
  IF ( nvars < 1 ) THEN
    CALL log_error("init_output",                                           &
                   "Profile " // TRIM(profile_name) // " is not " //        &
                   "outputting any variables - ignoring")
    CYCLE
  END IF

  !---------------------------------------------------------------------------
  ! Set up the datetime objects for output start and end
  ! If not given, they default to start and end of main run respectively
  !---------------------------------------------------------------------------
  IF ( LEN_TRIM(output_start) == 0 ) THEN
    output_start_dt = main_run_start
  ELSE
    output_start_dt = datetime_from_string(output_start)
  END IF
  IF ( LEN_TRIM(output_end) == 0 ) THEN
    output_end_dt = main_run_end
  ELSE
    output_end_dt = datetime_from_string(output_end)
  END IF

  !---------------------------------------------------------------------------
  ! Register the output profile - this performs more error checking
  !---------------------------------------------------------------------------
  CALL register_output_profile(profile_name, output_initial,                &
                               output_spinup, output_main_run,              &
                               output_start_dt, output_end_dt,              &
                               output_period, sample_period, file_period,   &
                               var(1:nvars), var_name(1:nvars),             &
                               output_type(1:nvars))

END DO

CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal("init_output",                                             &
                 "Error closing namelist file output.nml " //               &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")
END IF

RETURN

END SUBROUTINE init_output

!-----------------------------------------------------------------------------

SUBROUTINE check_output_vars( nvars, var, var_name, output_type )

USE jules_snow_mod, ONLY: nsmax

USE jules_soil_biogeochem_mod, ONLY: soil_model_ecosse, soil_model_rothc,  &
      soil_bgc_model

USE jules_soil_ecosse_mod, ONLY: l_soil_N

USE jules_vegetation_mod, ONLY: l_fapar_diag, l_fao_ref_evapotranspiration

USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

USE sf_diags_mod, ONLY: strnewsfdiag, sf_diag

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks that all variables selected for output are available with the
!   current configuration, and removes any that are not. Also sets flags
!   if certain diagnostics are selected.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
INTEGER, INTENT(INOUT) ::                                                     &
  nvars
    ! The number of variables in the output profile.

CHARACTER(LEN=*), INTENT(INOUT) ::                                            &
  var(:),                                                                     &
    ! The model identifiers for variables in the output profile.
  var_name(:),                                                                &
    ! The name to use for variables in output files.
  output_type(:)
    ! The type of output for each variable.

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  j,                                                                          &
    ! Index variable.
  nvars_in
    ! The number of variables that were requested for the profile currently
    ! being processed, before filtering.

LOGICAL ::                                                                    &
  remove_var
    ! Indicates if the current variable should be removed from the list as it
    ! is not allowed.

CHARACTER(LEN=200) :: message  !  Diagnostic message.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_OUTPUT_VARS'

!end of header
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Remove any variables that are not allowed for the current configuration,
! warning about their removal.
!-----------------------------------------------------------------------------
nvars_in = nvars
nvars    = 0

DO j = 1,nvars_in
  ! Assume that we are going to keep this variable until we decide otherwise.
  remove_var = .FALSE.
  message    = ''

  !---------------------------------------------------------------------------
  ! Check if this variable is allowed for the current configuration.
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Multilayer snow variables, and those derived from them, need nsmax>=1.
  !---------------------------------------------------------------------------
  IF ( nsmax < 1 ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'snow_ice_gb', 'snow_ice_tile',                                    &
           'snow_liq_gb', 'snow_liq_tile',                                    &
           'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
      remove_var = .TRUE.
      message    = 'Multi-layer snow model not used.'

    CASE DEFAULT
      remove_var = .FALSE.

    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with a multi-pool soil C model.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_rothc .AND.                               &
       soil_bgc_model /= soil_model_ecosse ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'c_bio',    'c_dpm',    'c_hum',    'c_rpm',                       &
           'c_bio_gb', 'c_dpm_gb', 'c_hum_gb', 'c_rpm_gb' )
      remove_var = .TRUE.
      message    = 'A suitable multi-pool soil C model is not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with a multi-pool soil N model.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_rothc .AND.                               &
       .NOT. ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'n_bio',      'n_dpm',    'n_hum',    'n_rpm',                     &
           'n_bio_gb',   'n_dpm_gb', 'n_hum_gb', 'n_rpm_gb',                  &
           'n_inorg_gb', 'n_soil_gb' )
      remove_var = .TRUE.
      message    = 'A suitable multi-pool soil N model is not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  !  ECOSSE variables - only allowed if ECOSSE selected.
  !  In this section we test variables that are always used with ECOSSE.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_ecosse ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'plant_input_c_gb', 'soil_ph' )
      remove_var = .TRUE.
      message    = 'ECOSSE not used.'
    END SELECT
  END IF  !  not ECOSSE

  !---------------------------------------------------------------------------
  !  ECOSSE N variables - only allowed if ECOSSE + soil N selected.
  !---------------------------------------------------------------------------
  IF ( .NOT. ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'n_amm', 'n_nit', 'plant_input_n_gb' )
      remove_var = .TRUE.
      message    = 'ECOSSE with Nitrogen is not being used.'
    END SELECT
  END IF  !  not ECOSSE with N

  IF ( remove_var ) THEN
    ! If the variable needs to be removed, issue a warning that we are doing
    ! this.
    CALL log_error(RoutineName,                                               &
                   "Variable " // TRIM(var(j)) // " is not available for " // &
                   "output with the current configuration: "               // &
                   TRIM(message) // ". Removing from output.")
  ELSE
    ! Otherwise add the variable to the collapsed list - we can do this since
    ! nvars <= j so we are not overwriting unprocessed data
    nvars              = nvars + 1
    var(nvars)         = var(j)
    var_name(nvars)    = var_name(j)
    output_type(nvars) = output_type(j)
  END IF

END DO  !  variables

!-----------------------------------------------------------------------------
! Check for specific variables that require a flag to be set.
!-----------------------------------------------------------------------------
DO j = 1,nvars

  SELECT CASE ( var(j) )

    !---------------------------------------------------------------------------
    ! First we consider diagnostics found in sf_diags.
    !---------------------------------------------------------------------------
  CASE ( 'et_stom' )
    sf_diag % l_et_stom_surft = .TRUE.

  CASE ( 'et_stom_gb' )
    sf_diag % l_et_stom = .TRUE.

  CASE ( 'fprf' )
    sf_diag % l_fprf = .TRUE.

  CASE ( 'fsth' )
    sf_diag % l_fsth = .TRUE.

  CASE ( 'ftemp' )
    sf_diag % l_ftemp = .TRUE.

  CASE ( 'latent_heat' )
    sf_diag % slh = .TRUE.

  CASE ( 'lw_down_surft', 'lw_up_surft' )
    sf_diag % l_lw_surft = .TRUE.

  CASE ( 'mu10m_n' )
    sf_diag % l_mu10m_n = .TRUE.

  CASE ( 'mv10m_n' )
    sf_diag % l_mv10m_n = .TRUE.

  CASE ( 'q1p5m', 'q1p5m_gb' )
    sf_diag % sq1p5 = .TRUE.

  CASE ( 'snice_smb_surft', 'snice_m_surft', 'snice_freez_surft',             &
         'snice_sicerate_surft', 'snice_sliqrate_surft',                      &
         'snice_runoff_surft' )
    sf_diag % l_snice = .TRUE.

  CASE ( 'snomlt_surf_htf' )
    sf_diag % smlt = .TRUE.

  CASE ( 't1p5m', 't1p5m_gb' )
    sf_diag % st1p5 = .TRUE.

  CASE ( 'u10m' )
    sf_diag % su10 = .TRUE.

  CASE ( 'u10m_n' )
    sf_diag % l_u10m_n = .TRUE.

  CASE ( 'v10m' )
    sf_diag % sv10 = .TRUE.

  CASE ( 'v10m_n' )
    sf_diag % l_v10m_n = .TRUE.

    !---------------------------------------------------------------------------
    ! Below here for diagnostics that are not in sf_diags.
    !---------------------------------------------------------------------------
  CASE ( 'fapar' )
    l_fapar_diag = .TRUE.

  CASE ( 'fao_et0' )
    l_fao_ref_evapotranspiration = .TRUE.
  
  END SELECT

END DO  !  variables

END SUBROUTINE check_output_vars

END MODULE init_output_mod
