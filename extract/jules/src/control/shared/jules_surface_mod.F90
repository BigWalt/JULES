
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_surface_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains surface options and a namelist for setting them
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE water_constants_mod, ONLY: lc, lf
USE planet_constants_mod, ONLY: cp, one_minus_epsilon, grcp, g, repsilon

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Parameters defining valid values for switches
!-----------------------------------------------------------------------------
! Options for form drag
INTEGER, PARAMETER ::                                                       &
  no_drag         = 0,                                                      &
  effective_z0    = 1,                                                      &
  explicit_stress = 2

! Options for ISrfExCnvGust
INTEGER, PARAMETER :: IP_SrfExWithCnv = 1

! Options for cor_mo_iter
INTEGER, PARAMETER ::                                                       &
  Use_Correct_Ustar = 2,                                                    &
      ! Option under the COR_MO_ITER switch for the dust scheme to use the
      ! correct ustar
  Limit_ObukhovL = 3
      ! Option under the COR_MO_ITER switch for imposing lower limit on L
      ! in very stable conditions

! Options for iscrntdiag
INTEGER, PARAMETER ::                                                       &
  ip_scrndecpl1 = 1,                                                        &
      ! Diagnose the screen temperature using surface similarity theory, but
      ! allow decoupling in very stable conditions based on the
      ! quasi-equilibrium radiative solution
  ip_scrndecpl2 = 2,                                                        &
      ! Diagnose the screen temperature using including transient effects
      ! and radiative cooling
  ip_scrndecpl3 = 3
     ! Diagnose the screen temperature and humidity using including transient 
     ! effects and radiative cooling


!-----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
LOGICAL ::                                                                  &
  l_flake_model = .FALSE.,                                                  &
      ! Switch for using the FLake lake model
  l_epot_corr = .FALSE.,                                                    &
      ! Switch for using a correction to the calculation of potential
      ! evaporation
  l_point_data = .FALSE.,                                                   &
      ! Switch for using point rainfall data
  l_aggregate = .FALSE.,                                                    &
      ! Switch for setting an aggregate surface scheme
  l_elev_land_ice = .FALSE.,                                                &
      ! Use a simple tiled bedrock on ice points to facilitate subgridscale
      ! altitude tiles for the ice
  l_land_ice_imp = .FALSE.,                                                 &
      ! Use implicit numerics to update land ice temperatures
  l_elev_lw_down = .FALSE.,                                                 &
      ! Use a simple tiled bedrock on ice points to facilitate subgridscale
      ! altitude tiles for the ice
  l_anthrop_heat_src = .FALSE.,                                             &
      ! Switch for anthropogenic heat source on urban tile
  l_vary_z0m_soil = .FALSE.,                                                &
      ! Switch for variable momentum bare soil roughness lengths
  l_urban2t       = .FALSE.
      ! Two urban tiles (canyon and roof) will be used instead of one

INTEGER ::                                                                  &
  i_modiscopt = 0,                                                          &
      ! Method of discretization in the surface layer
  all_tiles = 0,                                                            &
      ! Switch for doing calculations of tile properties on all tiles for
      ! all gridpoints even when the tile fraction is zero
      !(except for land ice)
  cor_mo_iter = 1,                                                          &
      ! Switch for MO iteration correction
  iscrntdiag = 0,                                                           &
      ! Method of diagnosing the screen temperature
  i_aggregate_opt = 0,                                                      &
      ! Method of aggregating tiled properties
      !   0 : Original option
      !   1 : Separate aggregation of z0h
  formdrag = no_drag,                                                       &
      ! Switch for orographic form drag
  fd_stab_dep = 0,                                                          &
      ! Switch to implement stability dependence of orographic form drag
  ISrfExCnvGust = 0
      ! Switch to include the effect of convective downdraughts on
      ! surface exchange
      ! OFF (=0)
      !   => Only boundary-layer gustiness is considered (original version)
      ! IP_SrfExWithCnv (=1)
      !   => The impact of gustiness due to boundary layer eddies is reduced
      !      relative to the above, but eddies driven by convective
      !      downdraughts are included

!-----------------------------------------------------------------------------
! Fixed parameters
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                       &
  l_neg_tstar = .FALSE.
      ! Test for negative surface temperature
      ! Despite always being .FALSE., this is retained as it can be useful
      ! for debugging if set to .TRUE. and recompiled

INTEGER, PARAMETER ::                                                       &
  iter = 3
      ! Number of iterations to determine the canopy climate

REAL, PARAMETER ::                                                          &
  ls = lc + lf,                                                             &
      ! Latent heat of sublimation (J per kg)
  ratio = 1.6,                                                              &
      ! Ratio of leaf resistance for CO2 to leaf resistance for H2O
  ratio_o3 = 1.67,                                                          &
      ! Ratio of leaf resistance for O3 to leaf resistance for H2O
  o2 = 0.23,                                                                &
      ! Atmospheric concentration of oxygen (kg O2/kg air)
  cmass = 0.40
      ! Fraction of leaf dry matter in the form of C
      ! Used to convert LMA (kgleaf/m2) to sigl (kgC/m2)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Constants for surface transfer coefficients
INTEGER, PARAMETER ::                                                       &
  n_its = 5
      ! Number of iterations for Monin-Obukhov length and stability functions

REAL, PARAMETER ::                                                          &
  beta = 0.08,                                                              &
      ! Tunable parameter in the surface layer scaling velocity formula
      ! (multiplying the turbulent convective scaling velocity)
  third = 1.0 / 3.0,                                                        &
      ! One third

  beta_cndd = 0.04,                                                         &
      ! Tunable parameter in the surface layer scaling velocity formula
      ! multiplying the turbulent convective scaling velocity (halved from
      ! the value in the original scheme to compensate for the convective
      ! contribution)
  cnst_cndd_0 = 1.0,                                                        &
      ! Constant in formula for convective gustiness
  cnst_cndd_1 = 6.004e02,                                                   &
      ! Constant in formula for convective gustiness
  cnst_cndd_2 = -4.375e03,                                                  &
      ! Constant in formula for convective gustiness
  min_wind = 1.0e-3,                                                        &
      ! Minimum windspeed for iteration convergence
  min_ustar = min_wind / 50.0,                                              &
      ! Corresponding friction velocity
  Ri_m = 10.0
      ! Maximum Richardson number

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Constants used in the Beljaars and Holtslag stable stability functions
REAL, PARAMETER ::                                                          &
  a = 1.0,                                                                  &
  b = 2.0 / 3.0,                                                            &
  c = 5.0,                                                                  &
  d = 0.35,                                                                 &
  c_over_d = c/d

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Stability parameters for calculation of aerodynamic resistance
REAL, PARAMETER ::                                                          &
  ah = 10.0,                                                                &
  cz = 4.0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Parameters used for calculation of saturated specific humidity
REAL, PARAMETER ::                                                          &
  t_low = 183.15,                                                           &
      ! Lowest temperature for which look-up table of saturation water
      ! vapour pressure is valid (k)
  t_high = 338.15,                                                          &
      ! Highest temperature for which look-up table of saturation water
      ! vapour pressure is valid (k)
  delta_t = 0.1
      ! Temperature increment of the look-up table of saturation
      ! vapour pressures

INTEGER, PARAMETER ::                                                       &
  n = ((t_high - t_low + (delta_t * 0.5)) / delta_t) + 1.0
      ! Size of look-up table of saturation water vapour pressures
      ! With t_low = 183.15, t_high = 338.15 and delta_t = 0.1, this gives
      ! n=1551

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Parameters used in interpolation of wind, T and q
REAL, PARAMETER ::                                                          &
  z_obs_tq = 1.5,                                                           &
      ! Height of screen observations of temperature and humidity
  z_obs_wind = 10.0
      ! Height of surface wind observations.

LOGICAL, PARAMETER ::                                                       &
  eff_int = .FALSE.
      ! Switch for orographic roughness in calculation of 10m wind speed
      !     .TRUE. orographic roughness included
      !     .FALSE. orographic roughness NOT included

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Parameters used to calculate blending height
REAL, PARAMETER ::                                                          &
  h_blend_max = 1000.0,                                                     &
      ! Maximum blending height (m)
  h_blend_min = 0.0
      ! Minimum blending height (m)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Other variables used in surface calculations
REAL, ALLOCATABLE ::                                                        &
  diff_frac(:)
      ! The fraction of radiation that is diffuse


!-----------------------------------------------------------------------------
! Parameters that can be set by the namelist
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  orog_drag_param = 0.3,                                                    &
      ! Drag coefficient for orographic form drag
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Parameters for heat capacity of vegetation.
    hleaf = 5.7e4,                                                            &
        ! Specific heat capacity of leaves (J / K / kg Carbon)
    hwood = 1.1e4,                                                            &
        ! Specific heat capacity of wood (J / K / kg Carbon)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Parameters for leaf conductance and photosynthesis.
    beta1 = 0.83,                                                             &
    beta2 = 0.93,                                                             &
        ! Coupling coefficients for co-limitation.
    fwe_c3 = 0.5,                                                             &
    fwe_c4 = 20000.0
        ! Factors in expressions for limitation of photosynthesis by transport
        ! of products, for C3 and C4 respectively


!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_surface/                                                    &
! Switches
    l_flake_model, l_epot_corr, l_point_data, l_aggregate, l_land_ice_imp,    &
    l_anthrop_heat_src, i_modiscopt, all_tiles, cor_mo_iter, iscrntdiag,      &
    i_aggregate_opt, formdrag, fd_stab_dep, ISrfExCnvGust, l_vary_z0m_soil,   &
    l_elev_lw_down, l_elev_land_ice, l_urban2t,                               &
! Parameters
    orog_drag_param, hleaf, hwood, beta1, beta2, fwe_c3, fwe_c4

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SURFACE_MOD'

CONTAINS


SUBROUTINE check_jules_surface()

USE ereport_mod, ONLY: ereport
USE jules_surface_types_mod, ONLY: elev_ice, elev_rock, urban, urban_canyon,  &
    urban_roof

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SURFACE namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errcode   ! error code to pass to ereport.

!-----------------------------------------------------------------------------


! Verify that the integer switches have suitable values
#if defined(UM_JULES)
IF ( i_modiscopt < 0 .OR. i_modiscopt > 1 ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "i_modiscopt should be 0 or 1")
END IF
#else
IF ( i_modiscopt /= 0 ) THEN
  errcode = -110
  CALL ereport("check_jules_surface", errcode,                            &
               "i_modiscopt should be 0 if forcing with data at a "//     &
               "specific level, rather than a vertical average. The "//   &
               "former is most likely in standalone JULES. Check that "// &
               "this setting was intended. If incorrectly set it may "//  &
               "cause failures.")
END IF
#endif

IF ( all_tiles < 0 .OR. all_tiles > 1 ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode, "all_tiles should be 0 or 1")
END IF

IF ( cor_mo_iter < 0 .OR. cor_mo_iter > Limit_ObukhovL ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "cor_mo_iter should be 0, 1, 2 or 3")
END IF

IF ( iscrntdiag < 0 .OR. iscrntdiag > ip_scrndecpl3 ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "iscrntdiag should be 0, 1, 2 or 3")
END IF

IF ( i_aggregate_opt < 0 .OR. i_aggregate_opt > 1 ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "i_aggregate_opt should be 0 or 1")
END IF

#if defined(UM_JULES)
IF ( formdrag < no_drag .OR. formdrag > explicit_stress ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "formdrag should be 0, 1, or 2")
END IF
#else
IF ( formdrag /= no_drag ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "Currently formdrag should be 0 (i.e. no drag) in standalone")
END IF
#endif

IF ( fd_stab_dep < 0 .OR. fd_stab_dep > 1 ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "fd_stab_dep should be 0 or 1")
END IF

IF ( ISrfExCnvGust < 0 .OR. ISrfExCnvGust > IP_SrfExWithCnv ) THEN
  errcode = 101
  CALL ereport("check_jules_surface", errcode,                            &
               "ISrfExCnvGust should be 0 or 1")
END IF

! Warn about cor_mo_iter changing under influence of l_flake_model
IF ( ( l_flake_model ) .AND. (cor_mo_iter /= Limit_ObukhovL) ) THEN
  cor_mo_iter = Limit_ObukhovL
  errcode = -100
  CALL ereport("check_jules_surface", errcode,                            &
               'cor_mo_iter set to Limit_ObukhovL since l_flake_model on')
END IF

! Check the Glacier/Icesheet model for consistency with the required surface
! types
IF ( l_elev_land_ice ) THEN
  ! Glacier/Icesheet model requires either elev_ice, elev_rock or both
  IF ( ALL( elev_ice <= 0 ) .AND. ALL( elev_rock <= 0 ) ) THEN
    errcode = 101
    CALL ereport("check_jules_surface", errcode,                         &
    "l_elev_land_ice = T. At least one of elev_ice or elev_rock"         &
    //" needs to be used (> 0).")
  END IF
ELSE
  IF ( ANY( elev_ice > 0 ) .OR. ANY( elev_rock > 0 ) ) THEN
    errcode = 101
    CALL ereport("check_jules_surface", errcode,                         &
    "l_elev_land_ice = F and at least one of elev_ice or elev_rock"      &
    //" is active (> 0).")
  END IF
END IF

! Check the two-tile urban schemes (the basic urban-2t or MORUSES) are
! consistent with the surface types
IF ( l_urban2t ) THEN

  ! The urban surface type cannot be used
  IF ( urban > 0 ) THEN
    errcode = 102
    CALL ereport("check_jules_surface", errcode,                              &
                 "The 'urban' surface type cannot be used with the " //       &
                 "two-tile urban schemes")
  END IF

  ! The urban_canyon and urban_roof have to be specified to use the two-tile
  ! urban schemes.
  IF ( ANY ( (/ urban_roof, urban_canyon /) < 0 ) ) THEN
    errcode = 103
    CALL ereport("check_jules_surface", errcode,                              &
                 "The two-tile urban schemes must have both the " //          &
                 "'urban_canyon' & 'urban_roof' surface types specified")
  END IF

END IF

END SUBROUTINE check_jules_surface


SUBROUTINE print_nlist_jules_surface()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer


!-----------------------------------------------------------------------------


CALL jules_print('jules_surface', 'Contents of namelist jules_surface')

WRITE(lineBuffer, *) '  l_flake_model = ', l_flake_model
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_epot_corr = ', l_epot_corr
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_point_data = ', l_point_data
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_aggregate = ', l_aggregate
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_elev_land_ice= ', l_elev_land_ice
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_land_ice_imp = ', l_land_ice_imp
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_elev_lw_down= ', l_elev_lw_down
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_anthrop_heat_src = ', l_anthrop_heat_src
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_vary_z0m_soil = ', l_vary_z0m_soil
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  l_urban2t = ', l_urban2t
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  i_modiscopt = ', i_modiscopt
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  all_tiles = ', all_tiles
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  cor_mo_iter = ', cor_mo_iter
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  iscrntdiag = ', iscrntdiag
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  i_aggregate_opt = ', i_aggregate_opt
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  formdrag = ', formdrag
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  fd_stab_dep = ', fd_stab_dep
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  ISrfExCnvGust = ', ISrfExCnvGust
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  hleaf = ', hleaf
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  hwood = ', hwood
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  beta1 = ', beta1
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  beta2 = ', beta2
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  fwe_c3 = ', fwe_c3
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  fwe_c4 = ', fwe_c4
CALL jules_print('jules_surface', lineBuffer)

END SUBROUTINE print_nlist_jules_surface

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_surface (unitnumber)

! Description:
!  Read the jules_surface namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_SURFACE'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 8
INTEGER, PARAMETER :: n_real = 7
INTEGER, PARAMETER :: n_log = 10

TYPE my_namelist
  SEQUENCE
  INTEGER :: i_modiscopt
  INTEGER :: all_tiles
  INTEGER :: cor_mo_iter
  INTEGER :: iscrntdiag
  INTEGER :: i_aggregate_opt
  INTEGER :: formdrag
  INTEGER :: fd_stab_dep
  INTEGER :: ISrfExCnvGust
  REAL :: orog_drag_param
  REAL :: hleaf
  REAL :: hwood
  REAL :: beta1
  REAL :: beta2
  REAL :: fwe_c3
  REAL :: fwe_c4
  LOGICAL :: l_flake_model
  LOGICAL :: l_epot_corr
  LOGICAL :: l_point_data
  LOGICAL :: l_aggregate
  LOGICAL :: l_elev_land_ice
  LOGICAL :: l_land_ice_imp
  LOGICAL :: l_elev_lw_down
  LOGICAL :: l_anthrop_heat_src
  LOGICAL :: l_vary_z0m_soil
  LOGICAL :: l_urban2t
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,            &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_surface, IOSTAT = errorstatus,           &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_surface", iomessage)

  my_nml % i_modiscopt        = i_modiscopt
  my_nml % all_tiles          = all_tiles
  my_nml % cor_mo_iter        = cor_mo_iter
  my_nml % iscrntdiag         = iscrntdiag
  my_nml % i_aggregate_opt    = i_aggregate_opt
  my_nml % formdrag           = formdrag
  my_nml % fd_stab_dep        = fd_stab_dep
  my_nml % ISrfExCnvGust      = ISrfExCnvGust
  my_nml % orog_drag_param    = orog_drag_param
  my_nml % hleaf              = hleaf
  my_nml % hwood              = hwood
  my_nml % beta1              = beta1
  my_nml % beta2              = beta2
  my_nml % fwe_c3             = fwe_c3
  my_nml % fwe_c4             = fwe_c4
  my_nml % l_flake_model      = l_flake_model
  my_nml % l_epot_corr        = l_epot_corr
  my_nml % l_point_data       = l_point_data
  my_nml % l_aggregate        = l_aggregate
  my_nml % l_elev_land_ice    = l_elev_land_ice
  my_nml % l_land_ice_imp     = l_land_ice_imp
  my_nml % l_elev_lw_down     = l_elev_lw_down
  my_nml % l_anthrop_heat_src = l_anthrop_heat_src
  my_nml % l_vary_z0m_soil    = l_vary_z0m_soil
  my_nml % l_urban2t          = l_urban2t

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  i_modiscopt        = my_nml % i_modiscopt
  all_tiles          = my_nml % all_tiles
  cor_mo_iter        = my_nml % cor_mo_iter
  iscrntdiag         = my_nml % iscrntdiag
  i_aggregate_opt    = my_nml % i_aggregate_opt
  formdrag           = my_nml % formdrag
  fd_stab_dep        = my_nml % fd_stab_dep
  ISrfExCnvGust      = my_nml % ISrfExCnvGust
  orog_drag_param    = my_nml % orog_drag_param
  hleaf              = my_nml % hleaf
  hwood              = my_nml % hwood
  beta1              = my_nml % beta1
  beta2              = my_nml % beta2
  fwe_c3             = my_nml % fwe_c3
  fwe_c4             = my_nml % fwe_c4
  l_flake_model      = my_nml % l_flake_model
  l_epot_corr        = my_nml % l_epot_corr
  l_point_data       = my_nml % l_point_data
  l_aggregate        = my_nml % l_aggregate
  l_elev_land_ice    = my_nml % l_elev_land_ice
  l_land_ice_imp     = my_nml % l_land_ice_imp
  l_elev_lw_down     = my_nml % l_elev_lw_down
  l_anthrop_heat_src = my_nml % l_anthrop_heat_src
  l_vary_z0m_soil    = my_nml % l_vary_z0m_soil
  l_urban2t          = my_nml % l_urban2t

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_surface
#endif

END MODULE jules_surface_mod
