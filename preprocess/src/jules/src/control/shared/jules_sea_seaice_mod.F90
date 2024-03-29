










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_sea_seaice_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains sea ice options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE c_kappai, ONLY: kappai, kappai_snow, kappa_seasurf

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Parameters defining valid values for switches
!-----------------------------------------------------------------------------
! Options for iseasurfalg
INTEGER, PARAMETER ::                                                       &
  ip_ss_solid = -1
         ! Disabling option for solid points
INTEGER, PARAMETER ::                                                       &
  ip_ss_fixed = 0
         ! Original scheme with fixed thermal roughness length
INTEGER, PARAMETER ::                                                       &
  ip_ss_surf_div = 1
         ! Thermal roughness from surface divergence, but
         ! without interactive calculation of roughness lengths
INTEGER, PARAMETER ::                                                       &
  ip_ss_surf_div_int = 2
         ! Thermal roughness from surface divergence, with
         ! interactive calculation of roughness lengths
INTEGER, PARAMETER ::                                                       &
  ip_ss_coare_mq = 3
         ! COARE algorithm, with linear dependence of Charnock's
         ! coefficient on the wind speed, but using z0q for both
         ! heat and moisture
!
! Options for i_high_wind_drag
INTEGER, PARAMETER ::                                                       &
  ip_hwdrag_null    = 0
         ! No special treatment of drag at high winds (default)
INTEGER, PARAMETER ::                                                       &
  ip_hwdrag_limited = 1
         ! The drag at high winds is capped.
!
!----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
LOGICAL ::                                                                  &
  l_tstar_sice_new = .FALSE.,                                               &
      ! Calculate sea ice surface temperature in scheme compatible with
      ! multi-categories
      ! This logical is only used in a single category run
  l_ssice_albedo = .FALSE.,                                                 &
      ! Switch for including the effect of snow on the sea-ice albedo
  l_sice_scattering = .FALSE.,                                              &
      ! Switch for seaice albedo internal scatter
  l_sice_swpen = .FALSE.,                                                   &
      ! Switch for penetration of SW radiation into sea ice
  l_sice_meltponds = .FALSE.,                                               &
      ! Sea-ice albedo affected by meltponds (simple parameterisation)
  l_sice_meltponds_cice = .FALSE.,                                          &
      ! Sea-ice albedo affected by meltponds (from CICE meltponds scheme)
  l_sice_multilayers = .FALSE.,                                             &
      ! True if coupled to sea ice multilayer model
  l_cice_alb = .FALSE.,                                                     &
      ! T = use sea ice albedo scheme from the CICE model
      ! The sea ice radiation code in control.F90 assumes this is always
      ! FALSE (standalone JULES only)
  l_sice_heatflux = .FALSE.,                                                &
      ! T: semi-implicit update of TI
  l_saldep_freeze   = .FALSE.,                                              &
      ! Switch for salinity dependent freezing to form sea ice
  l_ctile = .FALSE.,                                                        &
      ! True if coastal tiling is enabled
  l_use_dtstar_sea = .FALSE.
      ! Allow surface energy balance to modify SST

INTEGER ::                                                                  &
  nice = 0,                                                                 &
      ! Number of sea ice categories available
  nice_use = 0,                                                             &
      ! Number of sea ice categories in use
  iseasurfalg = ip_ss_fixed,                                                &
      ! Switch for the definition of the roughness lengths over the sea
  buddy_sea = 0,                                                            &
      ! Switch to use the wind speed from adjacent sea points for the sea
      ! part of coastal grid points
  i_high_wind_drag = ip_hwdrag_null
      ! Option to impose a special treatment of drag at high wind speeds.
      ! Set to the null option by default.


!-----------------------------------------------------------------------------
! Parameters
!-----------------------------------------------------------------------------
REAL ::                                                                     &
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Roughness lengths for sea ice
    z0miz = 1.0e-1,                                                           &
        ! Roughness length for heat, moisture and momentum over the
        ! Marginal Ice Zone (m)
    z0sice = 3.0e-3,                                                          &
        ! Roughness length for heat, moisture and momentum over sea-ice (m)
    z0h_z0m_miz = 1.0,                                                        &
        ! Ratio of thermal to momentum roughness lengths for marginal ice
    z0h_z0m_sice = 1.0,                                                       &
        ! Ratio of thermal to momentum roughness lengths for sea ice
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Roughness lengths for sea
    z0hsea = 4.0e-5,                                                          &
        ! Roughness length for heat, moisture and momentum over sea (m) for
        ! fixed roughness length setting (iseasurfalg = ip_ss_fixed)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Emissivities of sea and sea-ice
    emis_sea = 1.0,                                                           &
        ! Emissivity of open sea
    emis_sice = 1.0,                                                          &
        ! Emissivity of sea-ice
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Items moved here from run_bl / bl_option_mod
    SeaSalinityFactor = 1.0,                                                  &
        ! Scaling of qsat allowing for salinity of sea water
    charnock = 0.011,                                                         &
        ! Charnock parameter used in roughness length calculation over the sea
    cd_limit_sea = 0.0024,                                                    &
        ! Limit on the drag coefficient over open sea. This default is
        ! taken from 
        ! http://onlinelibrary.wiley.com/doi/10.1029/2004GL019460/pdf
! Parameters for original HadGEM albedo scheme:
    alpham = 0.72,                                                            &
        ! Albedo of sea-ice at melting point (TM) if .not.l_ssice_albedo, or
        ! Albedo of snow on sea-ice at melting point (TM) if l_ssice_albedo.
        ! "M" for "melting"
    ssalpham = 0.72,                                                          &
        ! Namelist input for alpham if l_ssice_albedo - assigned to alpham in
        ! readlsta.F90.  "M" for "melting"
    alphac = 0.80,                                                            &
       ! Albedo of sea-ice at and below TM-DTICE if .not.l_ssice_albedo, or
       ! Albedo of snow on sea-ice at and below TM-DTICE if l_ssice_albedo
       ! "C" for "cold"
    ssalphac = 0.80,                                                          &
       ! Namelist input for alphac if l_ssice_albedo - assigned to alphac in
       ! readlsta.F90. "C" for "cold"
    alphab = 0.61,                                                            &
       ! Albedo of snow-free sea-ice if l_ssice_albedo.  "B" for "bare".
    dtice = 2.0,                                                              &
       ! Temperature range in which albedo of sea-ice, if .not.l_ssice_albedo,
       ! or of snow on sea-ice, if l_ssice_albedo, varies between its limits
    ssdtice = 2.0,                                                            &
       ! Namelist input for dtice if l_ssice_albedo - assignd to dtice in
       ! readlsta.F90
    dt_bare = 1.0,                                                            &
       ! Temperature range below TM over which meltponds form if l_sice_meltponds
       ! and l_ssice_albedo
    dalb_bare_wet = -0.075,                                                   &
       ! Increment to albedo for each degree temperature rises above TM-DT_BARE.
       ! Only used if l_sice_meltponds and l_ssice_albedo
    pen_rad_frac = 0.20,                                                      &
       ! Fraction of SW radiation that penetrates seaice and scatters back
       ! causing an addition to the albedo. Only active if l_ssice_albedo and
       ! l_sice_scattering
    sw_beta = 0.60,                                                           &
      ! Attenutation parameter for SW in seaice which controls the additional
      ! albedo due to internal scattering
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Parameters for 4-band CICE albedo scheme used within JULES:
    albicev_cice = 0.78,                                                      &
      ! Sea ice albedo (visible)
    albicei_cice = 0.36,                                                      &
      ! Sea ice albedo (near-infrared)
    albsnowv_cice = 0.98,                                                     &
      ! Snow albedo (visible)
    albsnowi_cice = 0.70,                                                     &
      ! Snow albedo (near-infrared)
    albpondv_cice = 0.27,                                                     &
      ! Meltpond albedo (visible)
    albpondi_cice = 0.07,                                                     &
      ! Meltpond albedo (near-infrared)
    ahmax = 0.3,                                                              &
      ! Sea ice albedo in CICE multi-band scheme is constant above this
      ! thickness (metres)
    dalb_mlt_cice = -0.075,                                                   &
      ! Increment to sea ice albedo for each degree temperature rises above
      ! TM-DT_BARE in CICE multi-band scheme:
    dalb_mlts_v_cice = -0.1,                                                  &
      ! Increment to visible albedo of snow on sea ice for each degree
      ! temperature rises above TM-DT_BARE in CICE multi-band scheme
      ! (values for each radiation band)
    dalb_mlts_i_cice = -0.15,                                                 &
      ! Increment to infrared albedo of snow on sea ice for each degree
      ! temperature rises above TM-DT_BARE in CICE multi-band scheme
      ! (values for each radiation band)
    dt_bare_cice = 1.0,                                                       &
       ! Temperature range below TM over which meltponds form
       ! (in CICE albedo scheme)
    dt_snow_cice = 1.0,                                                       &
       ! Temperature range in which temperature of snow on sea-ice varies
       ! between its limits (in CICE albedo scheme)
    pen_rad_frac_cice = 0.2,                                                  &
      ! Fraction of SW radiation that penetrates seaice and scatters back
      ! causing an addition to the albedo in CICE multi-band scheme
    sw_beta_cice = 0.6,                                                       &
      ! Attenutation parameter for SW in seaice which controls the additional
      ! albedo due to internal scattering in the CICE multi-band scheme
    snowpatch = 0.02,                                                         &
      ! Length scale for parameterizing non uniform snow coverage (m)
      ! (used in CICE multi-band albedo scheme)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    hcap_sea = 0.0,                                                           &
        ! Value for open sea heat capacity if required to be non-zero
    beta_evap = 1.0
        ! availability of surface moisture - 0.0 = none, 1.0 = open sea

!-----------------------------------------------------------------------------
! Parameters for the COARE algorithm
!-----------------------------------------------------------------------------
!
LOGICAL :: l_10m_neut = .TRUE.
      ! Use true neutral 10m wind
REAL, PARAMETER :: z_10m = 10.0
      ! Level of wind used in setting Charnock's coefficient
REAL :: a_chrn_coare = 0.0017
      ! Linear coefficient in setting Charnock's coefficient
      ! in COARE algorithm (Default value from COARE3.5)
REAL :: b_chrn_coare = -0.005
      ! Constant in setting Charnock's coefficient
      ! in COARE algorithm (Default value from COARE3.5)
REAL :: u10_min_coare = 0.0
      ! Minimum wind speed for linear variation of Charnock's
      ! coefficient (Default value from COARE3.5)
REAL :: u10_max_coare = 19.0
      ! Maximum wind speed for linear variation of Charnock's
      ! coefficient (Default value from COARE3.5)

!-----------------------------------------------------------------------------
! Parameters for ice form drag following Lupkes et al. (2012)
!-----------------------------------------------------------------------------
!
LOGICAL :: l_iceformdrag_lupkes = .FALSE.
      ! True to use the diagnostic form drag scheme of Lupkes et al.
      ! (2012) JGR, 117, D13112 or Lupkes & Gryanik (2015) JGR, 120,
      ! p. 552
LOGICAL :: l_stability_lupkes = .FALSE.
      ! True to include the stability dependence from 
      ! Lupkes & Gryanik (2015), otherwise use the neutral form
      ! from Lupkes et al. (2012), but with the fetch-dependence
      ! from Lupkes & Gryanik (2015).
REAL    ::  h_freeboard_min   = 0.286
      ! Minimum height of freeboard
REAL    ::  h_freeboard_max   = 0.534
      ! Maximum height of freeboard
REAL    ::  beta_floe         = 1.0
      ! Constant in parametrization of crosswind length of floe
REAL    ::  d_floe_min        = 8.0
      ! Minimum crosswind length of floe
REAL    ::  d_floe_max        = 300.0
      ! Maximum crosswind length of floe
REAL    ::  ss_floe           = 0.5
      ! Sheltering constant
REAL    ::  ce_floe           = 0.222
      ! Effective resistance coefficient: NB. This is scaled from
      ! the value given under E2016A in Elvidge et al. (2016) to 
      ! account for the more accurate treatment of the airflow
      ! near the ice.

!-----------------------------------------------------------------------------
! Namelist used in UM only
!-----------------------------------------------------------------------------
NAMELIST  / jules_sea_seaice/                                                 &
! Switches
    nice, nice_use, l_tstar_sice_new, l_ssice_albedo, l_sice_scattering,      &
    l_sice_swpen, l_sice_meltponds,  l_sice_meltponds_cice,                   &
    l_sice_multilayers, l_cice_alb, l_sice_heatflux, l_saldep_freeze,         &
    l_ctile, l_iceformdrag_lupkes, l_stability_lupkes, iseasurfalg,           &
    l_10m_neut, buddy_sea, i_high_wind_drag, l_use_dtstar_sea,                &
! Parameters
    z0miz, z0sice, z0h_z0m_miz, z0h_z0m_sice, z0hsea, emis_sea, emis_sice,    &
    kappai, kappai_snow, kappa_seasurf, SeaSalinityFactor, charnock,          &
    a_chrn_coare, b_chrn_coare, u10_min_coare, u10_max_coare,                 &
    cd_limit_sea,                                                             &
    alpham, ssalpham, alphac, ssalphac, alphab, dtice, ssdtice,               &
    dt_bare,dalb_bare_wet,pen_rad_frac,sw_beta,                               &
    albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                 &
    albpondv_cice, albpondi_cice,                                             &
    ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice, dt_bare_cice,   &
    dt_snow_cice, pen_rad_frac_cice, sw_beta_cice, snowpatch,                 &
    h_freeboard_min, h_freeboard_max, beta_floe, d_floe_min, d_floe_max,      &
    ss_floe, ce_floe, hcap_sea, beta_evap



CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SEA_SEAICE_MOD'

CONTAINS

SUBROUTINE check_jules_sea_seaice()

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SEA_SEAICE namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatus

! Check iseasurfalg takes one of the allowed values
IF ( iseasurfalg /= ip_ss_fixed          .AND.                            &
     iseasurfalg /= ip_ss_surf_div       .AND.                            &
     iseasurfalg /= ip_ss_surf_div_int   .AND.                            &
     iseasurfalg /= ip_ss_coare_mq ) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                     &
               "iseasurfalg must be 0, 1, 2 or 3")
END IF

! Check the option for drag at high wind speed.
IF ( i_high_wind_drag /= ip_hwdrag_null .AND.                             &
     i_high_wind_drag /= ip_hwdrag_limited ) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                     &
               "i_high_wind_drag must be 0 (null) or 1 (limited)")
END IF

! Check buddy_sea takes one of the allowed values
IF ( buddy_sea /= 0 .AND. buddy_sea /= 1 ) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                     &
               "buddy_sea must be 0 (off) or 1 (on)")
END IF

END SUBROUTINE check_jules_sea_seaice


SUBROUTINE print_nlist_jules_sea_seaice()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_sea_seaice',                                      &
                 'Contents of namelist jules_sea_seaice')

WRITE(lineBuffer, *) '  nice = ', nice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  nice_use = ', nice_use
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_tstar_sice_new = ', l_tstar_sice_new
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_ssice_albedo = ', l_ssice_albedo
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_scattering = ', l_sice_scattering
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_swpen= ', l_sice_swpen
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_meltponds = ', l_sice_meltponds
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_meltponds_cice = ', l_sice_meltponds_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_multilayers = ', l_sice_multilayers
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_cice_alb = ', l_cice_alb
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_saldep_freeze = ', l_saldep_freeze
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_heatflux = ', l_sice_heatflux
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_ctile = ', l_ctile
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_use_dtstar_sea = ', l_use_dtstar_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  iseasurfalg = ', iseasurfalg
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  i_high_wind_drag = ', i_high_wind_drag
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_10m_neut = ', l_10m_neut
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  a_chrn_coare = ', a_chrn_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  b_chrn_coare = ', b_chrn_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  u10_min_coare = ', u10_min_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  u10_max_coare = ', u10_max_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, '(A, E10.3)') '  cd_limit_sea = ', cd_limit_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  buddy_sea = ', buddy_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0miz = ', z0miz
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0sice = ', z0sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0h_z0m_miz = ', z0h_z0m_miz
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0h_z0m_sice = ', z0h_z0m_sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0hsea = ', z0hsea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  emis_sea = ', emis_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  emis_sice = ', emis_sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  kappai = ', kappai
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  kappai_snow = ', kappai_snow
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  kappa_seasurf = ', kappa_seasurf
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  SeaSalinityFactor = ', SeaSalinityFactor
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  charnock = ', charnock
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alpham = ',alpham
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssalpham = ',ssalpham
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alphac = ',alphac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssalphac = ',ssalphac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alphab = ',alphab
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dtice = ',dtice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssdtice = ',ssdtice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_bare = ',dt_bare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_bare_wet = ',dalb_bare_wet
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' pen_rad_frac = ',pen_rad_frac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' sw_beta = ',sw_beta
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albicev_cice  = ', albicev_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albicei_cice  = ', albicei_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albsnowv_cice  = ', albsnowv_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albsnowi_cice  = ', albsnowi_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albpondv_cice  = ', albpondv_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albpondi_cice  = ', albpondi_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ahmax = ',ahmax
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlt_cice = ',dalb_mlt_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlts_v_cice = ',dalb_mlts_v_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlts_i_cice = ',dalb_mlts_i_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_bare_cice = ',dt_bare_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_snow_cice = ',dt_snow_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' pen_rad_frac_cice = ',pen_rad_frac_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' sw_beta_cice = ',sw_beta_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' snowpatch = ',snowpatch
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, L1)")' l_iceformdrag_lupkes = ', l_iceformdrag_lupkes
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, L1)")' l_stability_lupkes = ', l_stability_lupkes
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' h_freeboard_min = ', h_freeboard_min
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' h_freeboard_max = ', h_freeboard_max
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' beta_floe = ', beta_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' d_floe_min = ', d_floe_min
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' d_floe_max = ', d_floe_max
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' ss_floe = ', ss_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' ce_floe = ', ce_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)") ' hcap_sea = ', hcap_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)") ' beta_evap = ', beta_evap
CALL jules_print('jules_sea_seaice', lineBuffer)




END SUBROUTINE print_nlist_jules_sea_seaice


END MODULE jules_sea_seaice_mod
