










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_vegetation_mod

USE max_dimensions, ONLY: npft_max, nsurft_max
USE missing_data_mod, ONLY: rmdi

!-----------------------------------------------------------------------------
! Description:
!   Contains vegetation options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER ::                                                       &
  i_veg_vn_1b = 1,                                                          &
      ! Constant indicating fixed vegetation scheme
  i_veg_vn_2b = 2
      ! Constant indicating interactive vegetation scheme

!-----------------------------------------------------------------------------
! Items set in namelist
!-----------------------------------------------------------------------------
LOGICAL ::                                                                  &
  l_nrun_mid_trif = .FALSE.,                                                &
      ! Switch for starting NRUN mid-way through a TRIFFID period
  l_trif_init_accum = .TRUE.,                                               &
      ! Switch so that an NRUN will bit-compare with a CRUN when FALSE
  l_phenol = .FALSE.,                                                       &
      ! Switch for leaf phenology
  l_triffid = .FALSE.,                                                      &
      ! Switch for interactive veg model
  l_trif_eq = .FALSE.,                                                      &
      ! Switch for running TRIFFID in equilibrium mode
  l_veg_compete  = .TRUE.,                                                  &
      ! Switch for competing vegetation
      ! Setting l_triffid = .TRUE. and this as .FALSE. means that the carbon
      ! pools evolve but the PFT distribution does not change
      ! The default of .TRUE. means that enabling TRIFFID has competing veg
      ! on by default
  l_trait_phys  = .FALSE.,                                                  &
      ! .TRUE. for new trait-based PFTs (uses nmass & lma)
      ! .FALSE. for pre-new PFT configuration (nl0, sigl, and neff)
  l_ht_compete  = .FALSE.,                                                  &
      ! Switch for TRIFFID competition
      ! (T for height F for lotka)
      ! Must be true if npft > 5!
  l_landuse = .FALSE.,                                                      &
      ! Switch for landuse change that invokes wood product pools
  l_nitrogen = .FALSE.,                                                     &
      ! Switch for Nitrogen limiting NPP
  l_bvoc_emis = .FALSE.,                                                    &
      ! Switch to enable calculation of BVOC emissions
  l_o3_damage    = .FALSE.,                                                 &
      ! Switch for ozone damage
  l_prescsow = .FALSE.,                                                     &
      ! Only used if crop model is on ( ncpft > 0 )
      !   T => read in the sowing dates for each crop
      !   F => let the model determine sowing date
  l_irrig_dmd = .FALSE.,                                                    &
      !   Switch for using irrigation demand code
  l_irrig_limit = .FALSE.,                                                  &
      !   Switch for limiting irrigation supply
  l_recon = .TRUE.,                                                         &
      ! Used to switch on reconfiguration of veg fractions for TRIFFID
  l_trif_crop = .FALSE.,                                                    &
      ! switch to prevent crop and natural PFTs competing
  l_inferno = .FALSE.,                                                      &
      ! Switch used to control whether the Interactive fire scheme is used
  l_trif_fire = .FALSE.,                                                    &
      ! Switch used to control whether interactive fire is used
      !   T => if l_inferno is also true, g_burn is calculated in INFERNO
!   and passed to TRIFFID to calculate emissions and vegetation
       !   dynamics
       !   T => if l_inferno is false, interactive fire is calculated via
!   ancillary if provided, and is 0 if not provided
       !   F => g_burn is calculated via ancillary if provided, and is 0 if
!   not provided
   l_use_pft_psi = .FALSE.,                                                  &
       ! Switch used to control what parameters are used in the calculation
       ! of the soil moisture stress factor
       !   T => use psi_close and psi_open
       !   F => use sm_wilt, sm_crit and fsmc_p0

! Switches for bug fixes.
    l_leaf_n_resp_fix = .FALSE.,                                              &
        ! Switch to use correct forms for canopy-average leaf nitrogen.
        ! This affects can_rad_mod=1 to 5, not 6 (which is correct).
    l_stem_resp_fix = .FALSE.,                                                &
        ! Switch used to control whether LAI or LAI_BAL is used in stem
        ! resp calculation. Only affects non-crop PFTs with l_trait_phys=F.
    l_vegcan_soilfx = .FALSE.,                                                &
        ! Switch to modify the canopy model to allow for conduction through
        ! the soil below vegetation.
    l_gleaf_fix = .FALSE.,                                                    &
        ! Used to fix a bug accumulating g_leaf_phen_ac in standalone JULES
        !
        ! This bug occurs in standalone JULES because veg2 is called on TRIFFID
        ! timesteps and veg1 is called on phenol timesteps
        !
        ! In the UM, veg2 (where the accumulation is working) is called on
        ! TRIFFID AND phenol timesteps if l_triffid = TRUE and veg1 is called
        ! on phenol timesteps if l_triffid = FALSE, hence this bug doesn't arise
        !
        ! This means we don't bother adding it to the UM namelist transfer
        ! between PEs
    l_scale_resp_pm = .FALSE.,                                                &
        ! Switch for scaling whole plant maintenance respiraiton by the soil
        ! moisture stress factor. FALSE = Only scale leaf respiration;
        ! TRUE = scale whole plant respiration.
    l_vegdrag_surft(nsurft_max),                                              &
        ! Switch for using vegetation canopy drag scheme on each tile.
        ! Must be false for non-PFT tiles
    l_vegdrag_pft(npft_max),                                                  &
        ! Switch for using vegetation canopy drag scheme.
        ! This is what appears in the namelist, to ensure that true
        ! values can only given for pfts
    l_rsl_scalar = .FALSE.
        ! Switch for using roughness sublayer correction scheme in scalar
        ! variables. This is only valid when l_vegdrag_surft = .true.

DATA l_vegdrag_surft / nsurft_max * .FALSE. /
DATA l_vegdrag_pft / npft_max * .FALSE. /

INTEGER ::                                                                  &
  can_model = 4,                                                            &
      ! Switch for thermal vegetation
  can_rad_mod = 4,                                                          &
      ! Canopy radiation model
  ilayers = 10,                                                             &
      ! Number of layers for canopy radiation model
  irr_crop = 0,                                                             &
      ! Switch for irrigation cropping model, default is 0.
  ignition_method = 1,                                                      &
      ! Switch for the calculation method of INFERNO fire ignitions
      ! IGNITION_METHOD=1:Constant (1.67 per km2 per s)
      ! IGNITION_METHOD=2:Constant (Human - 1.5 per km2 per s)
      !                   Varying  (Lightning - see Pechony and Shindell,2009)
      ! IGNITION_METHOD=3:Vary Human and Lightning (Pechony and Shindell,2009)
  fsmc_shape = 0
      ! shape of the soil moisture stress function fsmc
      ! 0: piece-wise linear in vol. soil moisture.
      ! 1: piece-wise linear in soil potential.

INTEGER ::                                                                  &
  phenol_period = -32768,                                                   &
      ! Update frequency for leaf phenology (days)
  triffid_period = -32768
      ! Update frequency for TRIFFID (days)

INTEGER :: errcode   ! error code to pass to ereport.

REAL ::                                                                     &
  frac_min  = 1.0e-6,                                                       &
      ! Minimum areal fraction for PFTs.
  frac_seed = 0.01,                                                         &
      ! "Seed" fraction for PFTs.
  pow = 20.0,                                                               &
      ! Power in sigmoidal function.
  cd_leaf = rmdi,                                                           &
      ! Leaf level drag coefficient
  c1_usuh = rmdi,                                                           &
      ! u*/U(h) at the top of dense canopy
  c2_usuh = rmdi,                                                           &
      ! u*/U(h) above bare soil
  c3_usuh = rmdi,                                                           &
      ! Used in the exponent of equation weighting dense and sparse
      ! vegetation to get u*/U(h) in neutral condition
  stanton_leaf = rmdi
      ! Leaf-level Stanton number



!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_vegetation/                                                 &
! UM only
    l_nrun_mid_trif, l_trif_init_accum,                                       &
! Shared
    l_phenol, l_triffid, l_trif_eq, l_veg_compete,                            &
    phenol_period, triffid_period, l_trait_phys, l_ht_compete,                &
    l_bvoc_emis, l_o3_damage, can_model, can_rad_mod, ilayers,                &
    frac_min, frac_seed, pow, l_landuse, l_leaf_n_resp_fix, l_stem_resp_fix,  &
    l_nitrogen, l_vegcan_soilfx, l_trif_crop, l_trif_fire,                    &
    l_inferno, ignition_method, l_vegdrag_pft, l_rsl_scalar,                  &
    cd_leaf, c1_usuh, c2_usuh, c3_usuh, stanton_leaf,                         &
! Not used in the UM yet
    l_prescsow, l_recon, l_irrig_dmd, l_irrig_limit, irr_crop,l_gleaf_fix,    &
    l_scale_resp_pm, l_use_pft_psi, fsmc_shape

!-----------------------------------------------------------------------------
! Items derived from namelist inputs
!-----------------------------------------------------------------------------
INTEGER :: i_veg_vn = -32768
    ! Switch to determine version of vegetation scheme
    ! Must be one of i_veg_vn_1b or i_veg_vn_2b

LOGICAL :: l_crop = .FALSE.
    ! Indicates if the crop model is on
    ! This is derived from the value of ncpft

LOGICAL :: l_fapar_diag = .FALSE.
    ! Only true if fapar is in one of the output profiles

LOGICAL :: l_fao_ref_evapotranspiration = .FALSE.
    ! Only true if fao_et0 is in one of the output profiles

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_VEGETATION_MOD'

CONTAINS


SUBROUTINE check_jules_vegetation()

USE ereport_mod, ONLY: ereport

USE jules_surface_types_mod, ONLY: npft, ncpft, nnpft

USE jules_surface_mod, ONLY: l_aggregate

USE jules_hydrology_mod, ONLY: l_top

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_VEGETATION namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE


! Phenology or TRIFFID cannot be used with the aggregate surface scheme
IF ( l_aggregate .AND. (l_phenol .OR. l_triffid) ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Phenology or TRIFFID cannot be used with the ' //         &
               'aggregated surface scheme (i.e. l_aggregate = true)')
END IF

! Check that phenol_period is specified if phenology is on
IF ( l_phenol .AND. phenol_period < 0 ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Phenology is on but phenol_period is not given')
END IF
! Same for triffid_period if TRIFFID is on
IF ( l_triffid .AND. triffid_period < 0 ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'TRIFFID is on but triffid_period is not given')
END IF

! Turn off options that depend on TRIFFID if it is not enabled
IF ( .NOT. l_triffid ) THEN
  l_veg_compete = .FALSE.
  l_trif_eq = .FALSE.
  l_landuse = .FALSE.
  l_ht_compete = .FALSE.
  l_nitrogen = .FALSE.
  l_trif_crop = .FALSE.
  l_trif_fire = .FALSE.
END IF

! Always make sure that a veg version is selected
! If TRIFFID is on, select interactive veg, otherwise select fixed veg
i_veg_vn = i_veg_vn_1b
IF ( l_triffid ) THEN
  i_veg_vn = i_veg_vn_2b
END IF

! Check can_model and can_rad_mod are suitable
IF ( can_model < 1 .OR. can_model > 4 ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'can_model should be in range 1 to 4')
END IF

IF ( can_model == 4 .AND. l_aggregate ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'can_model=4 cannot be used with the aggregated ' //       &
               'surface scheme')
END IF

IF ( can_rad_mod < 1 .OR. can_rad_mod > 6 ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'can_rad_mod should be in range 1 to 6')
END IF

! Issue a warning if a deprecated value of can_rad_mod is selected.
SELECT CASE ( can_rad_mod )
CASE ( 2, 3 )
  errcode = -105
  CALL ereport("check_jules_vegetation", errcode,                      &
             'can_rad_mod=2 and 3 are deprecated and will be ' //      &
             'removed in future.')
END SELECT

IF ( l_triffid .AND. ( .NOT. l_phenol ) ) THEN
  errcode = -105 ! warning
  CALL ereport("check_jules_vegetation", errcode,                         &
               "When triffid is on, l_phenol=T is recommended. You " //   &
               "have set l_phenol=F. The LAI will be set to the " //      &
               "balanced LAI.")
END IF

! Check triffid-crop options are sensible
IF ( l_trif_crop .AND. l_trif_eq ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'trif_crop and trif_eq are incompatible')
END IF

IF (l_veg_compete .AND. ( .NOT. l_ht_compete ) .AND. ( nnpft /= 5 )) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                             &
               'l_ht_compete=F requires 5 natural PFTs: ' //              &
               'BT, NT, C3, C4, SH')
END IF

! Check crop options are sensible
l_crop = ncpft > 0

IF ( l_crop .AND. l_triffid ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Crop model and triffid are incompatible')
END IF

IF ( l_aggregate .AND. l_crop ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Crop model cannot be used with the aggregated surface ' //&
               'scheme (i.e. l_aggregate = true)')
END IF

! Irrigation demand is currently not available in the UM

IF ( l_irrig_limit .AND. .NOT. l_irrig_dmd ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'l_irrig_limit=T requires l_irrig_dmd=T ')
END IF

IF ( l_irrig_limit .AND. .NOT. l_top ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'l_irrig_limit=T requires l_top=T ')
END IF

IF ( l_irrig_dmd .AND. irr_crop == 2 .AND. .NOT. l_crop ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Irrigation triggered by crop DVI (irr_crop = 2) ' //      &
               'requires crop model to be active')
END IF

IF ( .NOT. l_crop ) THEN
  ! Note that in the UM, since ncpft_max = 0, l_crop is always .FALSE. and hence
  ! l_prescsow is .FALSE.
  l_prescsow = .FALSE.
END IF

! Check a suitable ignition_method was given
IF ( ignition_method < 1 .OR. ignition_method > 3 ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'ignition_method must be 1, 2 or 3')
END IF


IF ( fsmc_shape == 1 .AND. .NOT. l_use_pft_psi ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'fsmc_shape=1 requires l_use_pft_psi=T ')
END IF

IF ( l_aggregate .AND. ANY(l_vegdrag_pft) ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                         &
               'Vegetative drag scheme cannot be used with the ' //       &
               'aggregated surface scheme (i.e. l_aggregate = true)')
ELSE
  ! Copy the values for the given number of pfts across
  l_vegdrag_surft(1:npft) = l_vegdrag_pft(1:npft)
END IF

END SUBROUTINE check_jules_vegetation

SUBROUTINE print_nlist_jules_vegetation()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_vegetation_mod',                                  &
                 'Contents of namelist jules_vegetation')

WRITE(lineBuffer,*)' l_nrun_mid_trif = ', l_nrun_mid_trif
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_trif_init_accum = ', l_trif_init_accum
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_phenol = ',l_phenol
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_triffid = ',l_triffid
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_trif_eq = ',l_trif_eq
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_veg_compete = ',l_veg_compete
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_ht_compete = ',l_ht_compete
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_trif_crop = ',l_trif_crop
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_trif_fire = ',l_trif_fire
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_trait_phys = ',l_trait_phys
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_landuse = ',l_landuse
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_leaf_n_resp_fix = ',l_leaf_n_resp_fix
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_stem_resp_fix = ',l_stem_resp_fix
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_scale_resp_pm = ',l_scale_resp_pm
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_nitrogen = ',l_nitrogen
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*)' l_vegcan_soilfx = ',l_vegcan_soilfx
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' phenol_period = ',phenol_period
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' triffid_period = ',triffid_period
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_bvoc_emis = ',l_bvoc_emis
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_o3_damage = ',l_o3_damage
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_vegdrag_pft = ',l_vegdrag_pft
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_rsl_scalar = ',l_rsl_scalar
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' can_model = ',can_model
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' can_rad_mod = ',can_rad_mod
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' ilayers = ',ilayers
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' frac_min = ',frac_min
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' frac_seed = ',frac_seed
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' pow = ',pow
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' cd_leaf = ',cd_leaf
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' c1_usuh = ',c1_usuh
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' c2_usuh = ',c2_usuh
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' c3_usuh = ',c3_usuh
CALL jules_print('jules_vegetation_mod',lineBuffer)

WRITE(lineBuffer,*) ' stanton_leaf = ',stanton_leaf
CALL jules_print('jules_vegetation_mod',lineBuffer)

END SUBROUTINE print_nlist_jules_vegetation


END MODULE jules_vegetation_mod
