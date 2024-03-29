! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Version 2A of vegetation section: models leaf phenology and vegetation
! competition

! Subroutine Interface:
MODULE veg2_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG2_MOD'

CONTAINS

SUBROUTINE veg2(                                                              &
               land_pts, nsurft, a_step,                                      &
               phenol_period, triffid_period,                                 &
               trif_pts, trif_index, atimestep,                               &
               fraca, fracp, frac_vs,                                         &
               satcon_soilt_sfc, clay, z0m_soil,                              &
               l_phenol, l_triffid, l_trif_eq,                                &
               asteps_since_triffid,                                          &
               g_leaf_ac, g_leaf_phen_ac, npp_ac,                             &
               resp_s_ac, resp_w_ac,                                          &
               cs, frac, lai, ht,                                             &
               catch_s, catch_t, infil_t, z0_t, z0h_t, c_veg, cv,             &
               g_leaf_day, g_leaf_phen, g_leaf_dr_out,                        &
               lai_phen, lit_c, lit_c_mn, npp_dr_out,                         &
               resp_w_dr_out, resp_s_dr_out                                   &
               )

!Use in relevant subroutines
USE sparm_mod,             ONLY: sparm
USE infiltration_rate_mod, ONLY: infiltration_rate
USE tilepts_mod,           ONLY: tilepts
#if !defined(UM_JULES)
USE soilcarb_mix_mod, ONLY: soilcarb_mix
#endif

!Use in variables
USE ancil_info, ONLY: dim_cslayer

USE conversions_mod, ONLY: rsec_per_day

USE jules_surface_types_mod, ONLY: npft, nnpft, ncpft, ntype

USE jules_vegetation_mod, ONLY: frac_min, l_crop,                             &
                                l_trif_crop, l_trait_phys, can_model,         &
                                l_inferno, l_trif_fire

USE veg_param, ONLY: agric, litc_norm

USE jules_surface_mod, ONLY: l_aggregate, i_aggregate_opt, cmass

USE jules_soil_biogeochem_mod, ONLY:                                          &
  ! imported scalar parameters
  soil_model_rothc,                                                           &
  ! imported scalar variables
  l_layeredC, soil_bgc_model, tau_lit

USE jules_soil_mod, ONLY: dzsoil

USE descent, ONLY: gamma_eq, iter_eq

USE pftparm, ONLY: a_ws, eta_sl, a_wl, b_wl, lma, sigl

USE trif_vars_mod, ONLY: cnsrv_carbon_veg2_gb, exudates_pft, g_burn_pft_acc,  &
                         g_burn_pft, exudates_gb, lai_bal_pft, leafc_pft,     &
                         rootc_pft, woodc_pft

USE prognostics, ONLY: wood_prod_fast_gb, wood_prod_med_gb,                   &
                       wood_prod_slow_gb

USE crop_vars_mod, ONLY: rootc_cpft, harvc_cpft, reservec_cpft,               &
                          stemc_diag_cpft, leafc_diag_cpft

USE ancil_info, ONLY: nsoilt

USE calc_c_comps_triffid_mod, ONLY:                                           &
  ! imported procedures
  calc_c_comps_triffid

USE phenol_mod, ONLY: phenol 

USE triffid_mod, ONLY: triffid

USE jules_print_mgr, ONLY:                                                    &
    jules_message,                                                            &
    jules_print

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Description:
!   Updates Leaf Area Index for Plant Functional Types (PFTs) and uses
!   this to derive new vegetation parameters for PFTs along with gridbox
!   mean values where appropriate.

! Method:
!   Calls PHENOL which models phenolgy and updates Leaf Area Index
!   (LAI), then calls TRIFFID to update vegetation and soil fractions,
!   LAI, canopy height, veg and soil carbon and carbon fluxes.  Passes
!   fractions, LAI and canopy height to SPARM which derives the
!   vegetation parameters for each PFT and also the gridbox means where
!   this is required.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
    ! Number of land points.
  nsurft,                                                                     &
    ! Number of land-surface tiles.
  a_step,                                                                     &
    ! Atmospheric timestep number.
  phenol_period,                                                              &
    ! Phenology period (days).
  triffid_period,                                                             &
    ! TRIFFID period (days).
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER, INTENT(IN) ::                                                        &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL, INTENT(IN) ::                                                           &
  atimestep
    ! Atmospheric timestep (s).

REAL, INTENT(IN) ::                                                           &
  fraca(land_pts),                                                            &
    ! Fraction of agriculture.
  fracp(land_pts),                                                            &
    ! Fraction of pasture.
  frac_vs(land_pts),                                                          &
    ! Total fraction of gridbox covered by veg or soil.
  satcon_soilt_sfc(land_pts,nsoilt),                                          &
    ! Saturated hydraulic conductivity of the soil surface (kg/m2/s).
  clay(land_pts,dim_cslayer),                                                 &
    ! Clay fraction of soil.
  z0m_soil(land_pts)
    ! Roughness of bare soil, for momentum (m).

LOGICAL, INTENT(IN) ::                                                        &
  l_phenol,                                                                   &
    ! .T. for interactive leaf phenology.
  l_triffid,                                                                  &
    ! .T. for interactive vegetation.
  l_trif_eq
    ! .T. for vegetation equilibrium.

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
INTEGER, INTENT(INOUT) ::                                                     &
  asteps_since_triffid
    ! Number of atmosphere timesteps since last call to TRIFFID.

REAL, INTENT(INOUT) ::                                                        &
  g_leaf_ac(land_pts,npft),                                                   &
    ! Accumulated leaf turnover rate ([360days]-1).
  g_leaf_phen_ac(land_pts,npft),                                              &
    ! Accumulated leaf turnover rate including phenology ([360days]-1).
  npp_ac(land_pts,npft),                                                      &
    ! Accumulated NPP (kg C/m2).
  resp_s_ac(land_pts,dim_cslayer,4),                                          &
    ! Accumulated soil respiration (kg C/m2).
  resp_w_ac(land_pts,npft),                                                   &
    ! Accumulated wood respiration (kg C/m2).
  cs(land_pts,dim_cslayer,4),                                                 &
    ! Soil carbon content (kg m-2).
  frac(land_pts,ntype),                                                       &
    ! Fractions of surface types.
  lai(land_pts,npft),                                                         &
    ! LAI of plant functional types.
  ht(land_pts,npft)
    ! Height of plant functional types (m).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL, INTENT(OUT) ::                                                          &
  catch_s(land_pts,nsurft),                                                   &
    ! Snow capacity for tiles (kg/m2).
  catch_t(land_pts,nsurft),                                                   &
    ! Canopy capacity for tiles (kg/m2).
  infil_t(land_pts,nsurft),                                                   &
    ! Maximum surface infiltration rate for tiles (kg/m2/s).
  z0_t(land_pts,nsurft),                                                      &
    ! Roughness length for tiles (m).
  z0h_t(land_pts,nsurft),                                                     &
    ! Thermal roughness length for tiles (m).
  c_veg(land_pts,npft),                                                       &
    ! Total carbon content of the vegetation (kg C/m2).
  cv(land_pts),                                                               &
    ! Gridbox mean vegetation carbon (kg C/m2).
  g_leaf_day(land_pts,npft),                                                  &
    ! Mean leaf turnover rate for input to PHENOL (/360days).
  g_leaf_phen(land_pts,npft),                                                 &
    ! Mean leaf turnover rate over phenology period (/360days).
  g_leaf_dr_out(land_pts,npft),                                               &
    ! Mean leaf turnover rate for driving TRIFFID (/360days).
  lai_phen(land_pts,npft),                                                    &
    ! LAI of PFTs after phenology.
  lit_c(land_pts,npft),                                                       &
    ! Carbon Litter (kg C/m2/360days).
  lit_c_mn(land_pts),                                                         &
    ! Gridbox mean carbon litter (kg C/m2/360days).
  npp_dr_out(land_pts,npft),                                                  &
    ! Mean NPP for driving TRIFFID (kg C/m2/360days).
  resp_w_dr_out(land_pts,npft),                                               &
    ! Mean wood respiration for driving TRIFFID (kg C/m2/360days).
  resp_s_dr_out(land_pts,dim_cslayer,5)
    ! Mean soil resp (kg C/m2/360days).
    ! NB 5=dim_cs1+1. The 5th element is used for the total.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
! Parameters used to calculate resp_frac from clay fraction.
REAL,PARAMETER  :: resp_frac_a = 4.0895
REAL,PARAMETER  :: resp_frac_b = 2.672
REAL,PARAMETER  :: resp_frac_c=-0.0786

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  j,k,l,n,nn,                                                                 &
    ! Loop counters.
  kiter,                                                                      &
    ! Number of TRIFFID iterations.
  nstep_phen,                                                                 &
    ! Number of atmospheric timesteps between calls to PHENOL.
  nstep_trif,                                                                 &
    ! Number of atmospheric timesteps between calls to TRIFFID.
  t
    ! Loop counter.

INTEGER ::                                                                    &
  surft_pts(ntype),                                                           &
    ! Number of land points which include the nth surface type.
  surft_index(land_pts,ntype)
    ! Indices of land points which include the nth surface type.

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL ::                                                                       &
  dtime_phen,                                                                 &
    ! The phenology timestep (yr).
  forw,                                                                       &
    ! Forward timestep weighting for TRIFFID.
  r_gamma,                                                                    &
    ! Inverse TRIFFID timestep (/360days).
  gam_trif,                                                                   &
    ! Inverse TRIFFID coupling timestep (/360days).
  ratio,                                                                      &
    ! Ratio of fractional coverage before to that after TRIFFID.
  denom_resp,                                                                 &
    ! Denominator for calc resp_s_ac.
  lit_resp
    ! Multiplier for calc resp_s_ac.

REAL ::                                                                       &
  dcs(land_pts,dim_cslayer),                                                  &
    ! Change in soil carbon (kg C/m2).
  dcs_rothc(land_pts,dim_cslayer,5),                                          &
    ! Change in soil carbon (kg C/m2).
  frac_agric(land_pts),                                                       &
    ! Fraction of agriculture as seen by TRIFFID.
  frac_past(land_pts),                                                        &
    ! Fraction of pasture as seen by TRIFFID.
  frac_old(land_pts,ntype),                                                   &
    ! Fractions of surface types before the call to TRIFFID.
  g_leaf_dr(land_pts,npft),                                                   &
    ! Mean leaf turnover rate for driving TRIFFID (/360days).
  npp_dr(land_pts,npft),                                                      &
    ! Mean NPP for driving TRIFFID (kg C/m2/360days).
  resp_w_dr(land_pts,npft),                                                   &
    ! Mean wood respiration for driving TRIFFID (kg C/m2/360days).
  resp_s_dr(land_pts,dim_cslayer,5),                                          &
    ! Mean soil resp for driving TRIFFID (kg C/m2/360days).
    ! NB 5=dim_cs1+1. The 5th element is used as workspace.
  cs_tot(land_pts,dim_cslayer),                                               &
    ! Soil carbon content (kg C/m2).
  cv_trif(land_pts),                                                          &
    ! Gridbox mean natural pft vegetation carbon(kg C/m2). Not currently used.
  resp_frac(land_pts,dim_cslayer),                                            &
    ! The fraction of soil respiration that forms new soil C.
    ! This is the fraction that is NOT released to the atmosphere.
  lit_frac(dim_cslayer),                                                      &
    ! Litter fraction into each layer.
  cnsrv_carbon_flux(land_pts),                                                &
    ! Net carbon flux into land (vegetation, soil, wood products) (kg m-2).
    ! Used to assess carbon conservation.
  burnt_soil(land_pts),                                                       &
    ! burnt DPM and RPM for soil respiration correction (kg m-2 360d-1)
  cnsrv_veg2_correction(land_pts),                                            &
    ! Corrections to carbon conservation diagnostic 
    ! accounting for processes not explicitly represented in the veg2 
    ! subroutine: Change to wood product pools, crop harvest and fire 
    ! emissions (kg m-2).
  cnsrv_soil_resp(land_pts),                                                  &
    ! Carbon flux into atmosphere from soil respiration (kg/m2/360days).
#if !defined(UM_JULES)
  mix_s(land_pts,dim_cslayer-1,4),                                            &
    ! Diffusion coefficient for soil C between soil layers (m^2/360days)
    ! Equation 15 of Burke et al. (2017)
    ! https://www.geosci-model-dev.net/10/959/2017/gmd-10-959-2017.pdf
#endif
  mix_term(land_pts,dim_cslayer,4)
    ! Mixing term for calculating respiration correction (kgC/m^2/360days)

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VEG2'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------------
DO n = 1,npft
  DO l = 1,land_pts
    g_leaf_phen(l,n) = 0.0
    g_leaf_day(l,n)  = 0.0
    g_leaf_dr(l,n)   = 0.0
    npp_dr(l,n)      = 0.0
    resp_w_dr(l,n)   = 0.0
    c_veg(l,n)       = 0.0
    lit_c(l,n)       = 0.0
  END DO
END DO

DO n = 1,nsurft
  DO l = 1,land_pts
    catch_t(l,n) = 0.0
    infil_t(l,n) = 0.0
    z0_t(l,n)    = 0.0
  END DO
END DO

IF (l_aggregate .AND. i_aggregate_opt == 1) z0h_t(:,:) = 0.0

IF (can_model == 4) THEN
  DO n = 1,nsurft
    DO l = 1,land_pts
      catch_s(l,n) = 0.0
    END DO
  END DO
END IF

DO l = 1,land_pts
  cv(l) = 0.0
  lit_c_mn(l)   = 0.0
  frac_agric(l) = 0.0
  frac_past(l)  = 0.0
END DO

IF ( soil_bgc_model == soil_model_rothc ) THEN
  DO n = 1,5
    DO l = 1,land_pts
      DO nn = 1,dim_cslayer
        resp_s_dr(l,nn,n) = 0.0
      END DO
    END DO
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate the number of atmospheric timesteps between calls to PHENOL
! and TRIFFID.
!-----------------------------------------------------------------------------
nstep_phen = INT(rsec_per_day * phenol_period / atimestep)
nstep_trif = INT(rsec_per_day * triffid_period / atimestep)

IF ( ( l_triffid .AND. asteps_since_triffid == nstep_trif ) .AND.             &
     ( soil_bgc_model == soil_model_rothc ) ) THEN
  !---------------------------------------------------------------------------
  ! Check carbon conservation (1/3)
  ! Calculate total land carbon store and net carbon flux at start of
  ! routine. The wood product decay flux has not yet been calculated and
  ! will be added later.
  !---------------------------------------------------------------------------
  DO t = 1,trif_pts
    l = trif_index(t)
    cnsrv_carbon_veg2_gb(l) = 0.0

    DO n = 1,nnpft
      CALL calc_c_comps_triffid(n, ht(l,n), lai_bal_pft(l,n), leafc_pft(l,n), &
                                rootc_pft(l,n), woodc_pft(l,n), c_veg(l,n))
      cnsrv_carbon_veg2_gb(l) = cnsrv_carbon_veg2_gb(l)                       &
                                + frac(l,n) * c_veg(l,n)
    END DO

    cnsrv_carbon_veg2_gb(l) = cnsrv_carbon_veg2_gb(l)                         &
                              + SUM(cs(l,:,:))                                &
                              + wood_prod_fast_gb(l) + wood_prod_med_gb(l)    &
                              + wood_prod_slow_gb(l)
    ! Calculate RESP_FRAC.
    ! NOTE - here we want to use (1-X) - i.e. the fraction of
    !        respiration NOT released to the atmos, so RESP_FRAC
    !        here is 1-RESP_FRAC as calc'd in BL_CTL.
    DO nn = 1,dim_cslayer
      resp_frac(l,nn) = 1.0 / (resp_frac_a + resp_frac_b *                    &
                               EXP(resp_frac_c * 100.0 * clay(l,nn) ))
    END DO

    cnsrv_carbon_flux(l) = SUM(npp_ac(l,1:npft) * frac(l,1:npft))
  END DO
END IF  !  TRIFFID step and RothC

!-----------------------------------------------------------------------------
! Create the surft_index array of land points with each surface type
!-----------------------------------------------------------------------------
CALL tilepts(land_pts, frac, surft_pts, surft_index)

IF (l_phenol .AND. MOD(a_step,nstep_phen) == 0) THEN

  !---------------------------------------------------------------------------
  ! Calculate the phenology timestep in years.
  !---------------------------------------------------------------------------
  dtime_phen = REAL(phenol_period) / 360.0

  DO n = 1,nnpft

    !-------------------------------------------------------------------------
    ! Calculate the mean turnover rate and update the leaf phenological
    ! state, and take copy of updated LAI field for output as diagnostic.
    !-------------------------------------------------------------------------
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      g_leaf_day(l,n) = g_leaf_ac(l,n) / dtime_phen
    END DO
    
    CALL phenol (land_pts, surft_pts(n), n, surft_index(:,n),                 &
                 dtime_phen, g_leaf_day(:,n), ht(:,n),                        &
                 lai(:,n), g_leaf_phen(:,n))

    DO l = 1,land_pts
      lai_phen(l,n) = lai(l,n)
    END DO

    !-------------------------------------------------------------------------
    ! Increment the leaf turnover rate for driving TRIFFID and reset the
    ! accumulation over atmospheric model timesteps to zero.
    !-------------------------------------------------------------------------
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      g_leaf_phen_ac(l,n) = g_leaf_phen_ac(l,n)                               &
                            + g_leaf_phen(l,n) * dtime_phen
    END DO

    DO l = 1,land_pts
      g_leaf_ac(l,n) = 0.0
    END DO

  END DO
END IF  !  phenology step

!-----------------------------------------------------------------------------
! Call TRIFFID vegetation model to update vegetation and terrestrial
! carbon storage.
!-----------------------------------------------------------------------------
IF (l_triffid .AND.                                                           &
   (asteps_since_triffid == nstep_trif)) THEN

  !---------------------------------------------------------------------------
  ! Calculate the TRIFFID inverse coupling timestep.
  !---------------------------------------------------------------------------
  gam_trif = 360.0 / REAL(triffid_period)

  !---------------------------------------------------------------------------
  ! Diagnose the mean fluxes over the coupling period.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model == soil_model_rothc ) THEN
    DO t = 1,trif_pts
      l = trif_index(t)
      DO nn = 1,dim_cslayer
        resp_s_dr(l,nn,1) = resp_s_ac(l,nn,1) * gam_trif
        resp_s_dr(l,nn,2) = resp_s_ac(l,nn,2) * gam_trif
        resp_s_dr(l,nn,3) = resp_s_ac(l,nn,3) * gam_trif
        resp_s_dr(l,nn,4) = resp_s_ac(l,nn,4) * gam_trif
      END DO
    END DO
  END IF

  DO n = 1,nnpft
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      g_leaf_dr(l,n) = g_leaf_phen_ac(l,n) * gam_trif
      npp_dr(l,n)    = npp_ac(l,n) * gam_trif
      resp_w_dr(l,n) = resp_w_ac(l,n) * gam_trif

      IF (l_inferno .AND. l_trif_fire) THEN
        g_burn_pft(l,n) = g_burn_pft_acc(l,n) * gam_trif
      END IF

    END DO
  END DO

  !---------------------------------------------------------------------------
  ! Diagnose the mean leaf turnover rates over the coupling period.
  !---------------------------------------------------------------------------
  IF (l_phenol) THEN
    DO n = 1,nnpft
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        g_leaf_dr(l,n) = g_leaf_phen_ac(l,n) * gam_trif
      END DO
    END DO
  ELSE
    DO n = 1,nnpft
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        g_leaf_dr(l,n) = g_leaf_ac(l,n) * gam_trif
      END DO
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Define the agricultural regions.
  !---------------------------------------------------------------------------
  IF (agric) THEN
    DO l = 1,land_pts
      frac_agric(l) = MIN(fraca(l),frac_vs(l) - (REAL(nnpft+1) * frac_min))
      frac_agric(l) = MAX(frac_agric(l),0.0)
    END DO
  END IF
  IF (l_trif_crop) THEN
    DO l = 1,land_pts
      frac_past(l) = MIN(fracp(l),                                            &
                         frac_vs(l) - (REAL(nnpft+1) * frac_min)              &
                         - frac_agric(l))
      frac_past(l) = MAX(frac_past(l),0.0)
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Take copies of TRIFFID input variables for output as diagnostics.
  !---------------------------------------------------------------------------
  DO n = 1,nnpft
    DO l = 1,land_pts
      g_leaf_dr_out(l,n) = g_leaf_dr(l,n)
      npp_dr_out(l,n)    = npp_dr(l,n)
      resp_w_dr_out(l,n) = resp_w_dr(l,n)
      frac_old(l,n)      = frac(l,n)
    END DO
  END DO

  IF ( soil_bgc_model == soil_model_rothc ) THEN
    DO n = 1,4
      DO l = 1,land_pts
        DO nn = 1,dim_cslayer
          resp_s_dr_out(l,nn,n) = resp_s_dr(l,nn,n)
        END DO
      END DO
    END DO
    DO l = 1,land_pts
      DO nn = 1,dim_cslayer
        ! Save starting values of soil C.
        dcs(l,nn) = cs(l,nn,1) + cs(l,nn,2) + cs(l,nn,3) + cs(l,nn,4)

        dcs_rothc(l,nn,1) = cs(l,nn,1)
        dcs_rothc(l,nn,2) = cs(l,nn,2)
        dcs_rothc(l,nn,3) = cs(l,nn,3)
        dcs_rothc(l,nn,4) = cs(l,nn,4)

        ! Get total respiration.
        resp_s_dr_out(l,nn,5) = resp_s_dr_out(l,nn,1) + resp_s_dr_out(l,nn,2) &
                              + resp_s_dr_out(l,nn,3) + resp_s_dr_out(l,nn,4)
      END DO
    END DO
  END IF  !  soil_model_rothc

  !-----------------------------------------------------------------------
  ! Select timestep and forward timestep weighting parameters for
  ! equilibrium or dynamic vegetation and call TRIFFID.
  !-----------------------------------------------------------------------
  IF (l_trif_eq) THEN
    forw    = 1.0
    r_gamma = gamma_eq
    kiter   = iter_eq
  ELSE
    forw    = 0.0
    r_gamma = gam_trif
    kiter   = 1
  END IF

  DO k = 1,kiter

    CALL triffid (land_pts, trif_pts, trif_index, forw, r_gamma,              &
                  frac_agric, frac_past, resp_frac,                           &
                  g_leaf_dr, npp_dr, resp_s_dr,                               &
                  resp_w_dr, cs, c_veg, frac, ht, lai,                        &
                  cv_trif, lit_c, lit_c_mn,                                   &
                  cnsrv_veg2_correction, cnsrv_soil_resp, burnt_soil,         &
                  nstep_trif)

  END DO

  !---------------------------------------------------------------------------
  ! Check carbon conservation (2/3)
  ! Add the soil respiration and wood product decay fluxes to the net
  ! carbon flux.
  !----------------------------------------------------------------------
  
  IF ( soil_bgc_model == soil_model_rothc ) THEN
    DO t = 1,trif_pts
      l = trif_index(t)
      cnsrv_carbon_flux(l) = cnsrv_carbon_flux(l) - cnsrv_veg2_correction(l)  &
                             - cnsrv_soil_resp(l) / gam_trif
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Update surft_index for new surface type fractions.
  !---------------------------------------------------------------------------
  CALL tilepts(land_pts, frac, surft_pts, surft_index)

  !---------------------------------------------------------------------------
  ! Reset the accumulation fluxes to zero in equilibrium mode.
  !---------------------------------------------------------------------------
  IF (l_trif_eq) THEN

    IF ( soil_bgc_model == soil_model_rothc ) THEN
      DO n = 1,4
        DO l = 1,land_pts
          DO nn = 1,dim_cslayer
            resp_s_ac(l,nn,n) = 0.0
          END DO
        END DO
      END DO
    END IF

    DO n = 1,nnpft
      DO l = 1,land_pts
        npp_ac(l,n)    = 0.0
        resp_w_ac(l,n) = 0.0
      END DO
    END DO

    !-------------------------------------------------------------------------
    ! Reset the accumulation fluxes to the TRIFFID-diagnosed corrections
    ! in dynamic mode. Such corrections will be typically associated with
    ! the total depletion of a carbon reservoir or with the maintenance
    ! of the plant seed fraction. Any residual for the 4 component pools is
    ! assumed to be in proportion to the pool size itself.
    !
    ! (In the case of zero total soil carbon, use 1e-10 to prevent zero
    !  divide. This will have no impact because it is multiplied by the
    !  constituent stores which are also zero...)
    !-----------------------------------------------------------------------
  ELSE

    ! .NOT. l_trif_eq
    IF ( soil_bgc_model == soil_model_rothc ) THEN

      mix_term(:,:,:) = 0.0
      lit_frac(:)   = 1.0

#if !defined(UM_JULES)
      IF ( l_layeredC ) THEN
        ! Calculate vertical profile of litter inputs.
        lit_frac(1) = dzsoil(1) *  EXP( -tau_lit * 0.5 * dzsoil(1) )          &
                      / litc_norm
        DO nn = 2,dim_cslayer
          lit_frac(nn) = dzsoil(nn) * EXP( -tau_lit *                         &
                         ( SUM(dzsoil(1:nn-1)) + 0.5 * dzsoil(nn) ) )         &
                         / litc_norm
        END DO
        !Calculate the mixing term
        CALL soilcarb_mix(land_pts, trif_pts, trif_index, dcs_rothc(:,:,1:4), &
                          mix_term, mix_s)
      END IF
#endif

      DO t = 1, trif_pts
        l = trif_index(t)
        DO nn = 1,dim_cslayer
          cs_tot(l,nn)      = MAX(1.0e-10,                                    &
                                  cs(l,nn,1) + cs(l,nn,2) + cs(l,nn,3)        &
                                  + cs(l,nn,4))
          denom_resp        = 1.0 / (cs_tot(l,nn) * r_gamma)
          dcs(l,nn)         = cs_tot(l,nn) - dcs(l,nn)
          resp_s_dr(l,nn,1) = (1.0 - resp_frac(l,nn)) * resp_s_dr(l,nn,5)
          lit_resp          = lit_c_mn(l) * lit_frac(nn)                      &
                              - (r_gamma * dcs(l,nn)) - resp_s_dr(l,nn,1)     &
                              - burnt_soil(l) + SUM(mix_term(l,nn,:))
          resp_s_ac(l,nn,1) = lit_resp * cs(l,nn,1) * denom_resp
          resp_s_ac(l,nn,2) = lit_resp * cs(l,nn,2) * denom_resp
          resp_s_ac(l,nn,3) = lit_resp * cs(l,nn,3) * denom_resp
          resp_s_ac(l,nn,4) = lit_resp * cs(l,nn,4) * denom_resp
        END DO
      END DO  !  trif_pts
    END IF  !  soil_model_rothc

    DO n = 1,nnpft
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        ratio          = frac_old(l,n) / frac(l,n)
        npp_ac(l,n)    = ratio * (npp_dr(l,n) - npp_dr_out(l,n) +             &
                                  exudates_pft(l,n)) / gam_trif
        resp_w_ac(l,n) = ratio * (resp_w_dr(l,n) - resp_w_dr_out(l,n))        &
                         / gam_trif
      END DO
    END DO

  END IF  !  l_trif_eq

  !---------------------------------------------------------------------------
  ! Reset the accumulated leaf turnover rates to zero.
  !---------------------------------------------------------------------------
  IF (l_phenol) THEN
    DO n = 1,nnpft
      DO l = 1,land_pts
        g_leaf_phen_ac(l,n) = 0.0
      END DO
    END DO
  ELSE
    DO n = 1,nnpft
      DO l = 1,land_pts
        g_leaf_ac(l,n) = 0.0
      END DO
    END DO
  END IF

  asteps_since_triffid = 0

END IF  !  TRIFFID step

!-----------------------------------------------------------------------
! Calculate gridbox mean vegetation parameters from fractions of
! surface functional types
!-----------------------------------------------------------------------
CALL sparm (land_pts, nsurft, surft_pts, surft_index,                         &
            frac, ht, lai, z0m_soil,                                          &
            catch_s, catch_t, z0_t, z0h_t)

!Does need to be called as frac will be updated by triffid
CALL infiltration_rate(land_pts, nsurft, surft_pts, surft_index,              &
                       satcon_soilt_sfc, frac, infil_t)

DO t = 1, trif_pts
  l = trif_index(t)

  cv(l) = 0.0

  DO n = 1, nnpft
    cv(l) = cv(l) + frac(l,n) * c_veg(l,n)
  END DO

  IF ( l_crop ) THEN
    DO n = 1, ncpft
      c_veg(l, n + nnpft) = rootc_cpft(l,n) + harvc_cpft(l,n) +               &
                            reservec_cpft(l,n) + stemc_diag_cpft(l,n) +       &
                            leafc_diag_cpft(l,n)
      cv(l) = cv(l) + frac(l, n + nnpft) * c_veg(l, n + nnpft)
    END DO
  END IF
END DO

IF ( ( l_triffid .AND. asteps_since_triffid == 0 ) .AND.                      &
     ( soil_bgc_model == soil_model_rothc ) ) THEN
  !----------------------------------------------------------------------
  ! Check carbon conservation (3/3)
  ! Calculate the error in carbon conservation as:
  ! error=(final store)-(initial store)-(net flux)
  !      ="extra carbon in store not accounted for by fluxes"
  !----------------------------------------------------------------------
  DO t = 1,trif_pts
    l = trif_index(t)
    cnsrv_carbon_veg2_gb(l) = SUM(frac(l,1:npft) * c_veg(l,1:npft))           &
                              + SUM(cs(l,:,:))                                &
                              - cnsrv_carbon_veg2_gb(l)                       &
                              - cnsrv_carbon_flux(l)                          &
                              - SUM(npp_ac(l,1:npft) * frac(l,1:npft))        &
                              + exudates_gb(l) / gam_trif                     &
                              - SUM(resp_s_ac(l,:,1:4))
  END DO
END IF  !  TRIFFID step and RothC

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE veg2
END MODULE veg2_mod
