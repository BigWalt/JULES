MODULE surf_couple_extra_mod
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
IMPLICIT NONE

PRIVATE

PUBLIC :: surf_couple_extra

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_COUPLE_EXTRA_MOD'

CONTAINS

!===============================================================================
! Public subroutine
!===============================================================================
SUBROUTINE surf_couple_extra(                                                 &
#if !defined(UM_JULES)
   ! Arguments used by JULES-standalone
   !Driving data and associated INTENT(IN)
   ls_rain, con_rain, ls_snow, con_snow, tl_1, lw_down, qw_1, u_1, v_1, pstar,&
   !Fluxes INTENT(IN)
   ei_surft, surf_htf_surft, ecan_surft, ext_soilt, sw_surft,                 &
   !Misc INTENT(IN)
   a_step, smlt, tile_frac, hcons_soilt,                                      &
   !Fluxes INTENT(INOUT)
   melt_surft,                                                                &
   !Fluxes INTENT(OUT)
   snomlt_surf_htf, snowmelt, snomlt_sub_htf, sub_surf_roff, surf_roff,       &
   tot_tfall, snow_melt, rrun, rflow, snow_soil_htf                           &
#else
   !Arguments for the UM-----------------------------------------
   !IN
   land_pts, row_length, rows, river_row_length, river_rows, land_index,      &
   ls_rain, con_rain, ls_snow, ls_graup, con_snow, surf_ht_flux_land,         &
   cca_2d, smlt, nsurft, surft_pts, surft_index, tile_frac,                   &
   ei_surft, surf_htf_surft,                                                  &
   tstar_surft, hcons_soilt,                                                  &
   lice_pts, lice_index, soil_pts, soil_index, ext_soilt,                     &
   stf_sub_surf_roff,                                                         &
   ecan_surft, fexp_soilt, gamtot_soilt, ti_mean_soilt, ti_sig_soilt,         &
   cs_ch4_soilt, pstar_um_ij,                                                 &
   a_fsat_soilt, c_fsat_soilt, a_fwet_soilt, c_fwet_soilt,                    &
   ntype, fqw_surft,                                                          &
   halo_i, halo_j, model_levels,                                              &
   delta_lambda, delta_phi, xx_cos_theta_latitude, i_river_vn,                &
   aocpl_row_length, aocpl_p_rows, xpa, xua, xva, ypa, yua, yva,              &
   g_p_field, g_r_field, n_proc, global_row_length, global_rows,              &
   global_river_row_length, global_river_rows, flandg, river_vel, river_mcoef,&
   trivdir, trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land,       &
   substore, surfstore, flowin, bflowin, smvcst, smvcwt,                      &
   a_step, n_rows, offx, offy, n_procx, n_procy, g_rows, g_row_length,        &
   at_extremity, frac_disturb, satcon, soil_clay_ij, resp_s_gb_um, npp_gb,    &
   z0m_soil_gb,                                                               &
   !INOUT
   a_steps_since_riv, melt_surft, t_soil_soilt, tsurf_elev_surft,             &
   rgrain_surft, snow_grnd_surft, snow_surft,                                 &
   smcl_soilt, sthf_soilt, sthu_soilt, canopy_surft, fsat_soilt, fwetl_soilt, &
   zw_soilt, sthzw_soilt,                                                     &
   snow_depth, snowmelt, ls_rainfrac_land,                                    &
   tot_surf_runoff, tot_sub_runoff, acc_lake_evap, twatstor,                  &
   asteps_since_triffid, g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft,    &
   resp_s_acc_gb_um,                                                          &
   resp_w_acc_pft, cs_pool_gb_um, frac_surft, lai_pft, canht_pft,             &
   catch_snow_surft, catch_surft, infil_surft,                                &
   !OUT
   inlandout_atm_gb, surf_ht_flux_ld, snow_melt,                              &
   snomlt_sub_htf, dhf_surf_minus_soil,                                       &
   canopy_gb, snomlt_surf_htf, smc_soilt, sub_surf_roff,                      &
   surf_roff,                                                                 &
   tot_tfall, z0_surft, z0h_bare_surft, snow_soil_htf,                        &
   land_sea_mask                                                              &
#endif
   )

!Module imports

!Common modules
USE ereport_mod,              ONLY: ereport

! for testing LSM switch
USE jules_print_mgr,          ONLY: jules_message, jules_print

!Import interfaces to subroutines called
USE hydrol_mod,               ONLY: hydrol
USE snow_mod,                 ONLY: snow
USE river_control_mod,        ONLY: river_control

!Variables in modules that don't change names between UM and JULES
USE atm_fields_bounds_mod,    ONLY:                                           &
  tdims, tdims_s, pdims, pdims_s
USE jules_soil_mod,           ONLY:                                           &
  sm_levels, l_soil_sat_down, confrac
USE theta_field_sizes,        ONLY:                                           &
  t_i_length, t_j_length
USE jules_vegetation_mod,     ONLY:                                           &
  l_crop, l_triffid, l_trif_eq, l_phenol, phenol_period, triffid_period,      &
  l_irrig_dmd, irr_crop, l_irrig_limit, l_inferno, ignition_method,           &
  l_fao_ref_evapotranspiration, frac_min
USE trif_vars_mod, ONLY:                                                      &
  cnsrv_carbon_veg2_gb, cnsrv_veg_triffid_gb, cnsrv_soil_triffid_gb,          &
  cnsrv_prod_triffid_gb, cnsrv_carbon_triffid_gb, fao_et0,                    &
  cnsrv_vegN_triffid_gb, cnsrv_soilN_triffid_gb,                              &
  cnsrv_N_inorg_triffid_gb, cnsrv_nitrogen_triffid_gb, frac_past_gb,          &
  deposition_n_gb, n_leach_gb_acc

USE jules_hydrology_mod,      ONLY:                                           &
  l_hydrology, l_pdm, l_top, l_var_rainfrac
USE jules_surface_types_mod,  ONLY:                                           &
  npft, ncpft, nnpft, soil
USE sf_diags_mod,             ONLY: sf_diag
USE p_s_parms,                ONLY:                                           &
  bexp_soilt, sathh_soilt, hcap_soilt, hcon_soilt, satcon_soilt,              &
  smvccl_soilt, smvcwt_soilt, smvcst_soilt
USE ancil_info,               ONLY:                                           &
  dim_cslayer, nsoilt
USE jules_soil_biogeochem_mod, ONLY:                                          &
  soil_model_1pool, soil_model_ecosse, soil_model_rothc, soil_bgc_model
USE inferno_io_mod,           ONLY: inferno_io
USE ancil_info,               ONLY: dim_cslayer

!Variables required only in UM-mode
#if defined(UM_JULES)
USE veg_control_mod,          ONLY:                                         &
   veg_control
USE diagnostics_riv_mod,      ONLY:                                         &
   diagnostics_riv
USE prognostics,              ONLY:                                         &
   nsnow_surft, snowdepth_surft, rgrainl_surft, rho_snow_surft,             &
   sice_surft, sliq_surft, tsnow_surft, rho_snow_grnd_surft, ds_surft,      &
   wood_prod_fast_gb, wood_prod_med_gb, wood_prod_slow_gb

USE atm_fields_real_mod,      ONLY: disturb_veg_prev

USE timestep_mod,             ONLY:                                         &
   timestep
USE jules_radiation_mod,      ONLY:                                         &
   l_snow_albedo
USE um_parcore,               ONLY:                                         &
   mype
USE atm_step_local,           ONLY:                                         &
   land_pts_trif, npft_trif, STASHwork19, STASHwork8, STASHwork26

!Variables in modules that change name between JULES and UM
USE atm_step_local,           ONLY:                                         &
   dim_cs1, dim_cs2
USE river_inputs_mod,         ONLY:                                         &
   l_inland, l_rivers

!Error reporting
USE umPrintMgr
USE ereport_mod,              ONLY:                                         &
   ereport

!For detecting the SCM, rather than using an ifdef
USE model_domain_mod,         ONLY:                                         &
   model_type, mt_single_column

USE stash_array_mod,          ONLY: sf
! JULES-standalone only
#else

!Subroutines only required by JULES-standalone
USE crop_mod,                 ONLY: crop
USE veg1_mod,                 ONLY: veg1
USE veg2_mod,                 ONLY: veg2
USE fire_timestep_mod,        ONLY: fire_timestep
USE metstats_timestep_mod,    ONLY: metstats_timestep
USE gridbox_mean_mod,         ONLY: soiltiles_to_gbm
USE soil_biogeochem_control_mod, ONLY: soil_biogeochem_control
USE veg_soil_index_mod,       ONLY: get_veg_soil_index

!Modules that change name between JULES and UM
USE jules_rivers_mod,         ONLY:                                         &
  l_rivers
USE ancil_info,               ONLY:                                         &
  lice_pts, lice_index, soil_pts, soil_index,                               &
  dim_cs1, frac_surft, land_pts, nsurft, land_index, surft_pts, surft_index,&
  row_length, rows
USE switches,                 ONLY:                                         &
  l_inland

!JULES-standalone only
USE diag_swchs,               ONLY:                                         &
  stf_sub_surf_roff,                                                        &
  srflow, srrun
USE jules_surface_types_mod,  ONLY:                                         &
  ntype
USE trifctl,                  ONLY:                                         &
  asteps_since_triffid, g_leaf_acc_pft, npp_acc_pft, g_leaf_phen_acc_pft,   &
  resp_s_acc_soilt, resp_w_acc_pft, g_leaf_dr_out_pft, npp_dr_out_pft,      &
  resp_w_dr_out_pft, resp_s_dr_out_gb, c_veg_pft, cv_gb, lit_c_pft,         &
  lit_c_mn_gb, g_leaf_day_pft, g_leaf_phen_pft, lai_phen_pft, frac_agr_gb,  &
  resp_s_soilt, npp_gb

USE p_s_parms,                ONLY:                                         &
  catch_snow_surft, sthu_soilt, sthf_soilt, catch_surft, infil_surft,       &
  z0_surft, clay_soilt, z0h_bare_surft, z0m_soil_gb
USE model_time_mod,           ONLY:                                         &
  timestep_len
USE top_pdm,                  ONLY:                                         &
  inlandout_atm_gb, fexp_soilt, gamtot_soilt, ti_mean_soilt,                &
  ti_sig_soilt, dun_roff_soilt, drain_soilt, fsat_soilt, fwetl_soilt,       &
  qbase_soilt, qbase_zw_soilt, zw_soilt, sthzw_soilt, a_fsat_soilt,         &
  c_fsat_soilt, a_fwet_soilt, c_fwet_soilt, fch4_wetl_soilt,                &
  fch4_wetl_cs_soilt, fch4_wetl_npp_soilt, fch4_wetl_resps_soilt
USE prognostics,              ONLY:                                         &
  canopy_surft, canopy_gb, smc_soilt, tstar_surft, rgrain_surft,            &
  rgrainl_surft,                                                            &
  rho_snow_grnd_surft, sice_surft, sliq_surft, snow_grnd_surft, snow_surft, &
  tsnow_surft, ds_surft, snow_mass_ij, smcl_soilt, t_soil_soilt,            &
  snowdepth_surft, nsnow_surft, cs_pool_soilt, canht_pft, lai_pft,          &
  rho_snow_surft, tsurf_elev_surft
USE conversions_mod,             ONLY:                                      &
  isec_per_day, rsec_per_day

!For science that isn't in the UM at this point (or at all)
  !Crops
USE zenith_mod,               ONLY:                                         &
  photoperiod
USE crop_vars_mod,            ONLY:                                         &
  phot, dphotdt, dvi_cpft, rootc_cpft, harvc_cpft, reservec_cpft,           &
  croplai_cpft, cropcanht_cpft, dvimax_gb, frac_irr_soilt,                  &
  plant_n_gb, nday_crop, sthu_irr_soilt
USE trifctl,                  ONLY:                                         &
  npp_pft
USE p_s_parms,                ONLY:                                         &
  smvccl_soilt
USE fire_mod,                 ONLY:                                         &
  fire_prog, fire_diag, l_fire
USE metstats_mod,             ONLY:                                         &
  metstats_prog, metstats_input, l_metstats
USE model_time_mod,           ONLY:                                         &
  current_time
USE jules_rivers_trip_mod,    ONLY:                                         &
  adjust_routestore
USE fao_evapotranspiration,   ONLY:                                         &
  fao_ref_evapotranspiration
USE forcing,                  ONLY:                                         &
  pstar_ij, sw_down_ij, lw_down_ij
USE fluxes,                   ONLY:                                         &
  surf_ht_flux_ij, tstar_ij

USE gridbox_mean_mod,         ONLY:                                         &
  surftiles_to_gbm
USE lsm_switch_mod,           ONLY:                                         &
  lsm_id, jules, cable

#endif

!Dr Hook
USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook



IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Private subroutine for accessing the JULES science routines that are called
!   after the implicit code
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!Subroutine Arguments

!-----------------------------------------------------------------------------
! Variables required for UM_JULES definitions
#if defined(UM_JULES)
INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
  row_length,                                                                 &
  rows,                                                                       &
  river_row_length,                                                           &
  river_rows,                                                                 &
  land_index(land_pts),                                                       &
  nsurft
REAL :: clay_soilt(land_pts,nsoilt,dim_cslayer)
#endif

!-----------------------------------------------------------------------------
! Common variables to both UM_JULES and STANDALONE
!Driving data and associated INTENT(IN)
REAL, INTENT(INOUT) ::                                                        &
  ls_rain(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),               &
  con_rain(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
REAL, INTENT(IN) ::                                                           &
  ls_snow(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),               &
  con_snow(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)

!Fluxes INTENT(IN)
REAL, INTENT(IN) ::                                                           &
  ei_surft(land_pts,nsurft),                                                  &
       !Sublimation of snow (kg/m2/s)
  surf_htf_surft(land_pts,nsurft),                                            &
       !Surface heat flux (W/m2)
  ecan_surft(land_pts,nsurft),                                                &
       !Canopy evaporation from land tiles (kg/m2/s).
  ext_soilt(land_pts,nsoilt,sm_levels)
       !Extraction of water from each soil layer (kg/m2/s).

!Misc INTENT(IN)
INTEGER, INTENT(IN) ::                                                        &
  a_step
LOGICAL, INTENT(IN) ::                                                        &
  smlt
REAL, INTENT(IN)  ::                                                          &
  tile_frac(land_pts,nsurft)

!Fluxes INTENT(INOUT)
REAL, INTENT(INOUT) ::                                                        &
  melt_surft(land_pts,nsurft),                                                &
        !Surface or canopy snowmelt rate (kg/m2/s)
        !On output, this is the total melt rate for the tile
        !(i.e. sum of  melt on canopy and ground).
  snomlt_sub_htf(land_pts)
        !Sub-canopy snowmelt heat flux (W/m2)

REAL, INTENT(INOUT) ::                                                        &
   hcons_soilt(land_pts,nsoilt)

#if defined(UM_JULES)
REAL, INTENT(IN)    :: ls_graup(tdims%i_start:tdims%i_end,                    &
                                tdims%j_start:tdims%j_end)
        ! Surface graupel fall from UM
REAL, INTENT(INOUT) :: snowmelt(row_length,rows)
        ! Snowmelt initialised earlier for UM runs
REAL, INTENT(OUT)   :: dhf_surf_minus_soil(land_pts)
        ! Heat flux difference across the FLake snowpack (W/m2)
#else
REAL, INTENT(OUT) :: snowmelt(row_length,rows)
        ! Snowmelt purely output in stand-alone JULES
REAL :: dhf_surf_minus_soil(land_pts)
        ! Heat flux difference across the FLake snowpack (W/m2)
#endif


!Fluxes INTENT(OUT)
REAL, INTENT(OUT)::                                                           &
  sub_surf_roff(land_pts),                                                    &
        !Sub-surface runoff (kg/m2/s).
  surf_roff(land_pts),                                                        &
        !Surface runoff (kg/m2/s).
  tot_tfall(land_pts),                                                        &
        !Total throughfall (kg/m2/s).
  snomlt_surf_htf(row_length,rows),                                           &
        !Gridbox snowmelt heat flux (W/m2)
  snow_soil_htf(land_pts,nsurft),                                             &
        !Tiled snow->soil heat flux (W/m2)
  snow_melt(land_pts)

!Local constants
INTEGER ::                                                                    &
   i,j,l,n,m,                                                                 &
        !Various counters
   p_field
        !Number of model points

!Local variables
INTEGER ::                                                                    &
  phenol_call,                                                                &
        !indicates whether phenology is to be called
  triffid_call,                                                               &
        !indicates whether TRIFFID is to be called
  crop_call,                                                                  &
        !indicates whether crop model is to be called
  crop_period,                                                                &
        !crops have a daily calling period
  nstep_trif,                                                                 &
        !Number of atmospheric timesteps between calls to TRIFFID
        !vegetation model
  trif_pts
        ! Number of points on which TRIFFID may operate.

INTEGER ::                                                                    &
  trif_index(land_pts)
        ! Indices of land points on which TRIFFID may operate.

PARAMETER( crop_period = 1 )
        ! Crop code hard wired to run daily : crop_period = 1

REAL ::                                                                       &
  ls_rain_land(land_pts),                                                     &
  con_rain_land(land_pts),                                                    &
  ls_snow_land(land_pts),                                                     &
  ls_graup_land(land_pts),                                                    &
  con_snow_land(land_pts),                                                    &
  con_rainfrac_land(land_pts),                                                &
  inlandout_atmos(row_length,rows),                                           &
  lying_snow(land_pts),                                                       &
  frac_vs(land_pts),                                                          &
        ! Fraction of gridbox covered by veg or soil.
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                 &
        ! Base flow from each soil layer (kg m-2 s-1).
  w_flux_soilt(land_pts,nsoilt,0:sm_levels)
        ! Fluxes of water between layers (kg m-2 s-1).

!Soil carbon fuel; used by INFERNO
REAL ::                                                                       &
  c_soil_dpm_gb(land_pts),                                                    &
         ! Gridbox soil C in the Decomposable Plant Material pool (kg m-2).
  c_soil_rpm_gb(land_pts)
         ! Gridbox soil C in the Resistant Plant Material pool (kg m-2).

!-----------------------------------------------------------------------------
!JULES standalone-only arguments
#if !defined(UM_JULES)

REAL, INTENT(IN) ::                                                           &
   tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                 &
   lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),              &
   qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                 &
   u_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                  &
   v_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                  &
   pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                &
   sw_surft(land_pts,nsurft)
         !Surface net SW radiation on land tiles (W/m2)

!Fluxes INTENT(OUT)
REAL, INTENT(OUT)::                                                           &
  rrun(land_pts),                                                             &
         !Surface runoff after river routing (kg/m2/s)
  rflow(land_pts)
         !River runoff (kg/m2/s)

! Local variables - JULES standalone only
REAL ::                                                                       &
  timestep,                                                                   &
         ! Model timestep (s)
  frac_surft_start(land_pts,ntype),                                           &
         ! Fractions of surface types at the start of the timestep.
  surf_ht_flux_ld(land_pts),                                                  &
         ! Surface heat flux on land (W/m2)
  ls_rainfrac_land(land_pts),                                                 &
  cs_ch4_soilt(land_pts,nsoilt),                                              &
         ! soil carbon used in wetland CH4 emissions model if TRIFFID
         ! is switched off
  trad(land_pts),                                                             &
         ! gridbox effective radiative temperature (assuming emissivity=1)
  smc_gb(land_pts)
         !To allow GBM soil moisture to be passed down to fire
!-----------------------------------------------------------------------------
!UM-only arguments
#else

REAL, INTENT(IN) ::                                                           &
  surf_ht_flux_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),     &
  cca_2d(row_length,rows),                                                    &
  tstar_surft(land_pts,nsurft)

REAL ::                                                                       &
  cs_pool_soilt(land_pts,nsoilt,1,dim_cs1),                                   &
  resp_s_soilt(land_pts,nsoilt,1,dim_cs1)

INTEGER, INTENT(IN) ::                                                        &
  surft_pts(nsurft),                                                          &
  surft_index(land_pts,nsurft)

!Arguments added for hydrol
INTEGER, INTENT(IN) ::                                                        &
  lice_pts,                                                                   &
  lice_index(land_pts),                                                       &
  soil_pts,                                                                   &
  soil_index(land_pts)

REAL, INTENT(IN) ::                                                           &
  fexp_soilt(land_pts,nsoilt),                                                &
  gamtot_soilt(land_pts,nsoilt),                                              &
  ti_mean_soilt(land_pts,nsoilt),                                             &
  ti_sig_soilt(land_pts,nsoilt),                                              &
  resp_s_gb_um(land_pts,dim_cs1),                                             &
  npp_gb(land_pts),                                                           &
  a_fsat_soilt(land_pts,nsoilt),                                              &
  c_fsat_soilt(land_pts,nsoilt),                                              &
  a_fwet_soilt(land_pts,nsoilt),                                              &
  c_fwet_soilt(land_pts,nsoilt)

LOGICAL, INTENT(IN) ::                                                        &
  stf_sub_surf_roff

!Arguments added for rivers
INTEGER, INTENT(IN) ::                                                        &
  ntype,                                                                      &
  i_river_vn,                                                                 &
  aocpl_row_length,                                                           &
  aocpl_p_rows,                                                               &
  g_p_field,                                                                  &
  g_r_field,                                                                  &
  n_proc,                                                                     &
  global_row_length,                                                          &
  global_rows,                                                                &
  global_river_row_length,                                                    &
  global_river_rows,                                                          &
  halo_i,                                                                     &
  halo_j,                                                                     &
  model_levels

REAL, INTENT(IN) ::                                                           &
  fqw_surft(land_pts,nsurft),                                                 &
  delta_lambda,                                                               &
  delta_phi,                                                                  &
  xx_cos_theta_latitude(tdims_s%i_start:tdims_s%i_end,                        &
                        tdims_s%j_start:tdims_s%j_end),                       &
  xpa(aocpl_row_length+1),                                                    &
  xua(0:aocpl_row_length),                                                    &
  xva(aocpl_row_length+1),                                                    &
  ypa(aocpl_p_rows),                                                          &
  yua(aocpl_p_rows),                                                          &
  yva(0:aocpl_p_rows),                                                        &
  flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),        &
  river_vel,                                                                  &
  river_mcoef,                                                                &
  trivdir(river_row_length, river_rows),                                      &
  trivseq(river_row_length, river_rows),                                      &
  r_area(row_length, rows),                                                   &
  slope(row_length, rows),                                                    &
  flowobs1(row_length, rows),                                                 &
  r_inext(row_length, rows),                                                  &
  r_jnext(row_length, rows),                                                  &
  r_land(row_length, rows),                                                   &
  substore(row_length, rows),                                                 &
  surfstore(row_length, rows),                                                &
  flowin(row_length, rows),                                                   &
  bflowin(row_length, rows),                                                  &
  smvcst(land_pts),                                                           &
  smvcwt(land_pts)

INTEGER, INTENT(IN) ::                                                        &
  n_rows,                                                                     &
  offx,                                                                       &
  offy,                                                                       &
  n_procx,                                                                    &
  n_procy,                                                                    &
  g_rows (0:n_proc-1),                                                        &
  g_row_length (0:n_proc-1)

REAL, INTENT(IN) ::                                                           &
  frac_disturb(land_pts),                                                     &
  satcon(land_pts),                                                           &
  soil_clay_ij(row_length,rows),                                              &
  z0m_soil_gb(land_pts)

!variables needed for INFERNO in coupled mode
REAL, INTENT(IN) ::                                                           &
  pstar_um_ij(row_length,rows)

LOGICAL, INTENT(IN) ::                                                        &
  at_extremity(4)

INTEGER, INTENT(INOUT)  ::                                                    &
  a_steps_since_riv

REAL, INTENT(INOUT) ::                                                        &
  snow_depth(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),            &
  ls_rainfrac_land(land_pts),                                                 &
  catch_snow_surft(land_pts,nsurft),                                          &
  t_soil_soilt(land_pts,nsoilt,sm_levels),                                    &
  tsurf_elev_surft(land_pts,nsurft),                                          &
  rgrain_surft(land_pts,nsurft),                                              &
  snow_grnd_surft(land_pts,nsurft),                                           &
  snow_surft(land_pts,nsurft),                                                &
  smcl_soilt(land_pts,nsoilt,sm_levels),                                      &
  sthf_soilt(land_pts,nsoilt,sm_levels)

!Arguments added for hydrol
REAL, INTENT(INOUT) ::                                                        &
  infil_surft(land_pts,nsurft),                                               &
  catch_surft(land_pts,nsurft),                                               &
  sthu_soilt(land_pts,nsoilt,sm_levels),                                      &
  canopy_surft(land_pts,nsurft),                                              &
  fsat_soilt(land_pts,nsoilt),                                                &
  fwetl_soilt(land_pts,nsoilt),                                               &
  zw_soilt(land_pts,nsoilt),                                                  &
  sthzw_soilt(land_pts,nsoilt)

!Arguments added for rivers
REAL, INTENT(INOUT) ::                                                        &
  tot_surf_runoff(land_pts),                                                  &
  tot_sub_runoff(land_pts),                                                   &
  acc_lake_evap(row_length,rows),                                             &
  twatstor(river_row_length, river_rows)

!Arguments added for veg
INTEGER, INTENT(INOUT) ::                                                     &
  asteps_since_triffid

REAL, INTENT(INOUT) ::                                                        &
  g_leaf_acc_pft(land_pts,npft),                                              &
  g_leaf_phen_acc_pft(land_pts,npft),                                         &
  npp_acc_pft(land_pts_trif,npft_trif),                                       &
  resp_s_acc_gb_um(land_pts_trif,dim_cs1),                                    &
  resp_w_acc_pft(land_pts_trif,npft_trif),                                    &
  cs_pool_gb_um(land_pts,dim_cs1),                                            &
  frac_surft(land_pts,ntype),                                                 &
  lai_pft(land_pts,npft),                                                     &
  canht_pft(land_pts,npft)

REAL, INTENT(OUT) ::                                                          &
  inlandout_atm_gb(land_pts),                                                 &
  surf_ht_flux_ld(land_pts)

REAL, INTENT(OUT) ::                                                          &
  canopy_gb(land_pts),                                                        &
  smc_soilt(land_pts,nsoilt)

!Arguments added for veg
REAL, INTENT(OUT) ::                                                          &
  z0_surft(land_pts,nsurft),                                                  &
  z0h_bare_surft(land_pts,nsurft)

LOGICAL, INTENT(IN) ::                                                        &
  land_sea_mask(row_length, rows)

!Local variables
REAL ::                                                                       &
  riverout(row_length, rows),                                                 &
  box_outflow(river_row_length, river_rows),                                  &
  box_inflow(river_row_length, river_rows),                                   &
  inlandout_riv(river_row_length,river_rows),                                 &
  infil(land_pts),                                                            &
  dun_roff_soilt(land_pts,nsoilt),                                            &
  qbase_soilt(land_pts,nsoilt),                                               &
  qbase_zw_soilt(land_pts,nsoilt),                                            &
  drain_soilt(land_pts,nsoilt),                                               &
  cs_ch4_soilt(land_pts,nsoilt),                                              &
  fch4_wetl_soilt(land_pts,nsoilt),                                           &
  fch4_wetl_cs_soilt(land_pts,nsoilt),                                        &
  fch4_wetl_npp_soilt(land_pts,nsoilt),                                       &
  fch4_wetl_resps_soilt(land_pts,nsoilt)

!Local variables needed by INFERNO
REAL ::                                                                       &
  pstar(land_pts)

INTEGER ::                                                                    &
  nstep_trip,                                                                 &
  gather_pe_trip

LOGICAL ::                                                                    &
  first_routing,                                                              &
  invert_atmos,                                                               &
  trip_call

!Local variables to pass from science routines to STASH routines
REAL ::                                                                       &
  c_veg_pft(land_pts,npft),                                                   &
  cv_gb(land_pts),                                                            &
  lit_c_pft(land_pts,npft),                                                   &
  lit_c_mn_gb(land_pts),                                                      &
  g_leaf_day_pft(land_pts,npft),                                              &
  g_leaf_phen_pft(land_pts,npft),                                             &
  lai_phen_pft(land_pts,npft),                                                &
  g_leaf_dr_out_pft(land_pts,npft),                                           &
  npp_dr_out_pft(land_pts,npft),                                              &
  resp_w_dr_out_pft(land_pts,npft),                                           &
  resp_s_dr_out_gb_um(land_pts,dim_cs1+1)

! Need INVERT_OCEAN (hence N->S configuration of ATMOS grid)
! for river routing
LOGICAL, PARAMETER :: invert_ocean = .FALSE.

#endif

!-----------------------------------------------------------------------------

CHARACTER(LEN=256)            :: message
INTEGER                       :: errorstatus
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SURF_COUPLE_EXTRA'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!Only required until model switch implemented for coupled runs
#if defined(UM_JULES)
INTEGER, PARAMETER   :: jules = 1
INTEGER, PARAMETER   :: cable = 2
INTEGER              :: lsm_id = jules
#endif

!-----------------------------------------------------------------------------
!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

#if !defined(UM_JULES)
!Set up a some commonly used values
p_field  = t_i_length * t_j_length
timestep = REAL(timestep_len)

!initialise
trad(:) = 0.0
#endif
!CABLE_LSM: implement switching based on lsm_id
SELECT CASE( lsm_id )
CASE ( jules )
  ! -------------------------------------------------------------------
  ! Section HYD.1 Compress fields to land points, then call hydrology.
  ! ----------------------------------------------------------------------

  IF (l_hydrology .AND. land_pts /= 0 ) THEN
    ! Inland basin outflow is added to soil moisture at each timestep.
    ! This flux changes its value only when the river routing scheme
    ! has been called in the previous timestep

#if defined(UM_JULES)
    IF (l_rivers) THEN
      !Pass inland flow to soil moisture every timestep
      IF ( .NOT. l_inland) THEN
        inlandout_atmos = 0.0
        inlandout_riv   = 0.0
      END IF
    END IF  ! l_rivers
#endif

    !Initialise variables
    ls_rain_land(:)  = 0.0
    con_rain_land(:) = 0.0
    c_soil_dpm_gb(:) = 0.0
    c_soil_rpm_gb(:) = 0.0
#if defined(UM_JULES)
    pstar(:)    = 0.0
#endif

    !Compress fields to land points
    DO l = 1, land_pts
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      ls_rain_land(l)    = ls_rain(i,j)
      con_rain_land(l)   = con_rain(i,j)
      con_snow_land(l)   = con_snow(i,j)
      ls_snow_land(l)    = ls_snow(i,j)
      ls_graup_land(l)   = 0.0  ! initialise for standalone

#if defined(UM_JULES)
      surf_ht_flux_ld(l) = surf_ht_flux_land(i,j)
      lying_snow(l)      = snow_depth(i,j)
      snow_melt(l)       = snowmelt(i,j)
      ls_graup_land(l)   = ls_graup(i,j)
#endif

      !pass jules the modelled rain fractions
      IF (l_var_rainfrac) THEN

#if defined(UM_JULES)
        con_rainfrac_land(l) = MIN(cca_2d(i,j),0.5)  !As in diagnostics_conv
#else
        ! Not otherwise set in JULES stand-alone model
        con_rainfrac_land(l) = 0.0
        ls_rainfrac_land(l) = 0.0
#endif
        !provide some safety checking for convective rain with no CCA
        IF (con_rainfrac_land(l) == 0.0 .AND. con_rain_land(l) > 0.0) THEN
          con_rainfrac_land(l) = confrac
          !and for very small CCA amounts
        ELSE IF (con_rainfrac_land(l) < 0.01 .AND. con_rain_land(l) > 0.0) THEN
          con_rainfrac_land(l) = 0.01
        END IF

        !provide some safety checking for ls rain with no rainfrac
        IF (ls_rainfrac_land(l) == 0.0 .AND. ls_rain_land(l) > 0.0) THEN
          ls_rainfrac_land(l) = 0.5
          !and for very small rainfrac amounts
        ELSE IF (ls_rainfrac_land(l) < 0.01 .AND. ls_rain_land(l) > 0.0) THEN
          ls_rainfrac_land(l) = 0.01
        END IF

      ELSE
        !use original default values
        con_rainfrac_land(l) = confrac
        ls_rainfrac_land(l) = 1.0
      END IF
    END DO !land_pts

    !-------------------------------------------------------------------------------
    !   Snow processes
    !-------------------------------------------------------------------------------
    CALL snow ( land_pts,timestep,smlt,nsurft,surft_pts,                        &
                surft_index,catch_snow_surft,con_snow_land,con_rain_land,       &
                tile_frac,ls_snow_land,ls_graup_land,ls_rain_land,              &
                ei_surft,hcap_soilt(:,:,1),hcons_soilt,melt_surft,              &
                smcl_soilt(:,:,1),sthf_soilt(:,:,1),surf_htf_surft,             &
                t_soil_soilt(:,:,1),tsurf_elev_surft,                           &
                tstar_surft,smvcst_soilt(:,:,1),rgrain_surft,rgrainl_surft,     &
                rho_snow_grnd_surft,                                            &
                sice_surft,sliq_surft,snow_grnd_surft,snow_surft,               &
                snowdepth_surft, tsnow_surft,nsnow_surft,ds_surft,              &
                snomlt_surf_htf,lying_snow,rho_snow_surft,snomlt_sub_htf,       &
                snow_melt,snow_soil_htf,surf_ht_flux_ld,sf_diag,                &
                dhf_surf_minus_soil )

    !-------------------------------------------------------------------------------
    !   Land hydrology.
    !-------------------------------------------------------------------------------

    ! Calculate soil carbon for use in the wetland CH4 scheme only
    ! (only used if single-pool C model is used):
#if !defined(UM_JULES)
    IF ( soil_bgc_model == soil_model_1pool ) THEN
      DO m = 1,nsoilt
        DO j = 1,soil_pts
          i = soil_index(j)
          cs_ch4_soilt(i,m) = 0.0
          DO n = 1,dim_cslayer
            cs_ch4_soilt(i,m) = cs_ch4_soilt(i,m) + cs_pool_soilt(i,m,n,1)
          END DO
        END DO
      END DO
    END IF
#endif

#if defined(UM_JULES)
    DO l = 1,land_pts
      DO n = 1,dim_cs1
        resp_s_soilt(l,1,1,n) = resp_s_gb_um(l,n)
        cs_pool_soilt(l,1,1,n) = cs_pool_gb_um(l,n)
      END DO
    END DO
#endif
    !
    CALL hydrol (                                                               &
      lice_pts,lice_index,soil_pts,soil_index, nsnow_surft,                     &
      land_pts,sm_levels,bexp_soilt,catch_surft,con_rain_land,                  &
      ecan_surft,ext_soilt,hcap_soilt,hcon_soilt,ls_rain_land,                  &
      con_rainfrac_land, ls_rainfrac_land,                                      &
      satcon_soilt,sathh_soilt,snowdepth_surft, snow_soil_htf,                  &
      surf_ht_flux_ld,timestep,                                                 &
      smvcst_soilt,smvcwt_soilt,canopy_surft,                                   &
      stf_sub_surf_roff,smcl_soilt,sthf_soilt,sthu_soilt,                       &
      t_soil_soilt,tsurf_elev_surft,canopy_gb,smc_soilt,snow_melt,              &
      sub_surf_roff,surf_roff,tot_tfall,                                        &
      ! add new inland basin variable
      inlandout_atm_gb,l_inland,                                                &
      ! Additional variables for MOSES II
      nsurft,surft_pts,surft_index,                                             &
      infil_surft, melt_surft,tile_frac,                                        &
      ! Additional variables required for large-scale hydrology:
      l_top,l_pdm,fexp_soilt,ti_mean_soilt,cs_ch4_soilt,cs_pool_soilt,          &
      dun_roff_soilt,drain_soilt,fsat_soilt,fwetl_soilt,qbase_soilt,            &
      qbase_l_soilt, qbase_zw_soilt, w_flux_soilt,                              &
      zw_soilt,sthzw_soilt,a_fsat_soilt,c_fsat_soilt,a_fwet_soilt,              &
      c_fwet_soilt,                                                             &
      resp_s_soilt,npp_gb,fch4_wetl_soilt,                                      &
      fch4_wetl_cs_soilt,fch4_wetl_npp_soilt,fch4_wetl_resps_soilt,             &
      dim_cs1,l_soil_sat_down,l_triffid,asteps_since_triffid)

    !-------------------------------------------------------------------------------
    !   Reset snowmelt over land points.
    !-------------------------------------------------------------------------------
    !Copy land points output back to full fields array.
    DO l = 1, land_pts
      j=(land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
#if defined(UM_JULES)
      snow_depth(i,j) = lying_snow(l)
#else
      snow_mass_ij(i,j) = lying_snow(l)
#endif
      snowmelt(i,j) = snow_melt(l)
    END DO

  END IF ! ( l_hydrology .AND. land_pts /= 0 )

  !-------------------------------------------------------------------
  ! RIVER ROUTING
  !-------------------------------------------------------------------
  IF ( l_rivers ) THEN
#if defined(UM_JULES)
    CALL river_control(                                                         &
      !LOGICAL, INTENT(IN)
      invert_ocean,                                                             &
      !INTEGER, INTENT(IN)
      n_proc, land_pts, row_length, rows, river_row_length, river_rows,         &
      land_index, ntype, i_river_vn, aocpl_row_length, aocpl_p_rows, g_p_field, &
      g_r_field, mype, global_row_length, global_rows, global_river_row_length, &
      global_river_rows, halo_i, halo_j, model_levels, nsurft,                  &
      !REAL, INTENT(IN)
      fqw_surft, delta_lambda, delta_phi, xx_cos_theta_latitude,                &
      xpa, xua, xva, ypa, yua, yva, flandg, river_vel, river_mcoef, trivdir,    &
      trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land, substore,     &
      surfstore, flowin, bflowin, smvcst_soilt, smvcwt_soilt,                   &
      surf_roff, sub_surf_roff, frac_surft,                                     &
      !INTEGER, INTENT(INOUT)
      a_steps_since_riv,                                                        &
      !REAL, INTENT(INOUT)
      tot_surf_runoff, tot_sub_runoff, acc_lake_evap, twatstor,                 &
      smcl_soilt, sthu_soilt,                                                   &
      !LOGICAL, INTENT(OUT)
      trip_call,                                                                &
      !REAL, INTENT(OUT)
      inlandout_atm_gb, inlandout_atmos, inlandout_riv, riverout,               &
      box_outflow, box_inflow                                                   &
      )
#else
    CALL river_control( land_pts,sub_surf_roff                                  &
                               ,surf_roff,srflow,srrun,rflow,rrun)
#endif
  END IF ! l_rivers (ATMOS)

#if !defined(UM_JULES)
  !-------------------------------------------------------------------------------
  !   Calculate irrigation demand and planting dates if required
  !-------------------------------------------------------------------------------
  IF ( l_irrig_dmd ) THEN

    ! calculate maximum dvi per grid cell
    IF ( irr_crop == 2 ) THEN
      DO l = 1,land_pts
        dvimax_gb(l) = MAXVAL(dvi_cpft(l,:))
      END DO
    END IF

    CALL irrig_dmd(land_pts, sm_levels, frac_irr_soilt,                         &
                   a_step, plant_n_gb,                                          &
                   sthf_soilt, smvccl_soilt, smvcst_soilt, smvcwt_soilt,        &
                   sthzw_soilt, sthu_irr_soilt, sthu_soilt,                     &
                   smcl_soilt, irr_crop, dvimax_gb)

    IF ( irr_crop == 1 ) THEN
      CALL calc_crop_date(land_index, land_pts, t_i_length, t_j_length, nsurft, &
                          frac_surft, sw_surft, tstar_surft, lw_down, tl_1,     &
                          con_rain, ls_rain, con_snow, ls_snow,                 &
                          plant_n_gb, nday_crop)
    END IF

    IF ( l_irrig_limit ) THEN
      CALL adjust_routestore()
    END IF
  ELSE
    ! if .not. l_irrig_dmd, set sthu_irr_soilt to 0.0 in case it is still reported
    sthu_irr_soilt(:,:,:) = 0.0
  END IF ! l_irrig_dmd


  !-------------------------------------------------------------------------------
  ! Run crop code if required
  !-------------------------------------------------------------------------------
  IF ( l_crop ) THEN
    crop_call = MOD ( REAL(a_step),                                             &
                      REAL(crop_period) * rsec_per_day / timestep )

    DO n = 1,ncpft
      DO l = 1,land_pts
        npp_acc_pft(l,nnpft + n) = npp_acc_pft(l,nnpft + n)                     &
                              + (npp_pft(l,nnpft + n) * timestep)
      END DO
    END DO

    CALL photoperiod(p_field, phot, dphotdt)

    CALL crop(p_field, land_pts, land_index, a_step,                            &
              crop_call, sm_levels, frac_surft, phot, dphotdt,                  &
              sf_diag%t1p5m_surft, t_soil_soilt, sthu_soilt, smvccl_soilt,      &
              smvcst_soilt, npp_acc_pft,                                        &
              canht_pft, lai_pft, dvi_cpft, rootc_cpft, harvc_cpft,             &
              reservec_cpft, croplai_cpft, cropcanht_cpft,                      &
              catch_surft, z0_surft)
  END IF  ! l_crop

  !------------------------------------------------------------------------------
  !   Update metstats for this timestep
  !-------------------------------------------------------------------------------
  IF ( l_metstats ) THEN
    !Compress variables to land points as metstats has no knowledge of
    !i and j. Also use a TYPE to keep the argument list short
    DO l = 1, land_pts
      j = ( land_index(l) - 1 ) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      metstats_input(l)%temp     = tl_1(i,j)
      metstats_input(l)%spec_hum = qw_1(i,j)
      metstats_input(l)%wind_u   = u_1(i,j)
      metstats_input(l)%wind_v   = v_1(i,j)
      metstats_input(l)%ls_rain  = ls_rain(i,j)
      metstats_input(l)%con_rain = con_rain(i,j)
      metstats_input(l)%ls_snow  = ls_snow(i,j)
      metstats_input(l)%con_snow = con_snow(i,j)
      metstats_input(l)%press    = pstar(i,j)
    END DO

    CALL metstats_timestep(metstats_input,metstats_prog,                        &
         !Things that really ought to come in via USE but won't work with the UM
                           current_time%time, timestep,land_pts)
  END IF

  !------------------------------------------------------------------------------
  !   Call to fire module
  !------------------------------------------------------------------------------
  IF ( l_fire ) THEN

    !Calculate the gridbox mean soil moisture
    smc_gb = soiltiles_to_gbm(smc_soilt)
    CALL fire_timestep(metstats_prog, smc_gb, fire_prog, fire_diag,             &
         !Things that really ought to come in via USE but won't work with the UM
                       current_time%time, current_time%month, timestep, land_pts)
  END IF
#endif

  !--------------------------------------------------------------------------------
  !   Call to INFERNO (interactive fire module)
  !--------------------------------------------------------------------------------
  IF ( l_inferno ) THEN
    ! calculate the decomposable and resistant soil carbon pools
    ! these are used as a proxy for litter
    !
    ! Note that this code is currently incompatible with soil tiling, meaning we
    ! hard code the soilt index f cs_pool below, using m = 1
    ! See comments in INFERNO for more info

    m = 1

    ! Calculate gridbox total soil C in DPM and RPM pools.
    ! Note we assume that DPM and RPM are pools 1 and 2 respectively.
    ! In future a layered soil model could pass the near-surface soil C only.
    IF ( soil_bgc_model == soil_model_rothc ) THEN

      DO j = 1,soil_pts
        i = soil_index(j)
        c_soil_dpm_gb(i) = 0.0
        c_soil_rpm_gb(i) = 0.0

        DO n = 1,dim_cslayer
          c_soil_dpm_gb(i) = c_soil_dpm_gb(i) + cs_pool_soilt(i,m,n,1)
          c_soil_rpm_gb(i) = c_soil_rpm_gb(i) + cs_pool_soilt(i,m,n,2)
        END DO
      END DO

    ELSE IF ( soil_bgc_model == soil_model_1pool ) THEN
      ! With a single soil pool, we estimate the relative amounts of DPM and RPM.

      DO j = 1,soil_pts
        i = soil_index(j)
        c_soil_dpm_gb(i) = 0.0
        c_soil_rpm_gb(i) = 0.0

        DO n = 1,dim_cslayer
          c_soil_dpm_gb(i) = c_soil_dpm_gb(i) +                                  &
                             0.01 * cs_pool_soilt(i, m, n, 1)
          c_soil_rpm_gb(i) = c_soil_rpm_gb(i) +                                  &
                             0.2  * cs_pool_soilt(i, m, n, 1)
        END DO
      END DO

    END IF

#if defined(UM_JULES)
    ! compress fields to land points
    DO l = 1, land_pts
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      pstar(l) = pstar_um_ij(i,j)
    END DO
#endif

    ! Call INFERNO.
    CALL inferno_io( sf_diag%t1p5m_surft, sf_diag%q1p5m_surft, pstar,           &
                     sthu_soilt, sm_levels,                                     &
                     frac_surft, c_soil_dpm_gb, c_soil_rpm_gb, canht_pft,       &
                     ls_rain_land, con_rain_land,                               &
                     land_pts, ignition_method,                                 &
                     nsurft, asteps_since_triffid)

  END IF  !  l_inferno

    ! ----------------------------------------------------------------------
    ! Section 19 -- VEGETATION DYNAMICS
    ! ----------------------------------------------------------------------

    ! initialize carbon conservation diagnostics
    ! otherwise they can be non-zero on non-triffid timesteps
  IF ( l_triffid ) THEN
    DO l = 1, land_pts
      cnsrv_carbon_veg2_gb(l)      = 0.0
      cnsrv_carbon_triffid_gb(l)   = 0.0
      cnsrv_veg_triffid_gb(l)      = 0.0
      cnsrv_soil_triffid_gb(l)     = 0.0
      cnsrv_prod_triffid_gb(l)     = 0.0

      cnsrv_nitrogen_triffid_gb(l) = 0.0
      cnsrv_vegN_triffid_gb(l)     = 0.0
      cnsrv_soilN_triffid_gb(l)    = 0.0
      cnsrv_N_inorg_triffid_gb(l)  = 0.0

    END DO
  END IF

#if defined(UM_JULES)
  ! Change 2d to 1d soil clay content for soil respiration.
  ! Soil tiling not currently in the UM, so broadcast ij value to all tiles.
  ! Multi-layer clay not currently in UM so set all layers to same value.
  IF ( soil_bgc_model == soil_model_rothc ) THEN
    m = 1
    DO l = 1, land_pts
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      DO n = 1, dim_cslayer
        clay_soilt(l,m,n) = soil_clay_ij(i,j)
      END DO
    END DO
  END IF
#endif

    !-------------------------------------------------------------------------
    !   If leaf phenology and/or TRIFFID are activated, check whether these
    !   are to be called on this timestep.
    !-------------------------------------------------------------------------
#if defined(UM_JULES)
  IF (l_phenol .OR. l_triffid) THEN
    CALL veg_control(                                                           &
      land_pts, nsurft,                                                         &
      a_step, asteps_since_triffid,                                             &
      land_pts_trif, npft_trif,                                                 &
      phenol_period, triffid_period,                                            &
      l_phenol, l_triffid, l_trif_eq,                                           &
      timestep, frac_disturb, frac_past_gb, satcon_soilt(:,:,0),                &
      g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft,                         &
      resp_s_acc_gb_um, resp_w_acc_pft,                                         &
      cs_pool_gb_um, frac_surft, lai_pft, clay_soilt, z0m_soil_gb, canht_pft,   &
      catch_snow_surft, catch_surft, infil_surft, z0_surft, z0h_bare_surft,     &
      c_veg_pft, cv_gb, lit_c_pft, lit_c_mn_gb, g_leaf_day_pft, g_leaf_phen_pft,&
      lai_phen_pft, g_leaf_dr_out_pft, npp_dr_out_pft, resp_w_dr_out_pft,       &
      resp_s_dr_out_gb_um                                                       &
       )
  END IF
#else
  phenol_call  = 1
  triffid_call = 1
  IF ( l_phenol ) phenol_call = MOD ( a_step,                                   &
                                phenol_period * isec_per_day / timestep_len )

  IF ( l_triffid ) THEN
    nstep_trif = INT( rsec_per_day * REAL(triffid_period) / timestep )
    IF ( asteps_since_triffid == nstep_trif ) triffid_call = 0
    IF ( triffid_call == 0 .OR. soil_bgc_model == soil_model_ecosse ) THEN
      !-------------------------------------------------------------------------
      ! Find total fraction of gridbox covered by vegetation and soil, and use
      ! this to set indices of land points on which TRIFFID may operate.
      ! We also do this if ECOSSE is used so the veg and soil models operate on
      ! the same set of points.
      ! Note: This code is essentially a repeat of code in veg_control. That
      ! subroutine is expected to eventually also be used for standalone JULES,
      ! but in the meanwhile we also calculate trif_pts here.
      !-------------------------------------------------------------------------
      CALL get_veg_soil_index( land_pts, frac_surft, trif_pts,                  &
                               trif_index, frac_vs )
    END IF  !  triffid_call OR ecosse
  END IF  !  l_triffid

  ! Save frac at start of timestep (for ECOSSE).
  frac_surft_start(:,:) = frac_surft(:,:)

  IF ( triffid_call == 0 ) THEN
    !-------------------------------------------------------------------------------
    !     Run includes dynamic vegetation
    !
    ! Running with nsoilt > 1 is not compatible with dynamic vegetation, so we can
    ! hard-code the arguments appropriately by setting m = 1
    !
    !-------------------------------------------------------------------------------
    m = 1
    CALL veg2( land_pts, nsurft, a_step                                         &
              ,phenol_period, triffid_period                                    &
              ,trif_pts, trif_index, timestep                                   &
              ,frac_agr_gb, frac_past_gb, frac_vs                               &
              ,satcon_soilt(:,:,0), clay_soilt(:,m,:), z0m_soil_gb              &
              ,l_phenol, l_triffid, l_trif_eq                                   &
              ,asteps_since_triffid                                             &
              ,g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft                 &
              ,resp_s_acc_soilt(:,m,:,:), resp_w_acc_pft                        &
              ,cs_pool_soilt(:,m,:,:), frac_surft, lai_pft, canht_pft           &
              ,catch_snow_surft, catch_surft, infil_surft                       &
              ,z0_surft, z0h_bare_surft, c_veg_pft, cv_gb                       &
              ,g_leaf_day_pft, g_leaf_phen_pft, g_leaf_dr_out_pft               &
              ,lai_phen_pft, lit_c_pft, lit_c_mn_gb, npp_dr_out_pft             &
              ,resp_w_dr_out_pft, resp_s_dr_out_gb )

  ELSE

    IF ( phenol_call == 0 ) THEN
      !-------------------------------------------------------------------------
      ! Run includes phenology,  but not dynamic vegetation
      ! therefore call veg1 rather than veg2
      !-------------------------------------------------------------------------
      CALL veg1( land_pts, nsurft, a_step, phenol_period, timestep              &
                ,satcon_soilt(:,:,0), z0m_soil_gb, l_phenol                     &
                ,g_leaf_acc_pft, g_leaf_phen_acc_pft, frac_surft, lai_pft       &
                ,canht_pft, catch_snow_surft, catch_surft, infil_surft          &
                ,g_leaf_day_pft, g_leaf_phen_pft                                &
                ,lai_phen_pft, z0_surft, z0h_bare_surft )
    END IF

  END IF  !  triffid_call

  !-----------------------------------------------------------------------------
  ! Soil biogeochemistry.
  ! At present this only deals with the ECOSSE model of soil C and N.
  !-----------------------------------------------------------------------------
  IF ( soil_bgc_model == soil_model_ecosse ) THEN
    CALL soil_biogeochem_control( land_pts, triffid_call, trif_pts,             &
          trif_index, deposition_n_gb, frac_surft_start,                        &
          qbase_l_soilt, sthf_soilt, sthu_soilt, w_flux_soilt, t_soil_soilt )
  END IF

  !-----------------------------------------------------------------------------
  ! Calculate reference evapotranspiration.
  !-----------------------------------------------------------------------------
  IF (l_fao_ref_evapotranspiration) THEN
    trad = ( surftiles_to_gbm(tstar_surft**4) )**0.25
    CALL fao_ref_evapotranspiration(soil_pts, soil_index,                      &
      land_pts, land_index, sf_diag%t1p5m,                                     &
      sw_down_ij, lw_down_ij, surf_ht_flux_ij, sf_diag%u10m,                   &
      sf_diag%v10m, sf_diag%q1p5m, pstar_ij, trad, fao_et0)
  END IF

#endif

  !------------------------------------------------------------------------------
  !Call STASH diagnostic routines - UM-only
  !
  !For the UM, soil tiling has not been implemented, ie nsoilt = 1, so we can
  !hard-code _soilt variables with index 1 using m = 1
  !------------------------------------------------------------------------------
#if defined(UM_JULES)
  IF (model_type /= mt_single_column) THEN
    m = 1
    IF (l_hydrology .AND. sf(0,8) ) THEN
      ! DEPENDS ON: diagnostics_hyd
      CALL diagnostics_hyd(                                                     &
        row_length, rows, model_levels,                                         &
        n_rows, global_row_length, global_rows,                                 &
        halo_i, halo_j, offx, offy, mype,                                       &
        n_proc, n_procx, n_procy,                                               &
        g_rows, g_row_length,                                                   &
        at_extremity,                                                           &
        land_pts, sm_levels,                                                    &
        !Put inland basin outflow in call to diagriv
        land_index,inlandout_atm_gb,                                            &
        smc_soilt(:,m), surf_roff, sub_surf_roff,                               &
        lying_snow, snow_melt,                                                  &
        canopy_gb,t_soil_soilt(:,m,:),                                          &
        tsurf_elev_surft,snow_soil_htf,                                         &
        smcl_soilt(:,m,:),                                                      &
        nsurft, snomlt_surf_htf, sthu_soilt(:,m,:), sthf_soilt(:,m,:),          &
        tot_tfall, snow_surft, melt_surft,                                      &
        rgrain_surft, land_sea_mask,                                            &
        dun_roff_soilt(:,m), drain_soilt(:,m), qbase_soilt(:,m),                &
        qbase_zw_soilt(:,m), fch4_wetl_soilt(:,m),fch4_wetl_cs_soilt(:,m),      &
        fch4_wetl_npp_soilt(:,m),fch4_wetl_resps_soilt(:,m),                    &
        fexp_soilt(:,m),gamtot_soilt(:,m),ti_mean_soilt(:,m), ti_sig_soilt(:,m),&
        fsat_soilt(:,m),fwetl_soilt(:,m),zw_soilt(:,m),sthzw_soilt(:,m),        &
        timestep,                                                               &
        STASHwork8,                                                             &
        sf_diag                                                                 &
        )
    END IF

    IF ( l_rivers .AND. trip_call .AND. sf(0,26) ) THEN

      CALL diagnostics_riv(                                                     &
        row_length, rows,                                                       &
        river_row_length, river_rows,                                           &
        at_extremity,                                                           &
        at_extremity,                                                           &
        riverout,                                                               &
        box_outflow, box_inflow,                                                &
        !Put inland basin outflow in call to diagriv
        twatstor,inlandout_riv,                                                 &
        STASHwork26                                                             &
        )
    END IF

    IF (sf(0,19)) THEN
      ! DEPENDS ON: diagnostics_veg
      CALL diagnostics_veg(                                                     &
        row_length, rows, n_rows,                                               &
        global_row_length, global_rows,                                         &
        dim_cs1, dim_cs2,                                                       &
        halo_i, halo_j, offx, offy, mype,                                       &
        n_proc, n_procx, n_procy,                                               &
        g_rows, g_row_length,                                                   &
        at_extremity,                                                           &
        land_pts,                                                               &
        land_index,                                                             &
        ntype,npft,                                                             &
        c_veg_pft,cv_gb,g_leaf_phen_pft,                                        &
        lit_c_pft,lit_c_mn_gb,g_leaf_day_pft,                                   &
        lai_phen_pft,g_leaf_dr_out_pft,npp_dr_out_pft,                          &
        resp_w_dr_out_pft,resp_s_dr_out_gb_um,frac_disturb,disturb_veg_prev,    &
        wood_prod_fast_gb, wood_prod_med_gb, wood_prod_slow_gb,                 &
        frac_surft,lai_pft,canht_pft,cs_pool_gb_um,                             &
        STASHwork19                                                             &
        )
    END IF
  END IF
#endif

CASE ( cable )
#if defined(UM_JULES)
  errorstatus = 101
  CALL ereport('surf_couple_extra', errorstatus,                      &
               'CABLE not yet implemented')
#else
  ! for testing LSM switch
  WRITE(jules_message,'(A)') "CABLE not yet implemented"
  CALL jules_print('surf_couple_extra', jules_message)

  ! initialise all INTENT(OUT) for now until CABLE is implemented 
  melt_surft(:,:) = 0.0
  snomlt_surf_htf(:,:) = 0.0
  snowmelt(:,:) = 0.0 
  snomlt_sub_htf(:) = 0.0
  sub_surf_roff(:) = 0.0 
  surf_roff(:) = 0.0 
  tot_tfall(:) = 0.0
  snow_melt(:) = 0.0
  rrun(:) = 0.0
  rflow(:) = 0.0
  snow_soil_htf(:,:) = 0.0

#endif
CASE DEFAULT
  errorstatus = 101
  CALL ereport('surf_couple_extra', errorstatus,                                &
               'Unrecognised surface scheme')

END SELECT


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_extra
END MODULE surf_couple_extra_mod
