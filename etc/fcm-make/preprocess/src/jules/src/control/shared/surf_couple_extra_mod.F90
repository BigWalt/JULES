










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

REAL, INTENT(OUT) :: snowmelt(row_length,rows)
        ! Snowmelt purely output in stand-alone JULES
REAL :: dhf_surf_minus_soil(land_pts)
        ! Heat flux difference across the FLake snowpack (W/m2)


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

!-----------------------------------------------------------------------------

CHARACTER(LEN=256)            :: message
INTEGER                       :: errorstatus
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SURF_COUPLE_EXTRA'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!Only required until model switch implemented for coupled runs

!-----------------------------------------------------------------------------
!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Set up a some commonly used values
p_field  = t_i_length * t_j_length
timestep = REAL(timestep_len)

!initialise
trad(:) = 0.0
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


    !Initialise variables
    ls_rain_land(:)  = 0.0
    con_rain_land(:) = 0.0
    c_soil_dpm_gb(:) = 0.0
    c_soil_rpm_gb(:) = 0.0

    !Compress fields to land points
    DO l = 1, land_pts
      j = (land_index(l) - 1) / row_length + 1
      i = land_index(l) - (j-1) * row_length
      ls_rain_land(l)    = ls_rain(i,j)
      con_rain_land(l)   = con_rain(i,j)
      con_snow_land(l)   = con_snow(i,j)
      ls_snow_land(l)    = ls_snow(i,j)
      ls_graup_land(l)   = 0.0  ! initialise for standalone


      !pass jules the modelled rain fractions
      IF (l_var_rainfrac) THEN

        ! Not otherwise set in JULES stand-alone model
        con_rainfrac_land(l) = 0.0
        ls_rainfrac_land(l) = 0.0
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
      snow_mass_ij(i,j) = lying_snow(l)
      snowmelt(i,j) = snow_melt(l)
    END DO

  END IF ! ( l_hydrology .AND. land_pts /= 0 )

  !-------------------------------------------------------------------
  ! RIVER ROUTING
  !-------------------------------------------------------------------
  IF ( l_rivers ) THEN
    CALL river_control( land_pts,sub_surf_roff                                  &
                               ,surf_roff,srflow,srrun,rflow,rrun)
  END IF ! l_rivers (ATMOS)

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


    !-------------------------------------------------------------------------
    !   If leaf phenology and/or TRIFFID are activated, check whether these
    !   are to be called on this timestep.
    !-------------------------------------------------------------------------
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


  !------------------------------------------------------------------------------
  !Call STASH diagnostic routines - UM-only
  !
  !For the UM, soil tiling has not been implemented, ie nsoilt = 1, so we can
  !hard-code _soilt variables with index 1 using m = 1
  !------------------------------------------------------------------------------

CASE ( cable )
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

CASE DEFAULT
  errorstatus = 101
  CALL ereport('surf_couple_extra', errorstatus,                                &
               'Unrecognised surface scheme')

END SELECT


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_extra
END MODULE surf_couple_extra_mod
