#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_pftparm_jules(nml_dir)

  USE missing_data_mod, ONLY :                                                &
!  imported scalar parameters
     rmdi

  USE io_constants, ONLY : namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE jules_soil_biogeochem_mod, ONLY : l_layeredC, soil_bgc_model,           &
                                        soil_model_rothc

  USE jules_surface_types_mod, ONLY : npft, nnpft

  USE ancil_info, ONLY : land_pts

  USE pftparm

  USE c_z0h_z0m, ONLY : z0h_z0m, z0h_z0m_classic

  USE prognostics, ONLY : canht_pft, lai_pft

  USE pftparm_io

  USE jules_vegetation_mod, ONLY : can_rad_mod, l_crop, l_trait_phys,        &
                                   l_use_pft_psi, l_bvoc_emis, l_inferno,    &
                                   l_o3_damage

  USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs

  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the PFT parameters
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
  INTEGER :: error  ! Error indicator
  CHARACTER(LEN=errormessagelength) :: iomessage

  CHARACTER(LEN=*), PARAMETER :: routinename='INIT_PFTPARM_JULES'

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
  c3_io(:)     = -9
  orient_io(:) = -9
  fsmc_mod_io(:) = -9

  a_wl_io(:)                  = rmdi
  a_ws_io(:)                  = rmdi
  albsnc_max_io(:)            = rmdi
  albsnc_min_io(:)            = rmdi
  albsnf_maxu_io(:)           = rmdi
  albsnf_max_io(:)            = rmdi
  albsnf_maxl_io(:)           = rmdi
  alpha_io(:)                 = rmdi
  alniru_io(:)                = rmdi
  alnir_io(:)                 = rmdi
  alnirl_io(:)                = rmdi
  alparu_io(:)                = rmdi
  alpar_io(:)                 = rmdi
  alparl_io(:)                = rmdi
  b_wl_io(:)                  = rmdi
  catch0_io(:)                = rmdi
  dcatch_dlai_io(:)           = rmdi
  dgl_dm_io(:)                = rmdi
  dgl_dt_io(:)                = rmdi
  dqcrit_io(:)                = rmdi
  dz0v_dh_io(:)               = rmdi
  eta_sl_io(:)                = rmdi
  fd_io(:)                    = rmdi
  fsmc_of_io(:)               = rmdi
  f0_io(:)                    = rmdi
  g_leaf_0_io(:)              = rmdi
  glmin_io(:)                 = rmdi
  infil_f_io(:)               = rmdi
  kext_io(:)                  = rmdi
  kpar_io(:)                  = rmdi
  lai_alb_lim_io(:)           = rmdi
  neff_io(:)                  = rmdi
  nl0_io(:)                   = rmdi
  nr_nl_io(:)                 = rmdi
  ns_nl_io(:)                 = rmdi
  nsw_io(:)                   = rmdi
  nr_io(:)                    = rmdi
  hw_sw_io(:)                 = rmdi
  can_struct_a_io(:)          = rmdi
  omegau_io(:)                = rmdi
  omega_io(:)                 = rmdi
  omegal_io(:)                = rmdi
  omniru_io(:)                = rmdi
  omnir_io(:)                 = rmdi
  omnirl_io(:)                = rmdi
  r_grow_io(:)                = rmdi
  rootd_ft_io(:)              = rmdi
  psi_close_io(:)             = rmdi
  psi_open_io(:)              = rmdi
  fsmc_p0_io(:)               = rmdi
  sigl_io(:)                  = rmdi
  tleaf_of_io(:)              = rmdi
  tlow_io(:)                  = rmdi
  tupp_io(:)                  = rmdi
  emis_pft_io(:)              = rmdi
  z0hm_pft_io(:)              = rmdi
  z0hm_classic_pft_io(1:npft) = rmdi
  dust_veg_scj_io(:)          = rmdi
  fl_o3_ct_io(:)              = rmdi
  dfp_dcuo_io(:)              = rmdi
  ci_st_io(:)                 = rmdi
  gpp_st_io(:)                = rmdi
  ief_io(:)                   = rmdi
  tef_io(:)                   = rmdi
  mef_io(:)                   = rmdi
  aef_io(:)                   = rmdi
  q10_leaf_io(:)              = rmdi
  lma_io(:)                   = rmdi
  nmass_io(:)                 = rmdi
  vsl_io(:)                   = rmdi
  vint_io(:)                  = rmdi
  kn_io(:)                    = rmdi
  knl_io(:)                   = rmdi
  fef_co2_io(:)               = rmdi
  fef_co_io(:)                = rmdi
  fef_ch4_io(:)               = rmdi
  fef_nox_io(:)               = rmdi
  fef_so2_io(:)               = rmdi
  fef_oc_io(:)                = rmdi
  fef_bc_io(:)                = rmdi
  ccleaf_min_io(:)            = rmdi
  ccleaf_max_io(:)            = rmdi
  ccwood_min_io(:)            = rmdi
  ccwood_max_io(:)            = rmdi
  avg_ba_io(:)                = rmdi
    
  canht_ft_io(:) = rmdi
  lai_io(:)      = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info(routinename, "Reading JULES_PFTPARM namelist...")

! Open the pft parameters namelist file
  OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),        &
                 STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=error,&
                 IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal(routinename,                                            &
                   "Error opening namelist file pft_params.nml " //           &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  READ(namelist_unit, nml=jules_pftparm, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal(routinename,                                            &
                   "Error reading namelist JULES_PFTPARM " //                 &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

! Close the namelist file
  CLOSE(namelist_unit, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal(routinename,                                            &
                   "Error closing namelist file pft_params.nml " //           &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Radiation and albedo parameters.
  orient(:)       = orient_io(1:npft)
  albsnc_max(:)   = albsnc_max_io(1:npft)
  albsnc_min(:)   = albsnc_min_io(1:npft)
  albsnf_maxu(:)  = albsnf_maxu_io(1:npft)
  albsnf_max(:)   = albsnf_max_io(1:npft)
  albsnf_maxl(:)  = albsnf_maxl_io(1:npft)
  alniru(:)       = alniru_io(1:npft)
  alnir(:)        = alnir_io(1:npft)
  alnirl(:)       = alnirl_io(1:npft)
  alparu(:)       = alparu_io(1:npft)
  alpar(:)        = alpar_io(1:npft)
  alparl(:)       = alparl_io(1:npft)
  kext(:)         = kext_io(1:npft)
  kpar(:)         = kpar_io(1:npft)
  lai_alb_lim(:)  = lai_alb_lim_io(1:npft)
  can_struct_a(:) = can_struct_a_io(1:npft)
  omegau(:)       = omegau_io(1:npft)
  omega(:)        = omega_io(1:npft)
  omegal(:)       = omegal_io(1:npft)
  omniru(:)       = omniru_io(1:npft)
  omnir(:)        = omnir_io(1:npft)
  omnirl(:)       = omnirl_io(1:npft)

! Photosynthesis and respiration parameters.
  c3(:)           = c3_io(1:npft)
  alpha(:)        = alpha_io(1:npft)
  dqcrit(:)       = dqcrit_io(1:npft)
  fd(:)           = fd_io(1:npft)
  f0(:)           = f0_io(1:npft)
  kn(:)           = kn_io(1:npft)
  knl(:)          = knl_io(1:npft)
  neff(:)         = neff_io(1:npft)
  nl0(:)          = nl0_io(1:npft)
  nr_nl(:)        = nr_nl_io(1:npft)
  ns_nl(:)        = ns_nl_io(1:npft)
  r_grow(:)       = r_grow_io(1:npft)
  tlow(:)         = tlow_io(1:npft)
  tupp(:)         = tupp_io(1:npft)

! Trait physiology parameters
  lma(:)          = lma_io(1:npft)
  nmass(:)        = nmass_io(1:npft)
  vsl(:)          = vsl_io(1:npft)
  vint(:)         = vint_io(1:npft)
  q10_leaf(:)     = q10_leaf_io(1:npft)
  nr(:)           = nr_io(1:npft)
  nsw(:)          = nsw_io(1:npft)
  hw_sw(:)        = hw_sw_io(1:npft)

! Allometric and other parameters.
  a_wl(:)         = a_wl_io(1:npft)
  a_ws(:)         = a_ws_io(1:npft)
  b_wl(:)         = b_wl_io(1:npft)
  eta_sl(:)       = eta_sl_io(1:npft)
  sigl(:)         = sigl_io(1:npft)

! Phenology parameters.
  g_leaf_0(:)     = g_leaf_0_io(1:npft)
  dgl_dm(:)       = dgl_dm_io(1:npft)
  fsmc_of(:)      = fsmc_of_io(1:npft)
  dgl_dt(:)       = dgl_dt_io(1:npft)
  tleaf_of(:)     = tleaf_of_io(1:npft)

! Hydrological, thermal and other "physical" characteristics.
! Note that z0h_z0m, canht_ft and lai are read in via the pftparm namelist
! since they logically belong there, but the actual variables aren't in pftparm.
  catch0(:)       = catch0_io(1:npft)
  dcatch_dlai(:)  = dcatch_dlai_io(1:npft)
  infil_f(:)      = infil_f_io(1:npft)
  glmin(:)        = glmin_io(1:npft)
  dz0v_dh(:)      = dz0v_dh_io(1:npft)
  rootd_ft(:)     = rootd_ft_io(1:npft)
  psi_close(:)    = psi_close_io(1:npft)
  psi_open(:)     = psi_open_io(1:npft)
  fsmc_p0(:)      = fsmc_p0_io(1:npft)
  fsmc_mod(:)     = fsmc_mod_io(1:npft)
  emis_pft(:)     = emis_pft_io(1:npft)
  z0h_z0m(1:npft) = z0hm_pft_io(1:npft)
  z0h_z0m_classic(1:npft) = z0hm_classic_pft_io(1:npft)
  canht_pft(:,:)   = SPREAD(canht_ft_io(1:npft), 1, land_pts)
  lai_pft(:,:)        = SPREAD(lai_io(1:npft), 1, land_pts)

! Ozone damage parameters.
  fl_o3_ct(:)     = fl_o3_ct_io(1:npft)
  dfp_dcuo(:)     = dfp_dcuo_io(1:npft)

! BVOC emission parameters.
  ci_st(:)        = ci_st_io(1:npft)
  gpp_st(:)       = gpp_st_io(1:npft)
  ief(:)          = ief_io(1:npft)
  tef(:)          = tef_io(1:npft)
  mef(:)          = mef_io(1:npft)
  aef(:)          = aef_io(1:npft)
  
! INFERNO emission parameters
  fef_co2(:)      = fef_co2_io(1:npft)
  fef_co(:)       = fef_co_io(1:npft)
  fef_ch4(:)      = fef_ch4_io(1:npft)
  fef_nox(:)      = fef_nox_io(1:npft)
  fef_so2(:)      = fef_so2_io(1:npft)
  fef_oc(:)       = fef_oc_io(1:npft)
  fef_bc(:)       = fef_bc_io(1:npft)
  
! INFERNO combustion parameters
  ccleaf_min(:)   = ccleaf_min_io(1:npft)
  ccleaf_max(:)   = ccleaf_max_io(1:npft)
  ccwood_min(:)   = ccwood_min_io(1:npft)
  ccwood_max(:)   = ccwood_max_io(1:npft)
  avg_ba(:)       = avg_ba_io(1:npft)
  
!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but for now we insist on
! getting all parameters (and that there are not rmdi!).
!-----------------------------------------------------------------------------
  error = 0
  IF ( ANY( orient(:) < 0 ) ) THEN  ! orient was initialised to < 0
    error = 1
    CALL log_error(routinename, "No value for orient")
  END IF
  IF ( ANY( fsmc_mod(:) < 0 ) ) THEN  ! fsmc_mod was initialised to < 0
    error = 1
    CALL log_error(routinename, "No value for fsmc_mod")
  END IF
  IF ( ANY( ABS( albsnc_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for albsnc_max")
  END IF
  IF ( ANY( ABS( albsnc_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for albsnc_min")
  END IF
  IF ( ANY( ABS( albsnf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for albsnf_max")
  END IF
  IF ( ANY( ABS( alnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for alnir")
  END IF
  IF ( ANY( ABS( alpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for alpar")
  END IF
  IF ( ANY( ABS( kext(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for kext")
  END IF
  IF ( ANY( ABS( kpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for kpar")
  END IF
  IF ( ANY( ABS( lai_alb_lim(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for lai_alb_lim")
  END IF
  IF ( ANY( ABS( can_struct_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for can_struct_a")
  END IF
  IF ( ANY( ABS( omega(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for omega")
  END IF
  IF ( ANY( ABS( omnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for omnir")
  END IF
  IF ( ANY( c3(:) < 0 ) ) THEN  ! c3 was initialised to < 0
    error = 1
    CALL log_error(routinename, "No value for c3")
  END IF
  IF ( ANY( ABS( alpha(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for alpha")
  END IF
  IF ( ANY( ABS( dqcrit(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for dqcrit")
  END IF
  IF ( ANY( ABS( fd(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for fd")
  END IF
  IF ( ANY( ABS( f0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for f0")
  END IF
  IF ( ANY( ABS( neff(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for neff")
  END IF
  IF ( ANY( ABS( nl0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for nl0")
  END IF
  IF ( ANY( ABS( nr_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for nr_nl")
  END IF
  IF ( ANY( ABS( ns_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for ns_nl")
  END IF
  IF ( ANY( ABS( r_grow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for r_grow")
  END IF
  IF ( ANY( ABS( tlow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for tlow")
  END IF
  IF ( ANY( ABS( tupp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for tupp")
  END IF

  IF ( l_albedo_obs ) THEN
    IF ( ANY( ABS( albsnf_maxl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for albsnf_maxl")
    END IF
    IF ( ANY( ABS( albsnf_maxu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for albsnf_maxu")
    END IF
    IF ( l_spec_albedo ) THEN
      IF ( ANY( ABS( alnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for alnirl")
      END IF
      IF ( ANY( ABS( alniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for alniru")
      END IF
      IF ( ANY( ABS( alparl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for alparl")
      END IF
      IF ( ANY( ABS( alparu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for alparu")
      END IF
      IF ( ANY( ABS( omegal(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for omegal")
      END IF
      IF ( ANY( ABS( omegau(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for omegau")
      END IF
      IF ( ANY( ABS( omnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for omnirl")
      END IF
      IF ( ANY( ABS( omniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
        error = 1
        CALL log_error(routinename, "No value for omniru")
      END IF
    END IF
  END IF

  IF (l_trait_phys) THEN
    IF ( ANY( ABS( lma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for lma")
    END IF
    IF ( ANY( ABS( nmass(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for nmass")
    END IF
    IF ( ANY( ABS( vsl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for vsl")
    END IF
    IF ( ANY( ABS( vint(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for vint")
    END IF
    IF ( ANY( ABS( nr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for nr")
    END IF
    IF ( ANY( ABS( nsw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for nsw")
    END IF
    IF ( ANY( ABS( hw_sw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for hw_sw")
    END IF
    
  END IF !l_trait_phys

  IF ( ANY( ABS( kn(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for kn")
  END IF
  IF ( can_rad_mod==6 .AND. ANY( ABS( knl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for knl")
  END IF
  IF ( ANY( ABS( q10_leaf(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for q10_leaf")
  END IF
  IF ( ANY( ABS( a_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for a_wl")
  END IF
  IF ( ANY( ABS( a_ws(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for a_ws")
  END IF
  IF ( ANY( ABS( b_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for b_wl")
  END IF
  IF ( ANY( ABS( eta_sl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for eta_sl")
  END IF
  IF ( ANY( ABS( sigl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for sigl")
  END IF
  IF ( ANY( ABS( g_leaf_0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for g_leaf_0")
  END IF
  IF ( ANY( ABS( dgl_dm(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for dgl_dm")
  END IF
  IF ( ANY( ABS( fsmc_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for fsmc_of")
  END IF
  IF ( ANY( ABS( dgl_dt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for dgl_dt")
  END IF
  IF ( ANY( ABS( tleaf_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for tleaf_of")
  END IF
  IF ( ANY( ABS( catch0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for catch0")
  END IF
  IF ( ANY( ABS( dcatch_dlai(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for dcatch_dlai")
  END IF
  IF ( ANY( ABS( infil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for infil_f")
  END IF
  IF ( ANY( ABS( glmin(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for glmin")
  END IF
  IF ( ANY( ABS( dz0v_dh(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for dz0v_dh")
  END IF
  IF ( ANY( ABS( rootd_ft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for rootd_ft")
  END IF
  
  IF ( l_use_pft_psi ) THEN
    IF ( ANY( ABS( psi_close(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for psi_close")
    END IF
    IF ( ANY( ABS( psi_open(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for psi_open")
    END IF
  END IF !l_use_pft_psi
  
  IF ( l_bvoc_emis ) THEN
    IF ( ANY( ABS( ci_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ci_st")
    END IF
    IF ( ANY( ABS( gpp_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for gpp_st")
    END IF
    IF ( ANY( ABS( ief(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ief")
    END IF
    IF ( ANY( ABS( tef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for tef")
    END IF
    IF ( ANY( ABS( mef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for mef")
    END IF
    IF ( ANY( ABS( aef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for aef")
    END IF
  END IF

  IF ( l_inferno ) THEN
    IF ( ANY( ABS( fef_co2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fef_co2")
    END IF
    IF ( ANY( ABS( fef_co(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fef_co")
    END IF
    IF ( ANY( ABS( fef_ch4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fef_ch4")
    END IF
    IF ( ANY( ABS( fef_nox(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fef_nox")
    END IF
    IF ( ANY( ABS( fef_so2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fef_so2")
    END IF
    IF ( ANY( ABS( ccleaf_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ccleaf_min")
    END IF
    IF ( ANY( ABS( ccleaf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ccleaf_max")
    END IF
    IF ( ANY( ABS( ccwood_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ccwood_min")
    END IF
    IF ( ANY( ABS( ccwood_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for ccwood_max")
    END IF
    IF ( ANY( ABS( avg_ba(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for avg_ba")
    END IF
  END IF

  IF ( l_o3_damage ) THEN
    IF ( ANY( ABS( fl_o3_ct(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for fl_o3_ct")
    END IF
    IF ( ANY( ABS( dfp_dcuo(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      error = 1
      CALL log_error(routinename, "No value for dfp_dcuo")
    END IF
  END IF

  IF ( ANY( ABS( fsmc_p0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for fsmc_p0")
  END IF
  IF ( ANY( ABS( emis_pft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for emis_pft")
  END IF
  IF ( ANY( ABS( z0h_z0m(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for z0hm_pft")
  END IF
  IF ( ANY( ABS( z0h_z0m_classic(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for z0hm_classic_pft")
  END IF
  IF ( ANY( ABS( canht_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for canht_ft")
  END IF
  IF ( ANY( ABS( lai_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for lai")
  END IF

  IF ( error /= 0 )                                                           &
    CALL log_fatal(routinename,                                            &
                   "Variable(s) missing from namelist - see earlier " //      &
                   "error message(s)")

!-----------------------------------------------------------------------------
! Check that glmin is >0.
! This ensures that wt_ext in subroutine soil_evap cannot become a NaN (which
! it would if gs=glmin and gsoil=0), or blow up, and might well be required
! elsewhere too.
!-----------------------------------------------------------------------------
  IF ( ANY(glmin < 1.0e-10) )                                                 &
    CALL log_warn(routinename,                                             &
                  "Increasing one or more values of glmin - very small " //   &
                  "values can cause model to blow up or NaNs")

  WHERE ( glmin < 1.0e-10 )
    glmin = 1.0e-10
  END WHERE

  IF ( l_crop ) THEN
    IF ( ANY( ABS( a_ws(nnpft + 1: npft) - 1.0 ) > EPSILON(1.0) ) ) THEN
      CALL log_fatal(routinename, "crop tiles should have a_ws=1.0")
    END IF
  END IF

  IF ( l_use_pft_psi ) THEN
    IF ( ANY( psi_close(1: npft) > EPSILON(1.0) ) ) THEN
      CALL log_fatal(routinename, "psi_close should be negative")
    END IF
    IF ( ANY( psi_open(1: npft) > EPSILON(1.0) ) ) THEN
      CALL log_fatal(routinename, "psi_open should be negative")
    END IF
  END IF  

!-----------------------------------------------------------------------------
! fsmc_mod=1 should not be allowed with a layered RothC  model until this has
! been properly evaluated. (With fsmc_mod=1, subroutine root_frac does not
! return the exponential root profile that users might expect.)
! Note that l_layeredC=T is not currently allowed with the UM.
!-----------------------------------------------------------------------------
  IF ( l_layeredC .AND. ( soil_bgc_model == soil_model_rothc ) .AND.          &
       ANY( fsmc_mod(:) == 1 ) ) THEN
    CALL log_error(routinename,                                            &
                   "fsmc_mod=1 is not allowed with l_layeredC and RothC")
  END IF

  RETURN

END SUBROUTINE init_pftparm_jules 

SUBROUTINE init_pftparm_cable(nml_dir)

  USE missing_data_mod, ONLY :                                                &
!  imported scalar parameters
     rmdi
  USE io_constants, ONLY: namelist_unit

  USE string_utils_mod, ONLY: to_string

  USE errormessagelength_mod, ONLY: errormessagelength

  USE logging_mod, ONLY: log_info, log_fatal

  USE jules_surface_types_mod, ONLY : npft

  USE max_dimensions, ONLY: npft_max

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!  Reads the JULES_PFT_PARAMS_CABLE namelist for standalone runs
!-----------------------------------------------------------------------------
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                           ! namelists

!-----------------------------------------------------------------------------
! Work variables
!-----------------------------------------------------------------------------

! Work variables
  INTEGER :: error  ! Error indicator
  CHARACTER(LEN=errormessagelength) :: iomessage

  CHARACTER(LEN=*), PARAMETER :: routinename='INIT_PFTPARM_CABLE'

  INTEGER, PARAMETER ::                                                       &
    nsl = 6,  &   ! number of soil layers
    nscs = 2, &   ! number of soil carbon stores
    nvcs = 3, &   ! number of vegetation carbon stores
    nrb = 3       ! number of radiation bands

  ! With some compilers, namelists cannot contain multidimensional arrays.
  ! Therefore, an input type without multidimensional arrays is used to read
  ! in the the values from the namelist, and these values will then be
  ! transferred to the desired data type which does contain multidimensional
  ! arrays


  REAL ::                       &
        canst1_io(npft_max),    &
        length_io(npft_max),    &
        width_io(npft_max),     &
        vcmax_io(npft_max),     &
        ejmax_io(npft_max),     &
        hc_io(npft_max),        &
        xfang_io(npft_max),     &
        rp20_io(npft_max),      &
        rpcoef_io(npft_max),    &
        rs20_io(npft_max),      &
        wai_io(npft_max),       &
        rootbeta_io(npft_max),  &
        shelrb_io(npft_max),    &
        vegcf_io(npft_max),     &
        frac4_io(npft_max),     &
        xalbnir_io(npft_max),   &
        extkn_io(npft_max),     &
        tminvj_io(npft_max),    &
        tmaxvj_io(npft_max),    &
        vbeta_io(npft_max),     &
        a1gs_io(npft_max),      &
        d0gs_io(npft_max),      &
        alpha_io(npft_max),     &
        convex_io(npft_max),    &
        cfrd_io(npft_max),      &
        gswmin_io(npft_max),    &
        conkc0_io(npft_max),    &
        conko0_io(npft_max),    &
        ekc_io(npft_max),       &
        eko_io(npft_max),       &
        g0_io(npft_max),        &
        g1_io(npft_max),        & 
        zr_io(npft_max),        &
        clitt_io(npft_max),     &
        froot1_io(npft_max),    &
        froot2_io(npft_max),    &
        froot3_io(npft_max),    &
        froot4_io(npft_max),    &
        froot5_io(npft_max),    &
        froot6_io(npft_max),    &
        csoil1_io(npft_max),    &
        csoil2_io(npft_max),    &
        ratecs1_io(npft_max),   &
        ratecs2_io(npft_max),   &
        cplant1_io(npft_max),   &
        cplant2_io(npft_max),   &
        cplant3_io(npft_max),   &
        ratecp1_io(npft_max),   &
        ratecp2_io(npft_max),   &
        ratecp3_io(npft_max),   &
        refl1_io(npft_max),     &
        refl2_io(npft_max),     &
        refl3_io(npft_max),     &
        taul1_io(npft_max),     &        
        taul2_io(npft_max),     &
        taul3_io(npft_max)        

   
  TYPE vegin_type

   REAL ::                  &
        canst1(npft_max),       &
        dleaf(npft_max),        &
        length(npft_max),       &
        width(npft_max),        &
        vcmax(npft_max),        &
        ejmax(npft_max),        &
        hc(npft_max),           &
        xfang(npft_max),        &
        rp20(npft_max),         &
        rpcoef(npft_max),       &
        rs20(npft_max),         &
        wai(npft_max),          &
        rootbeta(npft_max),     &
        shelrb(npft_max),       &
        vegcf(npft_max),        &
        frac4(npft_max),        &
        xalbnir(npft_max),      &
        extkn(npft_max),        &
        tminvj(npft_max),       &
        tmaxvj(npft_max),       &
        vbeta(npft_max),        &
        a1gs(npft_max),         &
        d0gs(npft_max),         &
        alpha(npft_max),        &
        convex(npft_max),       &
        cfrd(npft_max),         &
        gswmin(npft_max),       &
        conkc0(npft_max),       &
        conko0(npft_max),       &
        ekc(npft_max),          &
        eko(npft_max),          &
        g0(npft_max),           &
        g1(npft_max),           & 
        zr(npft_max),           &
        clitt(npft_max),        &
        froot(nsl,npft_max),    &
        csoil(nscs,npft_max),   &
        ratecs(nscs,npft_max),  &
        cplant(nvcs,npft_max),  &
        ratecp(nvcs,npft_max),  &
        refl(nrb,npft_max),     &
        taul(nrb,npft_max)        

   
  END TYPE vegin_type

  TYPE(vegin_type),  SAVE  :: vegin


!-----------------------------------------------------------------------------
! Namelist definition
!-----------------------------------------------------------------------------
  NAMELIST / jules_pftparm_cable/ canst1_io, length_io, width_io, vcmax_io,   &
            ejmax_io, hc_io, xfang_io, rp20_io, rpcoef_io, rs20_io, wai_io,   &
            rootbeta_io, shelrb_io, vegcf_io, frac4_io, xalbnir_io, extkn_io, &
            tminvj_io, tmaxvj_io, vbeta_io, a1gs_io, d0gs_io, alpha_io,       &
            convex_io, cfrd_io, gswmin_io, conkc0_io, conko0_io, ekc_io,      &
            eko_io, g0_io, g1_io, zr_io, clitt_io, froot1_io, froot2_io,      &
            froot3_io, froot4_io, froot5_io, froot6_io, cplant1_io,           &
            cplant2_io, cplant3_io, csoil1_io, csoil2_io, ratecp1_io,         &
            ratecp2_io, ratecp3_io, ratecs1_io, ratecs2_io, refl1_io,         &
            refl2_io, refl3_io, taul1_io, taul2_io, taul3_io 

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
  canst1_io(:npft)     = rmdi
  length_io(:npft)     = rmdi
  width_io(:npft)      = rmdi
  vcmax_io(:npft)      = rmdi
  ejmax_io(:npft)      = rmdi
  hc_io(:npft)         = rmdi
  xfang_io(:npft)      = rmdi
  rp20_io(:npft)       = rmdi
  rpcoef_io(:npft)     = rmdi
  rs20_io(:npft)       = rmdi
  wai_io(:npft)        = rmdi
  rootbeta_io(:npft)   = rmdi
  shelrb_io(:npft)     = rmdi
  vegcf_io(:npft)      = rmdi
  frac4_io(:npft)      = rmdi
  xalbnir_io(:npft)    = rmdi
  extkn_io(:npft)      = rmdi
  tminvj_io(:npft)     = rmdi
  tmaxvj_io(:npft)     = rmdi
  vbeta_io(:npft)      = rmdi
  a1gs_io(:npft)       = rmdi
  d0gs_io(:npft)       = rmdi
  alpha_io(:npft)      = rmdi
  convex_io(:npft)     = rmdi
  cfrd_io(:npft)       = rmdi
  gswmin_io(:npft)     = rmdi
  conkc0_io(:npft)     = rmdi
  conko0_io(:npft)     = rmdi
  ekc_io(:npft)        = rmdi
  eko_io(:npft)        = rmdi
  g0_io(:npft)         = rmdi
  g1_io(:npft)         = rmdi
  zr_io(:npft)         = rmdi
  clitt_io(:npft)      = rmdi
  froot1_io(:npft)     = rmdi
  froot2_io(:npft)     = rmdi
  froot3_io(:npft)     = rmdi
  froot4_io(:npft)     = rmdi
  froot5_io(:npft)     = rmdi
  froot6_io(:npft)     = rmdi
  cplant1_io(:npft)    = rmdi
  cplant2_io(:npft)    = rmdi
  cplant3_io(:npft)    = rmdi
  csoil1_io(:npft)     = rmdi
  csoil2_io(:npft)     = rmdi
  ratecp1_io(:npft)    = rmdi
  ratecp2_io(:npft)    = rmdi
  ratecp3_io(:npft)    = rmdi
  ratecs1_io(:npft)    = rmdi
  ratecs2_io(:npft)    = rmdi
  refl1_io(:npft)      = rmdi
  refl2_io(:npft)      = rmdi
  refl3_io(:npft)      = rmdi
  taul1_io(:npft)      = rmdi
  taul2_io(:npft)      = rmdi
  taul3_io(:npft)      = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info(routinename, "Reading JULES_PFTPARM_CABLE namelist...")

  OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),&
                 STATUS='old', POSITION='rewind', ACTION='read', IOSTAT  = error,&
                 IOMSG = iomessage)
  IF ( error /= 0 )                                                         &
    CALL log_fatal(routinename,                                              &
                   "Error opening namelist file pft_params.nml " //   &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //     &
                   TRIM(iomessage) // ")")

  READ(namelist_unit, NML = jules_pftparm_cable, IOSTAT = error, IOMSG = iomessage)
  IF ( error /= 0 )                                                         &
    CALL log_fatal(routinename,                                              &
                   "Error reading namelist JULES_PFTPARM_CABLE " //            &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //     &
                   TRIM(iomessage) // ")")

  CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
  IF ( error /= 0 )                                                         &
    CALL log_fatal(routinename,                                              &
                   "Error closing namelist file pft_params.nml " // &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //     &
                   TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Transfer values from io values to vegin
!-----------------------------------------------------------------------------

  vegin%canst1(:npft)     = canst1_io(:npft)
  vegin%length(:npft)     = length_io(:npft)
  vegin%width(:npft)      = width_io(:npft)
  vegin%vcmax(:npft)      = vcmax_io(:npft)
  vegin%ejmax(:npft)      = ejmax_io(:npft)
  vegin%hc(:npft)         = hc_io(:npft)
  vegin%xfang(:npft)      = xfang_io(:npft)
  vegin%rp20(:npft)       = rp20_io(:npft)
  vegin%rpcoef(:npft)     = rpcoef_io(:npft)
  vegin%rs20(:npft)       = rs20_io(:npft)
  vegin%wai(:npft)        = wai_io(:npft)
  vegin%rootbeta(:npft)   = rootbeta_io(:npft)
  vegin%shelrb(:npft)     = shelrb_io(:npft)
  vegin%vegcf(:npft)      = vegcf_io(:npft)
  vegin%frac4(:npft)      = frac4_io(:npft)
  vegin%xalbnir(:npft)    = xalbnir_io(:npft)
  vegin%extkn(:npft)      = extkn_io(:npft)
  vegin%tminvj(:npft)     = tminvj_io(:npft)
  vegin%tmaxvj(:npft)     = tmaxvj_io(:npft)
  vegin%vbeta(:npft)      = vbeta_io(:npft)
  vegin%a1gs(:npft)       = a1gs_io(:npft)
  vegin%d0gs(:npft)       = d0gs_io(:npft)
  vegin%alpha(:npft)      = alpha_io(:npft)
  vegin%convex(:npft)     = convex_io(:npft)
  vegin%cfrd(:npft)       = cfrd_io(:npft)
  vegin%gswmin(:npft)     = gswmin_io(:npft)
  vegin%conkc0(:npft)     = conkc0_io(:npft)
  vegin%conko0(:npft)     = conko0_io(:npft)
  vegin%ekc(:npft)        = ekc_io(:npft)
  vegin%eko(:npft)        = eko_io(:npft)
  vegin%g0(:npft)         = g0_io(:npft)
  vegin%g1(:npft)         = g1_io(:npft)
  vegin%zr(:npft)         = zr_io(:npft)
  vegin%clitt(:npft)      = clitt_io(:npft)
  vegin%froot(1,:npft)    = froot1_io(:npft)
  vegin%froot(2,:npft)    = froot2_io(:npft)
  vegin%froot(3,:npft)    = froot3_io(:npft)
  vegin%froot(4,:npft)    = froot4_io(:npft)
  vegin%froot(5,:npft)    = froot5_io(:npft)
  vegin%froot(6,:npft)    = froot6_io(:npft)
  vegin%cplant(1,:npft)   = cplant1_io(:npft)
  vegin%cplant(2,:npft)   = cplant2_io(:npft)
  vegin%cplant(3,:npft)   = cplant3_io(:npft)
  vegin%csoil(1,:npft)    = csoil1_io(:npft)
  vegin%csoil(2,:npft)    = csoil2_io(:npft)
  vegin%ratecp(1,:npft)   = ratecp1_io(:npft)
  vegin%ratecp(2,:npft)   = ratecp2_io(:npft)
  vegin%ratecp(3,:npft)   = ratecp3_io(:npft)
  vegin%ratecs(1,:npft)   = ratecs1_io(:npft)
  vegin%ratecs(2,:npft)   = ratecs2_io(:npft)
  vegin%refl(1,:npft)     = refl1_io(:npft)
  vegin%refl(2,:npft)     = refl2_io(:npft)
  vegin%refl(3,:npft)     = refl3_io(:npft)
  vegin%taul(1,:npft)     = taul1_io(:npft)
  vegin%taul(2,:npft)     = taul2_io(:npft)
  vegin%taul(3,:npft)     = taul3_io(:npft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but for now we insist on
! getting all parameters (and that there are not rmdi!).
!-----------------------------------------------------------------------------
  error = 0
  IF ( ANY( ABS( vegin%canst1(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN  
    error = 1
    CALL log_error(routinename, "No value for canst1")
  END IF
  IF ( ANY( ABS( vegin%length(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for length")
  END IF
  IF ( ANY( ABS( vegin%width(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for width")
  END IF
  IF ( ANY( ABS( vegin%vcmax(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for vcmax")
  END IF
  IF ( ANY( ABS( vegin%ejmax(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for ejmax")
  END IF
  IF ( ANY( ABS( vegin%hc(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for hc")
  END IF
  IF ( ANY( ABS( vegin%xfang(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for xfang")
  END IF
  IF ( ANY( ABS( vegin%rp20(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for rp20")
  END IF
  IF ( ANY( ABS( vegin%rpcoef(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for rpcoef")
  END IF
  IF ( ANY( ABS( vegin%rs20(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for rs20")
  END IF
  IF ( ANY( ABS( vegin%wai(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for wai")
  END IF
  IF ( ANY( ABS( vegin%rootbeta(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for rootbeta")
  END IF
  IF ( ANY( ABS( vegin%shelrb(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for shelrb")
  END IF
  IF ( ANY( ABS( vegin%vegcf(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for vegcf")
  END IF
  IF ( ANY( ABS( vegin%frac4(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for frac4")
  END IF
  IF ( ANY( ABS( vegin%xalbnir(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for xalbnir")
  END IF
  IF ( ANY( ABS( vegin%extkn(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for extkni")
  END IF
  IF ( ANY( ABS( vegin%tminvj(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for tminvj")
  END IF
  IF ( ANY( ABS( vegin%tmaxvj(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for tmaxvj")
  END IF
  IF ( ANY( ABS( vegin%vbeta(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for vbeta")
  END IF
  IF ( ANY( ABS( vegin%a1gs(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for a1hs")
  END IF
  IF ( ANY( ABS( vegin%d0gs(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for d0gs")
  END IF
  IF ( ANY( ABS( vegin%alpha(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for alpha")
  END IF
  IF ( ANY( ABS( vegin%convex(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for convex")
  END IF
  IF ( ANY( ABS( vegin%cfrd(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for cfrd")
  END IF
  IF ( ANY( ABS( vegin%gswmin(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for gswmin")
  END IF
  IF ( ANY( ABS( vegin%conkc0(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for conkc0")
  END IF
  IF ( ANY( ABS( vegin%conko0(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for conko0")
  END IF
  IF ( ANY( ABS( vegin%ekc(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for ekc")
  END IF
  IF ( ANY( ABS( vegin%eko(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for eko")
  END IF
  IF ( ANY( ABS( vegin%g0(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for g0")
  END IF
  IF ( ANY( ABS( vegin%g1(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for g1")
  END IF
  IF ( ANY( ABS( vegin%zr(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for zr")
  END IF
  IF ( ANY( ABS( vegin%clitt(:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for clitt")
  END IF
  IF ( ANY( ABS( vegin%froot(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for froot")
  END IF
  IF ( ANY( ABS( vegin%cplant(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for cplant")
  END IF
  IF ( ANY( ABS( vegin%csoil(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for csoil")
  END IF
  IF ( ANY( ABS( vegin%ratecp(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for ratecp")
  END IF
  IF ( ANY( ABS( vegin%ratecs(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for ratecs")
  END IF
  IF ( ANY( ABS( vegin%refl(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for refl")
  END IF
  IF ( ANY( ABS( vegin%taul(:,:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error(routinename, "No value for taul")
  END IF

  IF ( error /= 0 )                                                           &
    CALL log_fatal(routinename,                                            &
                   "Variable(s) missing from namelist - see earlier " //      &
                   "error message(s)")

  vegin%dleaf(:) = SQRT(vegin%width(:) * vegin%length(:))
    
END SUBROUTINE init_pftparm_cable

#endif
