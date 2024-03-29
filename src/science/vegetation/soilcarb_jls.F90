! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE soilcarb_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOILCARB_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine SOILCARB --------------------------------------------------------
!
! Purpose : Updates carbon and nitrogen contents of the soil.
!
!Note that triffid is not compatible with soil tiling at this time, so all
!_soilt variables have their soil tile index hard-coded to 1
!
! ----------------------------------------------------------------------------
SUBROUTINE soilcarb (land_pts, trif_pts, trif_index,                          &
                     forw, r_gamma, lit_c, lit_c_t, lit_n_t, resp_frac,       &
                     resp_s_pot, cs, ns_gb, neg_n, implicit_resp_correction,  &
                     burnt_soil)

USE jules_surface_types_mod, ONLY: npft
USE jules_soil_biogeochem_mod, ONLY: bio_hum_CN

USE prognostics, ONLY: ns_pool_gb, n_inorg_soilt_lyrs
USE jules_vegetation_mod, ONLY: l_nitrogen
USE trif_vars_mod, ONLY: minl_n_gb, minl_n_pot_gb, immob_n_gb, immob_n_pot_gb,&
                         fn_gb, resp_s_diag_gb, resp_s_pot_diag_gb,           &
                         dpm_ratio_gb, n_gas_gb, resp_s_to_atmos_gb,          &
                         g_burn_gb, burnt_carbon_dpm, burnt_carbon_rpm
                        
USE jules_soil_mod, ONLY: cs_min
USE ancil_info, ONLY: dim_cslayer
USE jules_vegetation_mod, ONLY: l_trif_fire
#if !defined(UM_JULES)
USE p_s_parms, ONLY: sthu_soilt
#endif
USE dpm_rpm_mod, ONLY: dpm_rpm 
USE decay_mod, ONLY: decay
 
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER, INTENT(IN) ::                                                        &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL, INTENT(IN) ::                                                           &
  forw,                                                                       &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL, INTENT(IN) ::                                                           &
  lit_c(land_pts,npft),                                                       &
    ! Carbon Litter (kg C/m2/360days).
  lit_c_t(land_pts),                                                          &
    ! Total carbon litter (kg C/m2/360days).
  lit_n_t(land_pts),                                                          &
    ! Total nitrogen litter (kg N/m2/360days).
  resp_frac(land_pts)
    ! The fraction of RESP_S (soil respiration) that forms new soil C.
    ! This is the fraction that is NOT released to the atmosphere.

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL, INTENT(INOUT) ::                                                        &
  resp_s_pot(land_pts,dim_cslayer,5),                                         &
    ! Soil respiration (kg C/m2/360days).
  cs(land_pts,dim_cslayer,4)
    ! Soil carbon (kg C/m2).
    ! The 4 soil C pools are DPM, RPM, biomass and humus.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL, INTENT(OUT) ::                                                          &
  ns_gb(land_pts,dim_cslayer),                                                &
    ! Total Soil N (kg N/m2).
  neg_n(land_pts),                                                            &
    ! Negative N required to prevent ns<0 (kg N).
  implicit_resp_correction(land_pts),                                         &
    ! Respiration carried to next triffid timestep to account for applying
    ! minimum soil carbon constraint (kg m-2).
  burnt_soil(land_pts)
    ! Burnt C in RPM and DPM pools, for cnsrv diagnostics (kg/m2/360days).

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL, PARAMETER :: lit_cn    = 300.0
REAL, PARAMETER :: nminl_gas = 0.01
REAL, PARAMETER ::                                                            &
  ccdpm_min = 0.8,                                                            &
  ccdpm_max = 1.0,                                                            &
    ! Decomposable Plant Material burns between 80 to 100 %
  ccrpm_min = 0.0,                                                            &
  ccrpm_max = 0.2                                                           
    ! Resistant Plant Material burns between 0 to 20 %
    ! These values are also set in inferno_mod to calculate emitted_carbon_DPM
    ! and emitted_carbon_RPM, and are also set in soilcarb_layers

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  l,t                                                                          
    ! Loop counters

REAL ::                                                                       &
  cs_min_lit_cn,                                                              &
    ! cs_min/lit_cn - speeding up calcs
  cs_min_bio_hum_cn
    ! cs_min/bio_hum_cn - speeding up calcs

REAL ::                                                                       &
  dcs(land_pts,4),                                                            &
    ! Increment to the soil carbon (kg C/m2).
  dpc_dcs(land_pts,4),                                                        &
    ! Rate of change of PC with soil carbon (/360days).
  pc(land_pts,4),                                                             &
    ! Net carbon accumulation in the soil (kg C/m2/360days).
  pn(land_pts,4),                                                             &
    ! Net nitrogen accumulation in the soil (kg N/m2/360days).
  resp_frac_mult46(land_pts),                                                 &
    ! resp_frac (see above) multiplied by 0.46.
  resp_frac_mult54(land_pts),                                                 &
    ! resp_frac (see above) multiplied by 0.54.
  resp_s(land_pts,dim_cslayer,5),                                             &
    ! Soil respiration (kg C/m2/360days).
  cn(land_pts,dim_cslayer,5)
    ! C:N ratios of pools.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOILCARB'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Note: The 4 soil carbon pools are  1 decomposable plant material,
! 2 resistant plant material, 3 biomass, 4 humus.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
burnt_soil(:)     = 0.0
fn_gb(:,:)        = 1.0
n_gas_gb(:,:)     = 0.0
! Precalculate constants.
cs_min_lit_cn     = cs_min / lit_cn
cs_min_bio_hum_cn = cs_min / bio_hum_cn

DO t = 1,trif_pts
  l = trif_index(t)

  resp_s_pot(l,1,5) = resp_s_pot(l,1,1) + resp_s_pot(l,1,2)                   &
                      + resp_s_pot(l,1,3) + resp_s_pot(l,1,4)

  cn(l,1,1) = cs(l,1,1) / ns_pool_gb(l,1,1)  ! C:N ratio of DPM - Prognostic
  cn(l,1,2) = cs(l,1,2) / ns_pool_gb(l,1,2)  ! C:N ratio of DPM - Prognostic
  cn(l,1,3) = bio_hum_cn                     ! C:N ratio of BIO - Fixed
  cn(l,1,4) = bio_hum_cn                     ! C:N ratio of HUM - Fixed

  ! N pool BIO+HUM are diagnostic from equivalent cSoil pool
  ns_pool_gb(l,1,3) = cs(l,1,3) / bio_hum_cn
  ns_pool_gb(l,1,4) = cs(l,1,4) / bio_hum_cn

  cn(l,1,1) = MIN(MAX(cn(l,1,1),1.0e-6),1000.0)
  cn(l,1,2) = MIN(MAX(cn(l,1,2),1.0e-6),1000.0)
  cn(l,1,3) = MIN(MAX(cn(l,1,3),1.0e-6),1000.0)
  cn(l,1,4) = MIN(MAX(cn(l,1,4),1.0e-6),1000.0)

  cn(l,1,5) = SUM(cs(l,1,:)) / SUM(ns_pool_gb(l,1,:))

  minl_n_pot_gb(l,1,1)  = resp_s_pot(l,1,1) / cn(l,1,1)
  minl_n_pot_gb(l,1,2)  = resp_s_pot(l,1,2) / cn(l,1,2)
  minl_n_pot_gb(l,1,3)  = resp_s_pot(l,1,3) / cn(l,1,3)
  minl_n_pot_gb(l,1,4)  = resp_s_pot(l,1,4) / cn(l,1,4)

  minl_n_pot_gb(l,1,5)  = minl_n_pot_gb(l,1,1) + minl_n_pot_gb(l,1,2) +       &
                          minl_n_pot_gb(l,1,3) + minl_n_pot_gb(l,1,4)

  resp_frac_mult46(l)   = 0.46 * resp_frac(l)
  resp_frac_mult54(l)   = 0.54 * resp_frac(l)
  immob_n_pot_gb(l,1,1) = (resp_frac_mult46(l) * resp_s_pot(l,1,1) /          &
                          cn(l,1,3))  + (resp_frac_mult54(l) *                &
                          resp_s_pot(l,1,1) / cn(l,1,4))
  immob_n_pot_gb(l,1,2) = (resp_frac_mult46(l) * resp_s_pot(l,1,2) /          &
                          cn(l,1,3)) + (resp_frac_mult54(l) *                 &
                          resp_s_pot(l,1,2) / cn(l,1,4))
  immob_n_pot_gb(l,1,3) = (resp_frac_mult46(l) * resp_s_pot(l,1,3) /          &
                          cn(l,1,3)) + (resp_frac_mult54(l) *                 &
                          resp_s_pot(l,1,3) / cn(l,1,4))
  immob_n_pot_gb(l,1,4) = (resp_frac_mult46(l) * resp_s_pot(l,1,4) /          &
                          cn(l,1,3)) + (resp_frac_mult54(l) *                 &
                          resp_s_pot(l,1,4) / cn(l,1,4))

  immob_n_pot_gb(l,1,5) = immob_n_pot_gb(l,1,1) + immob_n_pot_gb(l,1,2) +     &
                          immob_n_pot_gb(l,1,3) + immob_n_pot_gb(l,1,4)

  !---------------------------------------------------------------------------
  ! If soil N demand greater than N_available then limit decay through fn_gb
  !---------------------------------------------------------------------------
  fn_gb(l,1) = 1.0
  IF ( l_nitrogen .AND.                                                       &
      (immob_n_pot_gb(l,1,5) - minl_n_pot_gb(l,1,5)) / r_gamma                &
      > n_inorg_soilt_lyrs(l,1,1) ) THEN

    fn_gb(l,1) = (((minl_n_pot_gb(l,1,3) + minl_n_pot_gb(l,1,4) -             &
                 immob_n_pot_gb(l,1,3) - immob_n_pot_gb(l,1,4)) / r_gamma) +  &
                 n_inorg_soilt_lyrs(l,1,1)) /                                 &
                 ((immob_n_pot_gb(l,1,1) - minl_n_pot_gb(l,1,1)) / r_gamma +  &
                 (immob_n_pot_gb(l,1,2) - minl_n_pot_gb(l,1,2)) / r_gamma)

    fn_gb(l,1) = MIN(MAX(fn_gb(l,1), 0.0), 1.0)
  END IF

  minl_n_gb(l,1,1)  = fn_gb(l,1) * minl_n_pot_gb(l,1,1)
  minl_n_gb(l,1,2)  = fn_gb(l,1) * minl_n_pot_gb(l,1,2)
  minl_n_gb(l,1,3)  = minl_n_pot_gb(l,1,3)
  minl_n_gb(l,1,4)  = minl_n_pot_gb(l,1,4)

  immob_n_gb(l,1,1) = fn_gb(l,1) * immob_n_pot_gb(l,1,1)
  immob_n_gb(l,1,2) = fn_gb(l,1) * immob_n_pot_gb(l,1,2)
  immob_n_gb(l,1,3) = immob_n_pot_gb(l,1,3)
  immob_n_gb(l,1,4) = immob_n_pot_gb(l,1,4)

  resp_s(l,1,1)     = fn_gb(l,1) * resp_s_pot(l,1,1)
  resp_s(l,1,2)     = fn_gb(l,1) * resp_s_pot(l,1,2)
  resp_s(l,1,3)     = resp_s_pot(l,1,3)
  resp_s(l,1,4)     = resp_s_pot(l,1,4)

  resp_s(l,1,5)     = resp_s(l,1,1) + resp_s(l,1,2) +                         &
                      resp_s(l,1,3) + resp_s(l,1,4)
  minl_n_gb(l,1,5)  = minl_n_gb(l,1,1) + minl_n_gb(l,1,2) +                   &
                      minl_n_gb(l,1,3) + minl_n_gb(l,1,4)
  immob_n_gb(l,1,5) = immob_n_gb(l,1,1) + immob_n_gb(l,1,2) +                 &
                      immob_n_gb(l,1,3) + immob_n_gb(l,1,4)

END DO

! calculate DPM:RPM ratio of input litter Carbon
CALL dpm_rpm(land_pts, trif_pts, trif_index, lit_c, dpm_ratio_gb)

DO t = 1,trif_pts
  l = trif_index(t)
  !---------------------------------------------------------------------------
  ! Diagnose the net local nitrogen flux into the soil
  !---------------------------------------------------------------------------
  pn(l,1) = dpm_ratio_gb(l) * lit_n_t(l) - minl_n_gb(l,1,1)
  pn(l,2) = (1.0 - dpm_ratio_gb(l)) * lit_n_t(l) - minl_n_gb(l,1,2)
  pn(l,3) = 0.46 * immob_n_gb(l,1,5) - minl_n_gb(l,1,3)
  pn(l,4) = 0.54 * immob_n_gb(l,1,5) - minl_n_gb(l,1,4)

  ns_pool_gb(l,1,1) = ns_pool_gb(l,1,1) + pn(l,1) / r_gamma
  ns_pool_gb(l,1,2) = ns_pool_gb(l,1,2) + pn(l,2) / r_gamma
  ns_pool_gb(l,1,3) = ns_pool_gb(l,1,3) + pn(l,3) / r_gamma
  ns_pool_gb(l,1,4) = ns_pool_gb(l,1,4) + pn(l,4) / r_gamma

  neg_n(l) = 0.0
  IF (ns_pool_gb(l,1,1) <  cs_min_lit_cn) THEN
    neg_n(l)  = neg_n(l) + ns_pool_gb(l,1,1) - cs_min_lit_cn
  END IF
  IF (ns_pool_gb(l,1,2) <  cs_min_lit_cn) THEN
    neg_n(l)  = neg_n(l) + ns_pool_gb(l,1,2) - cs_min_lit_cn
  END IF
  IF (ns_pool_gb(l,1,3) <  cs_min_bio_hum_cn) THEN
    neg_n(l) = neg_n(l) + ns_pool_gb(l,1,3) - cs_min_bio_hum_cn
  END IF
  IF (ns_pool_gb(l,1,4) <  cs_min_bio_hum_cn) THEN
    neg_n(l) = neg_n(l) + ns_pool_gb(l,1,4) - cs_min_bio_hum_cn
  END IF

  ns_pool_gb(l,1,1) = MAX(ns_pool_gb(l,1,1),cs_min_lit_cn)
  ns_pool_gb(l,1,2) = MAX(ns_pool_gb(l,1,2),cs_min_lit_cn)
  ns_pool_gb(l,1,3) = MAX(ns_pool_gb(l,1,3),cs_min_bio_hum_cn)
  ns_pool_gb(l,1,4) = MAX(ns_pool_gb(l,1,4),cs_min_bio_hum_cn)

  ! increase immobilisation to account for mininum n content
  immob_n_gb(l,1,5) = immob_n_gb(l,1,5) - (neg_n(l) * r_gamma)

  ! calculate mineralised gas emissions
  n_gas_gb(l,1) = nminl_gas * MAX((minl_n_gb(l,1,5) - immob_n_gb(l,1,5)),0.0)

  !---------------------------------------------------------------------------
  ! Update inorganic N
  !---------------------------------------------------------------------------
  n_inorg_soilt_lyrs(l,1,1) = n_inorg_soilt_lyrs(l,1,1) +                     &
                  (minl_n_gb(l,1,5) - immob_n_gb(l,1,5) - n_gas_gb(l,1))      &
                  / r_gamma
  ns_gb(l,1) = ns_pool_gb(l,1,1) + ns_pool_gb(l,1,2) +                        &
               ns_pool_gb(l,1,3) + ns_pool_gb(l,1,4)

END DO

#if !defined(UM_JULES)
!-----------------------------------------------------------------------------
! Remove burnt litter
!-----------------------------------------------------------------------------
IF (l_trif_fire) THEN
  DO t = 1,trif_pts
    l = trif_index(t)
    burnt_carbon_dpm(l) = MAX(g_burn_gb(l) *                                  &
                          (cs(l,1,1) * (ccdpm_min + (ccdpm_max - ccdpm_min)   &
                          * (1.0 - (sthu_soilt(l,1,1))))) ,0.0)
    burnt_carbon_rpm(l) = MAX(g_burn_gb(l) *                                  &
                          (cs(l,1,2) * (ccrpm_min + (ccrpm_max - ccrpm_min)   &
                          * (1.0 - (sthu_soilt(l,1,1))))) ,0.0)
    cs(l,1,1)     = cs(l,1,1) - burnt_carbon_dpm(l) / r_gamma   
    cs(l,1,2)     = cs(l,1,2) - burnt_carbon_rpm(l) / r_gamma  
    burnt_soil(l) = burnt_carbon_dpm(l) + burnt_carbon_rpm(l)  
  END DO
END IF
#endif

DO t = 1,trif_pts
  l = trif_index(t)

  !---------------------------------------------------------------------------
  ! Diagnose the gridbox mean soil-to-atmosphere respiration carbon flux
  ! [kg m-2 (360 days)-1]
  !---------------------------------------------------------------------------
  resp_s_to_atmos_gb(l,1) = (1.0 - resp_frac(l)) * resp_s(l,1,5)

  !---------------------------------------------------------------------------
  ! Diagnose the net local carbon flux into the soil
  !---------------------------------------------------------------------------
  pc(l,1) = dpm_ratio_gb(l) * lit_c_t(l) - resp_s(l,1,1)
  pc(l,2) = (1.0 - dpm_ratio_gb(l)) * lit_c_t(l) - resp_s(l,1,2)
  pc(l,3) = resp_frac_mult46(l) * resp_s(l,1,5) - resp_s(l,1,3)
  pc(l,4) = resp_frac_mult54(l) * resp_s(l,1,5) - resp_s(l,1,4)

  !---------------------------------------------------------------------------
  ! Variables required for the implicit and equilibrium calculations
  !---------------------------------------------------------------------------
  dpc_dcs(l,1) = resp_s(l,1,1) / cs(l,1,1)
  dpc_dcs(l,2) = resp_s(l,1,2) / cs(l,1,2)
  dpc_dcs(l,3) = resp_s(l,1,3) / cs(l,1,3)
  dpc_dcs(l,4) = resp_s(l,1,4) / cs(l,1,4)

  !---------------------------------------------------------------------------
  ! Save current value of soil carbon
  !---------------------------------------------------------------------------
  dcs(l,1) = cs(l,1,1)
  dcs(l,2) = cs(l,1,2)
  dcs(l,3) = cs(l,1,3)
  dcs(l,4) = cs(l,1,4)

END DO

!-----------------------------------------------------------------------------
! Update soil carbon
!-----------------------------------------------------------------------------
CALL decay (land_pts, trif_pts, trif_index, forw, r_gamma, dpc_dcs, pc,       &
            cs(:,1,:))

!-----------------------------------------------------------------------------
! Apply implicit correction to the soil respiration rate.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)

  dcs(l,1) = cs(l,1,1) - dcs(l,1)
  dcs(l,2) = cs(l,1,2) - dcs(l,2)
  dcs(l,3) = cs(l,1,3) - dcs(l,3)
  dcs(l,4) = cs(l,1,4) - dcs(l,4)

  resp_s(l,1,1) = resp_s(l,1,1) + forw * dpc_dcs(l,1) * dcs(l,1)
  resp_s(l,1,2) = resp_s(l,1,2) + forw * dpc_dcs(l,2) * dcs(l,2)
  resp_s(l,1,3) = resp_s(l,1,3) + forw * dpc_dcs(l,3) * dcs(l,3)
  resp_s(l,1,4) = resp_s(l,1,4) + forw * dpc_dcs(l,4) * dcs(l,4)

  implicit_resp_correction(l) = (r_gamma * SUM(dcs(l,1:4)) - SUM(pc(l,1:4)))  &
                                / r_gamma
END DO


! sum total respiration
DO t = 1,trif_pts
  l = trif_index(t)
  resp_s(l,1,5) = resp_s(l,1,1) + resp_s(l,1,2) +                             &
                  resp_s(l,1,3) + resp_s(l,1,4)

  resp_s_diag_gb(l,1,:)     = resp_s(l,1,:)
  resp_s_pot_diag_gb(l,1,:) = resp_s_pot(l,1,:)

  resp_s_pot(l,1,1) = resp_s(l,1,1)
  resp_s_pot(l,1,2) = resp_s(l,1,2)
  resp_s_pot(l,1,3) = resp_s(l,1,3)
  resp_s_pot(l,1,4) = resp_s(l,1,4)

  resp_s_pot(l,1,5) = resp_s_pot(l,1,1) + resp_s_pot(l,1,2) +                 &
                      resp_s_pot(l,1,3) + resp_s_pot(l,1,4)

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soilcarb

END MODULE soilcarb_mod
