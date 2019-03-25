#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Top-level control routine for vegetation section
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE veg_control_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG_CONTROL_MOD'

CONTAINS
SUBROUTINE veg_control(                                                       &
  land_pts, nsurft,                                                           &
  a_step, asteps_since_triffid,                                               &
  land_pts_trif, npft_trif,                                                   &
  phenol_period, triffid_period,                                              &
  l_phenol, l_triffid, l_trif_eq,                                             &
  atimestep, frac_disturb, frac_past, satcon_soilt_sfc,                       &
  g_leaf_acc, g_leaf_phen_acc, npp_acc,                                       &
  resp_s_acc_um, resp_w_acc,                                                  &
  cs_um, frac, lai, clay_soilt, z0m_soil, ht,                                 &
  catch_s, catch_t, infil_t, z0_t, z0h_t,                                     &
  c_veg, cv, lit_c, lit_c_mn, g_leaf_day, g_leaf_phen,                        &
  lai_phen, g_leaf_dr_out, npp_dr_out, resp_w_dr_out,                         &
  resp_s_dr_out_um                                                            &
   )

!Module imports

!Import relevant subroutines
USE veg1_mod, ONLY: veg1
USE veg2_mod, ONLY: veg2

!Common
USE jules_surface_types_mod
USE jules_vegetation_mod,     ONLY:                                         &
  i_veg_vn, i_veg_vn_1b, i_veg_vn_2b, frac_min
USE ancil_info,               ONLY:                                         &
  dim_cslayer, nsoilt

!Modules that change name between JULES and UM
#if defined(UM_JULES)
USE atm_step_local,           ONLY:                                         &
  dim_cs1
#else
USE ancil_info,               ONLY:                                         &
  dim_cs1
#endif

USE conversions_mod, ONLY: rsec_per_day

USE ereport_mod,              ONLY:                                         &
  ereport

USE timestep_mod,             ONLY:                                         &
  timestep

!Dr Hook
USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Description: Controls calling of the vegetation code.
!              In due course the final UM-isms will be removed to allow this
!              to be called from JULES-standalone control too.

INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
  nsurft,                                                                     &
  a_step,                                                                     &
  phenol_period,                                                              &
  triffid_period,                                                             &
  land_pts_trif,                                                              &
  npft_trif

REAL, INTENT(IN) ::                                                           &
  atimestep,                                                                  &
  frac_disturb(land_pts),                                                     &
  satcon_soilt_sfc(land_pts,nsoilt),                                          &
    ! Saturated hydraulic conductivity of the soil surface (kg/m2/s).
  z0m_soil(land_pts)

LOGICAL, INTENT(IN) ::                                                        &
  l_phenol, l_triffid, l_trif_eq

INTEGER, INTENT(INOUT) ::                                                     &
  asteps_since_triffid

REAL, INTENT(INOUT) ::                                                        &
  g_leaf_acc(land_pts,npft),                                                  &
  g_leaf_phen_acc(land_pts,npft),                                             &
  npp_acc(land_pts_trif,npft_trif),                                           &
  resp_s_acc_um(land_pts_trif,dim_cs1),                                       &
  resp_w_acc(land_pts_trif,npft_trif),                                        &
  cs_um(land_pts,dim_cs1),                                                    &
  frac(land_pts,ntype),                                                       &
  lai(land_pts,npft),                                                         &
  frac_past(land_pts),                                                        &
  ht(land_pts,npft),                                                          &
  clay_soilt(land_pts,nsoilt,dim_cslayer)

REAL, INTENT(OUT) ::                                                          &
  catch_s(land_pts,nsurft),                                                   &
  catch_t(land_pts,nsurft),                                                   &
  infil_t(land_pts,nsurft),                                                   &
  z0_t(land_pts,nsurft),                                                      &
  z0h_t(land_pts,nsurft),                                                     &
  c_veg(land_pts,npft),                                                       &
  cv(land_pts),                                                               &
  lit_c(land_pts,npft),                                                       &
  lit_c_mn(land_pts),                                                         &
  g_leaf_day(land_pts,npft),                                                  &
  g_leaf_phen(land_pts,npft),                                                 &
  lai_phen(land_pts,npft),                                                    &
  g_leaf_dr_out(land_pts,npft),                                               &
  npp_dr_out(land_pts,npft),                                                  &
  resp_w_dr_out(land_pts,npft),                                               &
  resp_s_dr_out_um(land_pts,dim_cs1+1)
REAL ::                                                                       &
  resp_s_dr_out(land_pts,1,dim_cs1+1),                                        &
  resp_s_acc_soilt(land_pts_trif,nsoilt,1,dim_cs1),                           &
  cs_pool_soilt(land_pts,nsoilt,1,dim_cs1)

INTEGER :: phenol_call, triffid_call, nstep_trif

INTEGER ::                                                                    &
  l, n,                                                                       &
    ! Indices.
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER :: trif_index(land_pts)
  ! Indices of land points on which TRIFFID may operate.

REAL :: frac_vs(land_pts)
  ! Fraction of gridbox covered by veg or soil.

CHARACTER(LEN=errormessagelength)       :: cmessage
INTEGER                  :: errorstatus

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VEG_CONTROL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-------------------------------------------------------------------------
!   If leaf phenology is activated, check whether the atmosphere model
!   has run an integer number of phenology calling periods.
!-------------------------------------------------------------------------
phenol_call = 1
IF (l_phenol) THEN
  phenol_call = MOD(REAL(a_step),                                             &
                    (REAL(phenol_period) *  (rsec_per_day / timestep)))
END IF

triffid_call = 1
IF (l_triffid) THEN
  nstep_trif = INT(rsec_per_day * triffid_period / timestep)
  IF (asteps_since_triffid == nstep_trif) THEN
    triffid_call = 0
  END IF
END IF

cs_pool_soilt(:,1,1,:)    = cs_um(:,:)
resp_s_acc_soilt(:,1,1,:) = resp_s_acc_um(:,:)
resp_s_dr_out(:,1,:)      = 0.0
!Can't copy from resp_s_dr_out_um as it's INTENT(OUT) so set to zero

!Call to veg 1 or 2 as appropriate
IF ((phenol_call == 0) .OR. (triffid_call == 0)) THEN
  SELECT CASE ( i_veg_vn )
  CASE ( i_veg_vn_2b )
    !---------------------------------------------------------------------
    ! Find total fraction of gridbox covered by vegetation and soil, and
    !  use this to set indices of land points on which TRIFFID may operate.
    !---------------------------------------------------------------------
    trif_pts = 0
    DO l = 1,land_pts
      frac_vs(l) = 0.0
      DO n = 1,nnpft
        frac_vs(l) = frac_vs(l) + frac(l,n)
      END DO
      frac_vs(l) = frac_vs(l) + frac(l,soil)
      IF ( frac_vs(l) >= REAL(nnpft) * frac_min ) THEN
        trif_pts = trif_pts + 1
        trif_index(trif_pts) = l
      END IF
    END DO
    ! Running with nsoilt > 1 is not compatible with dynamic vegetation, so we can
    ! hard-code the arguments appropriately.
    CALL veg2( land_pts, nsurft, a_step,                                      &
               phenol_period, triffid_period,                                 &
               trif_pts, trif_index, atimestep,                               &
               frac_disturb, frac_past, frac_vs,                              &
               satcon_soilt_sfc, clay_soilt(:,1,:), z0m_soil,                 &
               l_phenol, l_triffid, l_trif_eq,                                &
               asteps_since_triffid,                                          &
               g_leaf_acc, g_leaf_phen_acc, npp_acc,                          &
               resp_s_acc_soilt(:,1,1,:), resp_w_acc,                         &
               cs_pool_soilt(:,1,1,:), frac, lai, ht,                         &
               catch_s, catch_t, infil_t,                                     &
               z0_t, z0h_t, c_veg, cv,                                        &
               g_leaf_day, g_leaf_phen, g_leaf_dr_out,                        &
               lai_phen, lit_c, lit_c_mn, npp_dr_out,                         &
               resp_w_dr_out, resp_s_dr_out                                   &
               )

  CASE ( i_veg_vn_1b )
    CALL veg1( land_pts, nsurft, a_step, phenol_period, atimestep,            &
               satcon_soilt_sfc, z0m_soil, l_phenol,                          &
               g_leaf_acc, g_leaf_phen_acc, frac, lai, ht,                    &
               catch_s, catch_t, infil_t,                                     &
               g_leaf_day, g_leaf_phen,                                       &
               lai_phen, z0_t, z0h_t                                          &
               )

  CASE DEFAULT ! i_veg_vn
    errorstatus = 10
    WRITE (cmessage,'(A,A,I6)') 'Vegetation scheme version value',            &
           'i_veg_vn = ',i_veg_vn
    CALL Ereport ('VEG_CTL', errorstatus, cmessage)

  END SELECT ! i_veg_vn
END IF

cs_um(:,:)            = cs_pool_soilt(:,1,1,:)
resp_s_acc_um(:,:)    = resp_s_acc_soilt(:,1,1,:)
resp_s_dr_out_um(:,:) = resp_s_dr_out(:,1,:)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE veg_control
END MODULE veg_control_mod
#endif
