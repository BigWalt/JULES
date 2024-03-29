#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Calls routines to initialize veg parameters and accumulated C fluxes
!
! Subroutine Interface
MODULE init_veg_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_VEG_MOD'

CONTAINS
SUBROUTINE init_veg(a_step, triffid_period_arg, nstep_since_triffid)

!Use in relevant subroutines
USE init_min_mod,             ONLY: init_min
USE init_acc_mod,             ONLY: init_acc
USE sparm_mod,                ONLY: sparm
USE infiltration_rate_mod,    ONLY: infiltration_rate
USE tilepts_mod,              ONLY: tilepts

!USE in JULES modules
USE jules_surface_types_mod,  ONLY: npft, ntype, nnpft
USE jules_vegetation_mod,     ONLY: l_nrun_mid_trif, l_triffid, l_phenol,     &
                                    triffid_period, l_trif_init_accum
                                    
!USE in UM modules

!Names of some variables changed to be consistent with usage around JULES
USE nlsizes_namelist_mod,     ONLY: nsurft            => ntiles,              &
                                    land_pts          => land_field

USE submodel_mod,             ONLY: atmos_im
USE conversions_mod,          ONLY: rsec_per_day
USE model_time_mod,           ONLY: secs_per_stepim
USE atm_fields_mod,           ONLY: frac_surft        => frac_typ,            &
                                    z0m_soil_gb       => z0m_soil,            &
                                    catch_snow_surft  => catch_snow,          &
                                    catch_surft       => catch_tile,          &
                                    z0_surft          => z0_tile,             &
                                    z0h_bare_surft    => z0h_tile,            &
                                    satcon_gb         => sat_soil_cond,       &
                                    infil_surft       => infil_tile,          &
                                    soil_carb1,                               &
                                    canht_pft, lai_pft,                       &
                                    npp_pft_acc, g_phlf_pft_acc,              &
                                    rsp_w_pft_acc, rsp_s_acc1, g_lf_pft_acc

USE trif_vars_mod,            ONLY: lai_bal_pft
USE trif,                     ONLY: lai_min
USE pftparm,                  ONLY: a_ws, eta_sl, a_wl, b_wl

USE umPrintMgr,               ONLY: umPrint, umMessage
USE yomhook,                  ONLY: lhook, dr_hook
USE parkind1,                 ONLY: jprb, jpim

IMPLICIT NONE

!
! Description:
!   Initializes vegetation parameters from fractions of surface types
!   and initializes accumulated carbon fluxes to zero if a new TRIFFID
!   calling period is starting.
!
! Method:
!   Calls routine SPARM to initialize vegetation parameters.
!   Calls routine INIT_ACC to initialize accumulated carbon fluxes.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Vegetation
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.
!

!Arguments
INTEGER, INTENT(IN)     :: a_step  !Current timestep in atmosphere model
INTEGER, INTENT(INOUT)  :: triffid_period_arg
INTEGER, INTENT(INOUT)  :: nstep_since_triffid

! #include "typduma.h"

!Local variables
INTEGER :: surft_pts(ntype)
  !Number of land points which include the nth surface type
INTEGER :: surft_index(land_pts,ntype)
  !Indices of land points which include the nth surface type
INTEGER :: nstep_trif
  !Number of atmospheric timesteps between calls to TRIFFID.
INTEGER :: l,n  ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_VEG'

!-----------------------------------------------------------------------
! If TRIFFID on, call INIT_MIN to ensure PFT fractions are GE minimum
! fraction except where vegetation excluded by ice, water or urban
!-----------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (l_triffid) THEN
  CALL init_min(land_pts,frac_surft,soil_carb1)
  
  IF ( .NOT. l_phenol) THEN
    ! If l_phenol=F, then lai is not prognostic, so needs to be initialised
    lai_bal_pft(:,:) = 0.0
    DO l = 1,land_pts
      DO n = 1,nnpft 
        IF (frac_surft(l,n) > 0.0) THEN        
          lai_bal_pft(l,n) = (a_ws(n) * eta_sl(n) * canht_pft(l,n)                           &
                            / a_wl(n))** (1.0 / (b_wl(n) - 1.0) )
          lai_pft(l,n) = lai_bal_pft(l,n) 
        ELSE  
          lai_pft(l,n) = lai_min(n) 
        END IF
      END DO
    END DO
  END IF
END IF

!-----------------------------------------------------------------------
! Call TILEPTS to initialise TILE_PTS and TILE_INDEX
!-----------------------------------------------------------------------
CALL tilepts(land_pts,frac_surft,surft_pts,surft_index)

!-----------------------------------------------------------------------
! Initialise tiled and gridbox mean vegetation parameters
!-----------------------------------------------------------------------
CALL sparm (land_pts, nsurft, surft_pts, surft_index,                         &
            frac_surft, canht_pft, lai_pft, z0m_soil_gb,                      &
            catch_snow_surft, catch_surft, z0_surft, z0h_bare_surft)

CALL infiltration_rate(land_pts, nsurft, surft_pts, surft_index,              &
                       satcon_gb, frac_surft, infil_surft)
  
IF (l_triffid) THEN
  !-----------------------------------------------------------------------
  ! If this is an NRUN and re-start from mid-way through a TRIFFID calling
  ! period has not been requested: (i) initialise accumulation prognostics
  ! to zero, (ii) set TRIFFID_PERIOD in integer header, and
  ! (iii) initialise ASTEPS_SINCE_TRIFFID integer header to zero.
  ! If mid-period restart is requested then leave the accumulated fields
  ! unchanged, and if a new calling period is specified then reset
  ! calling period header the new value provided that the number of
  ! atmosphere timesteps since the last call to TRIFFID does not exceed
  ! the new calling period .
  !
  ! If, additionally, an NRUN needs to bit-compare with a CRUN then it is
  ! required to keep these accumulated variables, so we skip the call to 
  ! INIT_ACC.
  !-----------------------------------------------------------------------
  IF (a_step == 0) THEN
    IF (l_nrun_mid_trif) THEN

      IF (triffid_period /= triffid_period_arg) THEN
        nstep_trif = INT(rsec_per_day * triffid_period / secs_per_stepim(atmos_im))
        IF (nstep_since_triffid >  nstep_trif) THEN
          WRITE(umMessage,*) '**ERROR IN TRIFFID** YOU HAVE SELECTED TO'
          CALL umPrint(umMessage,src='init_veg')
          WRITE(umMessage,*) 'START MID-WAY THROUGH A TRIFFID CALLING'
          CALL umPrint(umMessage,src='init_veg')
          WRITE(umMessage,*) 'PERIOD BUT YOUR INITIAL DUMP CONTAINS'
          CALL umPrint(umMessage,src='init_veg')
          WRITE(umMessage,*) 'PROGNOSTICS ACCUMULATED OVER A PERIOD'
          CALL umPrint(umMessage,src='init_veg')
          WRITE(umMessage,*) 'LONGER THAN THE NEW CALLING PERIOD'
          CALL umPrint(umMessage,src='init_veg')
        ELSE
          triffid_period_arg = triffid_period
        END IF
      END IF

    ELSE

      IF (l_trif_init_accum) THEN
        CALL init_acc(land_pts, npp_pft_acc, g_phlf_pft_acc,               &
                      rsp_w_pft_acc, rsp_s_acc1)
      END IF

      triffid_period_arg = triffid_period
      nstep_since_triffid = 0

    END IF
  END IF
END IF

IF (l_phenol) THEN
  ! Initialise accumulated leaf turnover rate to zero
  g_lf_pft_acc(:,:) = 0.0
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_veg
END MODULE init_veg_mod
#endif
