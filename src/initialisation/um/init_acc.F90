#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Initialises accumulated carbon fluxes to zero if new calling period
!
MODULE init_acc_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_ACC_MOD'

CONTAINS
! Subroutine Interface:
SUBROUTINE init_acc(land_pts,                                                 &
       npp_pft_acc,g_leaf_phen_pft_acc,                                       &
       resp_w_pft_acc,resp_s_acc)


USE jules_surface_types_mod

USE umPrintMgr, ONLY: umPrint, umMessage

USE yomhook,    ONLY: lhook, dr_hook
USE parkind1,   ONLY: jprb, jpim

IMPLICIT NONE

!
! Description:
!   Resets accumulation prognostics to zero if a new TRIFFID calling
!   period is starting.  This routine is needed when starting an NRUN
!   from an initial dump created in either of the following situations:
!
!   i)  Initial dump created from a non-TRIFFID run
!
!   ii) Initial dump created in a TRIFFID run mid-way through a TRIFFID
!       calling period.  The NRUN may re-start at the same point within
!       this calling period and continue with the accumulation already
!       part-completed in this dump; in this case this routine will not
!       be used.  Alternatively, the NRUN may start a new calling
!       period, in which case the accumulation must begin; this routine
!       allows this by re-setting the relevant prognostics to zero.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Vegetation
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.
!
! Arguments
INTEGER, INTENT(IN) :: land_pts !number of land points

REAL, INTENT(INOUT) :: npp_pft_acc(land_pts,npft)
  !Accumulated NPP on PFTs
REAL, INTENT(INOUT) :: g_leaf_phen_pft_acc(land_pts,npft)
  !Accum. phenological leaf turnover rate PFTs
REAL, INTENT(INOUT) :: resp_w_pft_acc(land_pts,npft)
  !Accumulated wood respiration on PFTs
REAL, INTENT(INOUT) :: resp_s_acc(land_pts,4)

!Local variables
INTEGER ::l,n

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_ACC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
WRITE(umMessage,*)                                                            &
 'INIT_ACC: setting accumulation prognostics to zero'
CALL umPrint(umMessage,src='init_acc')

DO l = 1,land_pts
  DO n = 1,npft
    npp_pft_acc(l,n)          = 0.0
    g_leaf_phen_pft_acc(l,n)  = 0.0
    resp_w_pft_acc(l,n)       = 0.0
  END DO
END DO

DO n = 1,4
  DO l = 1,land_pts
    resp_s_acc(l,n)           = 0.0
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_acc
END MODULE init_acc_mod
#endif
