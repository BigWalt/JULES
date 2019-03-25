#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: river routing

MODULE init_riv_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_RIV_MOD'

CONTAINS
SUBROUTINE init_riv(nstep_since_riv, icode, cmessage)

! Purpose: To initialise variables for river routing. This will be
! extended later.
! Method: Sets up variables in the dump

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE atm_fields_bounds_mod
USE UM_ParVars
USE Control_Max_Sizes
USE Submodel_Mod

USE nlsizes_namelist_mod, ONLY:                                         &
    a_len1_coldepc, a_len1_flddepc, a_len1_levdepc, a_len1_rowdepc,     &
    a_len2_coldepc, a_len2_flddepc, a_len2_levdepc, a_len2_lookup,      &
    a_len2_rowdepc, a_len_cfi1, a_len_cfi2, a_len_cfi3, a_len_extcnst,  &
    a_len_inthd, a_len_realhd, len1_lookup,                             &
    len_dumphist, len_fixhd, len_tot, model_levels, mpp_len1_lookup,    &
    n_cca_lev, n_obj_d1_max, sm_levels, st_levels, tpps_ozone_levels,   &
    tr_lbc_ukca, tr_lbc_vars, tr_ukca, tr_vars

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.
!   This code is written to UMDP3 v6 programming standards.
!
INTEGER :: icode                 ! OUT Internal return code
CHARACTER(LEN=errormessagelength) :: cmessage
                              ! OUT Internal error message

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_RIV'

INTEGER, INTENT(OUT) :: nstep_since_riv

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in, &
                        zhook_handle)
icode = 0
cmessage=""
nstep_since_riv = 0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out, &
                        zhook_handle)
RETURN
END SUBROUTINE init_riv
END MODULE init_riv_mod
#endif
