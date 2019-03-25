#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE init_urban_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_URBAN_MOD'

CONTAINS
SUBROUTINE init_urban ()
! Description:
!   Routine to set and check two-tile urban logic. MacDonald 1998
!   calculations for urban roughness length and displacement height now in
!   UM reconfiguration. UM D1 array prognostics copied to JULES module.
!
!  6.1   10/01/07   First written. Peter Clark and Aurore Porson
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land

USE urban_param,         ONLY:                                              &
   hgt_gb, hwr_gb, wrr_gb, disp_gb, ztm_gb, albwl_gb, albrd_gb,             &
   emisw_gb, emisr_gb

USE atm_fields_real_mod, ONLY:                                              &
   hgt, hwr, wrr, disp, ztm, albwl, albrd, emisw, emisr

USE nlsizes_namelist_mod, ONLY: land_pts => land_field

USE jules_surface_mod, ONLY: l_urban2t

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE jules_print_mgr, ONLY:                                                 &
   jules_message,                                                          &
   jules_print,                                                            &
   PrNorm

IMPLICIT NONE

INTEGER :: l

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_URBAN'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! .NOT. l_moruses
! Currently only really need wrr for anthrop heat scaling, but they are all in
! the dump and set currently for l_urban2t. Should probably be removed from the
! dump unless l_moruses, but for now pass to avoid dump confusion i.e. if
! MORUSES is run from a seemingly MORUSES dump, which turns out to be urban-2t
! dump with unset fields.
! Prognostics are not currently updated, but some may be in the future to
! account for snow and are thus copied back again the the D1 array in
! atmos_physics2. Any changes made here should be duplicated there too.
IF ( l_urban2t ) THEN
  DO l = 1, land_pts
    wrr_gb(l)   = wrr(l)
    hwr_gb(l)   = hwr(l)
    hgt_gb(l)   = hgt(l)
    disp_gb(l)  = disp(l)
    ztm_gb(l)   = ztm(l)
    albwl_gb(l) = albwl(l)
    albrd_gb(l) = albrd(l)
    emisw_gb(l) = emisw(l)
    emisr_gb(l) = emisr(l)
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_urban
END MODULE init_urban_mod
#endif
