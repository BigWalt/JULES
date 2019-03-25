










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!  *** JULES version of science_fixes_mod ***

! Description:
!   This module declares 'short-term' temporary logicals used to protect
!   science bug fixes that lead to significant alterations in science results.
!   It is expected that these logicals will be short lived as the preference
!   should be for all configurations to use the corrected code. But
!   to maintain short term reproducibility of results across UM versions
!   the fixes are protected by logicals until the fixes become the default
!   in all model configurations and the logical is retired.

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

MODULE science_fixes_mod

IMPLICIT NONE

LOGICAL :: l_emis_ssi_full = .TRUE.

! Set this logical to .TRUE. to correct the updating of the surface
! temperature in the implicit solver.
LOGICAL :: l_dtcanfix = .TRUE.

! Fixes the calculation of surface exchange in coastal grid-boxes when
! coastal tiling is switched on. Should have no effect in standalone
! JULES, but default to TRUE anyway
LOGICAL :: l_fix_ctile_orog = .TRUE.

! Fixes how ustar is included in the exchange coefficient for dust deposition. 
! Has no effect in standalone JULES, but default to TRUE anyway
LOGICAL :: l_fix_ustar_dust = .TRUE.

! Fixes bug in ice thickness used in sea ice albedo calculation when 
! multilayer sea ice is used.
! Has no effect in standalone JULES, but default to TRUE anyway
LOGICAL :: l_fix_alb_ice_thick = .TRUE.

! Corrects the calculation of the albedo of snow in the two-stream
! scheme.
LOGICAL :: l_fix_albsnow_ts = .TRUE.

! Fixes a bug that means the unloading of snow from vegetation would
! potentially and incorrectly use a wind speed of zero.
! Has no effect in standalone JULES, but default to TRUE anyway.
LOGICAL :: l_fix_wind_snow = .TRUE.

END MODULE science_fixes_mod
