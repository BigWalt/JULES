! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE givelen_mod

USE givelat_mod,  ONLY: givelat
USE givelon_mod,  ONLY: givelon
USE giverade_mod, ONLY: giverade

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GIVELEN_MOD'

CONTAINS

REAL FUNCTION givelen(rx1, ry1, rx2, ry2)

!     give the length (km) between (rx1, ry1) to (rx2, ry2)
!     sphere approximation is applied.
!     see page 621 of Rika-Nenpyo (1995)
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing

IMPLICIT NONE

REAL :: rlat, dx, dy, re
REAL :: rx1, rx2, ry1, ry2, dlon

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='GIVELEN'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

dlon = ABS(rx1 - rx2)
IF (dlon >= 180.0) dlon = ABS(360.0 - dlon)

IF (rx1 == rx2) THEN
  rlat = (ry1 + ry2) / 2.0
  givelen = givelat(rlat) * ABS(ry1 - ry2)
ELSE IF (ry1 == ry2) THEN
  rlat = ry1
  givelen = givelon(rlat) * dlon

ELSE
  rlat = (ry1 + ry2) / 2.0
  re = giverade(rlat)
  dx = givelon(rlat) * dlon / re
  dy = givelat(rlat) * ABS(ry1 - ry2) / re
  givelen = ACOS(COS(dx) * COS(dy)) * re
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END FUNCTION givelen
END MODULE givelen_mod
