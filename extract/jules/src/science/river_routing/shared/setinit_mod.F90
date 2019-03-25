!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE setinit_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SETINIT_MOD'

CONTAINS

SUBROUTINE setinit(nx, ny, area, rinit, sto, jmax)


USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     assume water equivalent to rinit [mm] over 1 grid box is stored
!     as in the initial stage
!     [mm] x [m^2] = [10^(-3) m^3] ===> [kg]
!
INTEGER :: nx, ny, i, j, jmax
REAL :: area(nx, ny), sto(nx, ny), rinit

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SETINIT'

!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,          &
                        zhook_handle)
DO j = 1, jmax
  DO i = 1, nx
    sto(i,j) = rinit * area(i,j)
  END DO
END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,         &
                        zhook_handle)
RETURN
END SUBROUTINE setinit
END MODULE setinit_mod
