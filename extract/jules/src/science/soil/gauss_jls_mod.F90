! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE GAUSS--------------------------------------------------

! Description:
!     Solves a tridiagnonal matrix equation of the form:

!             A(n) X(n-1) + B(n) X(n) + C(n) X(n+1) = D(n)

!     by Gausian elimination, assuming boundary conditions:

!             A(1) = 0.0    at the top
!             C(N) = 0.0    at the bottom.

MODULE gauss_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GAUSS_MOD'

CONTAINS

SUBROUTINE gauss (nlevs, npnts, soil_pts, soil_index, a, b, c, d, xmin, xmax, &
                  x, use_lims)

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                        &
  use_lims

INTEGER, INTENT(IN) ::                                                        &
  nlevs,                                                                      &
    ! Number of levels.
  npnts,                                                                      &
    ! Number of gridpoints.
  soil_pts,                                                                   &
    ! Number of soil points.
  soil_index(npnts)
    ! Array of soil points.

REAL, INTENT(IN) ::                                                           &
  a(npnts,nlevs),                                                             &
    ! Matrix elements corresponding to the coefficients of X(n-1).
  b(npnts,nlevs),                                                             &
    ! Matrix elements corresponding to the coefficients of X(n).
  c(npnts,nlevs),                                                             &
    ! Matrix elements corresponding to the coefficients of X(n+1).
  d(npnts,nlevs),                                                             &
    ! Matrix elements corresponding to the RHS of the equation.
  xmin(npnts,nlevs),                                                          &
    ! Minimum permitted value of X.
  xmax(npnts,nlevs)
    ! Maximum permitted value of X.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL, INTENT(OUT) ::                                                          &
  x(npnts,nlevs)
    ! Solution.

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  i, j, n
    ! Loop counters.

REAL ::                                                                       &
  adash(npnts,nlevs), bdash(npnts,nlevs), ddash(npnts,nlevs)
    ! Transformed matrix elements

INTEGER :: errcount, errcode   ! error reporting
CHARACTER(LEN=80) :: errmsg

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='GAUSS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! By default set the implicit increment to the explicit increment
! (for when denominators vanish).
!-----------------------------------------------------------------------------
DO n = 1,nlevs
  !CDIR NODEP
  DO j = 1,soil_pts
    i = soil_index(j)
    x(i,n) = d(i,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Upward Sweep: eliminate "C" elements by replacing nth equation with:
!                  B'(n+1)*Eq(n)-C(n)*Eq'(n+1)
! where "'" denotes a previously tranformed equation. The resulting
! equations take the form:
!                A'(n) X(n-1) + B'(n) X(n) = D'(n)
! (NB. The bottom boundary condition implies that the NLEV equation does
!  not need transforming.)
!-----------------------------------------------------------------------------
!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)
  adash(i,nlevs) = a(i,nlevs)
  bdash(i,nlevs) = b(i,nlevs)
  ddash(i,nlevs) = d(i,nlevs)
END DO

DO n = nlevs - 1,1,-1
  !CDIR NODEP
  DO j = 1,soil_pts
    i = soil_index(j)
    adash(i,n) = bdash(i,n+1) * a(i,n)
    bdash(i,n) = bdash(i,n+1) * b(i,n) - c(i,n) * adash(i,n+1)
    ddash(i,n) = bdash(i,n+1) * d(i,n) - c(i,n) * ddash(i,n+1)
  END DO
END DO

!-----------------------------------------------------------------------------
! Top boundary condition: A(1) = 0.0 , allows X(1) to be diagnosed
!-----------------------------------------------------------------------------
errcount = 0
!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)
  IF (bdash(i,1) /= 0.0) THEN
    x(i,1) = ddash(i,1) / bdash(i,1)
  ELSE
    errcount = errcount + 1   ! occurence count
  END IF
  IF (use_lims) THEN
    x(i,1) = MAX(x(i,1),xmin(i,1))
    x(i,1) = MIN(x(i,1),xmax(i,1))
  END IF
END DO

! Output any error(s).
IF (errcount > 0) THEN
  errcode = -1   ! problems with first layer
  WRITE (errmsg,*) errcount, ' divide by zero warnings at layer 1'
  !        CALL ereport( SubName, ErrCode, ErrMsg )
END IF

!-----------------------------------------------------------------------------
! Downward Sweep: calculate X(n) from X(n-1):
!                X(n) = (D'(n) - A'(n) X(n-1)) / B'(n)
!-----------------------------------------------------------------------------
DO n = 2,nlevs
  errcount = 0
  !CDIR NODEP
  DO j = 1,soil_pts
    i = soil_index(j)
    IF (bdash(i,n) /= 0.0) THEN
      x(i,n)=(ddash(i,n) - adash(i,n) * x(i,n-1)) / bdash(i,n)
    ELSE
      errcount = errcount + 1
    END IF
    IF (use_lims) THEN
      x(i,n) = MAX(x(i,n),xmin(i,n))
      x(i,n) = MIN(x(i,n),xmax(i,n))
    END IF
  END DO

  !output any error(s)
  IF (errcount > 0) THEN
    errcode = -2   ! problems with other layers
    WRITE (errmsg,*) errcount, ' divide by zero warnings at layer', n
    !          CALL ereport( SubName, ErrCode, ErrMsg )
  END IF

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE gauss
END MODULE gauss_mod
