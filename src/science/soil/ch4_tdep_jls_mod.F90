! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CH4_TDEP-----------------------------------------------

! Description:
!     Calculates methane emissions from wetland area.

MODULE ch4_tdep_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CH4_TDEP_MOD'

CONTAINS

SUBROUTINE ch4_tdep(npnts, soil_pts, soil_index, tsoil_d, cs_eff, resp_s_tot, &
                    npp, f_wetl, t0_ch4, const_tdep_cs, const_tdep_npp,       &
                    const_tdep_resps, fch4_wetl_cs, fch4_wetl_npp,            &
                    fch4_wetl_resps)

USE water_constants_mod,  ONLY:                                               &
  tm

USE jules_soil_biogeochem_mod, ONLY:                                          &
   const_ch4_cs, const_ch4_npp, const_ch4_resps

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  npnts,                                                                      &
    ! Number of gridpoints.
  soil_pts,                                                                   &
    ! Number of soil points.
  soil_index(npnts)                                                          
    ! Array of soil points.

REAL, INTENT(IN) ::                                                           &
  tsoil_d(npnts),                                                             &
    ! Diagnosed soil temp to 1 metre (K).
  cs_eff(npnts),                                                              &
    ! Effective soil carbon (kg C/m2).
  resp_s_tot(npnts),                                                          &
    ! Soil respiration total (kg C/m2/s).
  npp(npnts),                                                                 &
    ! Gridbox mean net primary productivity (kg C/m2/s).
  f_wetl(npnts),                                                              &
    ! Wetland fraction
  t0_ch4,                                                                     &
    ! T0 value (zero celsius in kelvin)
  const_tdep_cs,                                                              &
    ! T and Q10(0) dependent function
  const_tdep_npp,                                                             &
    ! T and Q10(0) dependent function
  const_tdep_resps
    ! T and Q10(0) dependent function

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT):
!-----------------------------------------------------------------------------
REAL, INTENT(INOUT) ::                                                        &
  fch4_wetl_cs(npnts),                                                        &
    ! Scaled methane flux (soil carbon substrate) (kg C/m2/s).
  fch4_wetl_npp(npnts),                                                       &
    ! Scaled methane flux (npp substrate) (kg C/m2/s).
  fch4_wetl_resps(npnts)
    ! Scaled methane flux (soil respiration substrate) (kg C/m2/s).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
  i, j

REAL ::                                                                       &
  q10t_ch4_cs,                                                                &
    ! Q10 value at T
  q10t_ch4_npp,                                                               &
    ! Q10 value at T
  q10t_ch4_resps
    ! Q10 value at Tcs

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CH4_TDEP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate an effective soil carbon for wetland methane emission.
!-----------------------------------------------------------------------------

DO j = 1,soil_pts
  i = soil_index(j)
  IF ( tsoil_d(i) > tm .AND. f_wetl(i) > 0.0 ) THEN

    q10t_ch4_cs    = EXP( const_tdep_cs / tsoil_d(i) )
    q10t_ch4_npp   = EXP( const_tdep_npp / tsoil_d(i) )
    q10t_ch4_resps = EXP( const_tdep_resps / tsoil_d(i) )

    fch4_wetl_cs(i) = const_ch4_cs * cs_eff(i) * f_wetl(i) *                  &
                      q10t_ch4_cs ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    IF ( npp(i) > 0.0 ) THEN
      fch4_wetl_npp(i) = const_ch4_npp * npp(i) * f_wetl(i) *                 &
                         q10t_ch4_npp ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    END IF
    IF ( resp_s_tot(i) > 0.0 ) THEN
      fch4_wetl_resps(i) = const_ch4_resps * resp_s_tot(i) * f_wetl(i) *      &
                           q10t_ch4_resps ** ( 0.1 * (tsoil_d(i) - t0_ch4) )
    END IF

  END IF  !  tsoil_d and f_wetl
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ch4_tdep
END MODULE ch4_tdep_mod
