! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sea_rough_int_mod

USE phi_m_h_mod, ONLY: phi_m_h
 
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SEA_ROUGH_INT_MOD'

CONTAINS
!   SUBROUTINE SEA_ROUGH_INT ------------------------------------------

!  Purpose: Calculate the roughness lengths at the sea surface if
!           this is being done interactively within the iteration
!           for the Obukhov length.


!  Documentation: UM Documentation Paper No 24

!--------------------------------------------------------------------
SUBROUTINE sea_rough_int (                                                    &
 points,surft_pts,surft_index,pts_index,                                      &
 charnock,v_s,recip_l_mo,                                                     &
 z0m,z0h                                                                      &
)

USE planet_constants_mod, ONLY:                                               &
                    g, vkman

USE jules_sea_seaice_mod, ONLY:                                              &
                    iseasurfalg,                                              &
                    ip_ss_surf_div_int,                                       &
                    ip_ss_coare_mq,                                           &
                    a_chrn_coare,                                             &
                    b_chrn_coare,                                             &
                    u10_min_coare,                                            &
                    u10_max_coare,                                            &
                    l_10m_neut,                                               &
                    z_10m

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                        &
 points                                                                       &
                      ! Number of points.
,surft_pts                                                                    &
                      ! Number of tile points.
,surft_index(points)                                                          &
                      ! Index of tile points.
,pts_index(points)    ! Index of sea points.

REAL, INTENT(IN) ::                                                           &
  charnock                                                                    &
!                    ! Prescribed value of Charnock's coefficient
,v_s(points)                                                                  &
                     ! Surface layer scaling velocity
,recip_l_mo(points)
!                    ! Reciprocal of the Obukhov length ! (m^-1).


REAL, INTENT(OUT) ::                                                          &
 z0m(points)                                                                  &
               ! Roughness length for momentum transport (m).
,z0h(points)
               ! Roughness length for heat and moisture (m).

!----------------------------------------------------------------------

!  Local variables

INTEGER ::                                                                    &
  k,l ! Loop counter; horizontal field index.


REAL ::                                                                       &
 charnock_int                                                                 &
              ! Interactive value of Charnock's coefficient
,m10                                                                          &
              ! Wind speed at 10m
,re_rough                                                                     &
              ! Roughness Reynolds number
,phi_m_10(points)                                                             &
              ! Value of Phi_m at 10m
,phi_h_10(points)
              ! Value of Phi_h at 10m

REAL, PARAMETER :: visc_air = 1.4e-5
              ! Kinematic viscosity of air

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SEA_ROUGH_INT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


SELECT CASE (iseasurfalg)
  !
CASE (ip_ss_surf_div_int)
  !   Constant value of Charnock's cofficient and interactive
  !   calculation of z0h from surface divergence theory.
  DO k = 1,surft_pts
    l = surft_index(k)
    z0m(l) = charnock * v_s(l)**2 / g +                                     &
             1.54e-6 / (v_s(l) + 1.0e-5)
    z0h(l)   = MAX(7.0e-8, 2.56e-9 / z0m(l))
    IF (v_s(l) < 0.1) z0h(l)   = MAX(z0h(l), 2.52e-6 / v_s(l))
  END DO
  !
CASE (ip_ss_coare_mq)
  !
  !   Recalculate Charnock's coefficient from the wind-speed at
  !   10 m. Either the true neutral wind may be used (perferred)
  !   or the stability-dependent 10m wind for cases when the
  !   distinction is ignored.
  IF ( .NOT. l_10m_neut) THEN
    CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                  &
                   recip_l_mo,SPREAD(z_10m,1,points),                       &
                   SPREAD(z_10m,1,points),z0m,z0h,                          &
                   phi_m_10,phi_h_10)
  END IF
  DO k = 1,surft_pts
    l = surft_index(k)
    IF (l_10m_neut) THEN
      m10 = (v_s(l) / vkman) * LOG(1.0 + z_10m / z0m(l))
    ELSE
      m10 = (v_s(l) / vkman) * phi_m_10(l)
    END IF
    charnock_int = b_chrn_coare + a_chrn_coare *                            &
      MAX(MIN(m10, u10_max_coare), u10_min_coare)
    z0m(l) = charnock_int * v_s(l)**2 / g +                                 &
             0.11 * visc_air / v_s(l)
    !     Impose a lower limit of the molecular mean free path and
    !     an upper limit of 0.015 m based on observations indicating
    !     a saturation or decline in the drag around 35ms-1. (In the
    !     aerodynamically smooth limit U10 would need to fall below
    !     0.003 ms-1 to be limited  by this.)
    z0m(l) = MIN(MAX(z0m(l), 7.0e-8), 0.015)
    re_rough = v_s(l) * z0m(l) / visc_air
    !     Roughness lengths for heat and momentum set to the expression
    !     for z0q from COARE3.0 and 3.5, as latent heat should dominate.
    z0h(l)   = MIN(1.15e-4, 5.5e-5 / re_rough**0.6)
    !
    !
  END DO
  !
END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sea_rough_int
END MODULE sea_rough_int_mod
