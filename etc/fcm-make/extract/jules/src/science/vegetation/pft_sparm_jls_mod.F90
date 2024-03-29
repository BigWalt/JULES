! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! SUBROUTINE pft_sparm
!
! Purpose:
! Routine to calculate the land surface parameters of a given PFT from
! its areal fraction and structural properties.
!
! Note that _pft variable name suffixes are not relevant as this routine
! works on a single tile (ipft)
!
! *********************************************************************
MODULE pft_sparm_mod

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PFT_SPARM_MOD'

CONTAINS

!#############################################################################

SUBROUTINE pft_sparm  (land_pts, ipft, surft_pts, surft_index,                &
                       canht, lai, catch,z0)

USE can_drag_mod,         ONLY: can_drag_z0
USE jules_vegetation_mod, ONLY: l_vegdrag_pft

USE pftparm,  ONLY: dz0v_dh, catch0, dcatch_dlai

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b, dz0v_dh_rp

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
    ! Number of land points.
  ipft,                                                                       &
    ! Plant functional type.
  surft_pts,                                                                  &
    ! Number of points which include the surface type.
  surft_index(land_pts)
    ! Indices of points which include the surface type.

REAL, INTENT(IN) ::                                                           &
  canht(land_pts),                                                            &
    ! Vegetation height (m).
  lai(land_pts)
    ! Leaf area index.

REAL, INTENT(OUT) ::                                                          &
  catch(land_pts),                                                            &
    ! Canopy capacity (kg/m2).
  z0(land_pts)
    ! Roughness length (m).

INTEGER ::                                                                    &
  j,l  ! Loop counters.

REAL ::                                                                       &
  z0h(land_pts),                                                              &
    ! Roughness length for scalars
  zdt(land_pts),                                                              &
    ! Difference between canopy height and displacement height
  array_zero(land_pts)
    ! Dummy array

REAL, PARAMETER ::                                                            &
  z0_soil = 3.0e-4  ! Roughness length for soil (m).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PFT_SPARM'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_rp2 .AND. i_rp_scheme == i_rp2b ) THEN
  dz0v_dh(ipft) = dz0v_dh_rp(ipft)
END IF

IF (l_vegdrag_pft(ipft)) THEN
  array_zero(:) = 0.0
  CALL can_drag_z0(land_pts, surft_pts, surft_index,                          &
                   array_zero, canht, lai,                                    &
                   z0, z0h, zdt)
ELSE
  DO j = 1,surft_pts
    l = surft_index(j)
    z0(l) = dz0v_dh(ipft) * canht(l)
  END DO
END IF

DO j = 1,surft_pts
  l = surft_index(j)

  IF ( z0(l) < z0_soil ) THEN
    z0(l) = z0_soil
  END IF

  catch(l) = catch0(ipft) + dcatch_dlai(ipft) * lai(l)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE pft_sparm
END MODULE pft_sparm_mod
