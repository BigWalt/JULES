










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE lotka_noeq_subset_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LOTKA_NOEQ_SUBSET_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine LOTKA -----------------------------------------------------------
!
! Purpose : Updates fractional coverage of each functional type.
!           Based on the Lotka-Volterra equations of interspecies
!           competition.
!
!-----------------------------------------------------------------------------
SUBROUTINE lotka_noeq_subset (land_pts, trif_pts, nsub, crop_lotka,           &
                              trif_index, r_gamma,                            &
                              c_veg, frac_agric, frac_agr_prev, ht, pc_s,     &
                              frac, dfrac, frac_na, dfrac_na)

USE jules_surface_types_mod, ONLY:                                            &
  ! imported scalars
  npft, nnpft, ntype, soil

USE trif, ONLY: crop, g_area
USE jules_vegetation_mod, ONLY: frac_min, frac_seed, pow

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                        &
  land_pts,                                                                   &
    ! Total number of land points.
  trif_pts,                                                                   &
    ! Number of points on which TRIFFID may operate.
  nsub,                                                                       &
    ! Number of PFTs to use.
  crop_lotka
    ! Switch to use crop PFTs (=1) or non-crop PFTs (not = 1).

INTEGER, INTENT(IN) ::                                                        &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL, INTENT(IN) ::                                                           &
  r_gamma
    ! Inverse timestep (/360days).

REAL, INTENT(IN) ::                                                           &
  c_veg(land_pts,npft),                                                       &
    ! Carbon content of vegetation (kg C/m2).
  frac_agric(land_pts),                                                       &
  frac_agr_prev(land_pts),                                                    &
    ! Fraction of agriculture.
  ht(land_pts,npft) ,                                                         &
    ! Canopy height (m)
  pc_s(land_pts,npft)                                                          
    ! Net carbon flux available for spreading (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL, INTENT(INOUT) ::                                                        &
  frac(land_pts,ntype),                                                       &
    ! Fractional cover of each Functional Type.
  dfrac(land_pts,npft),                                                       &
    ! Increment to the areal fraction
    ! during the timestep.
  frac_na(land_pts,ntype),                                                    &
    ! Fractional cover of each Functional Type.
  dfrac_na(land_pts,npft)                                                      
    ! Increment to the areal fraction during the timestep,
    ! prior to the land use change.

!-----------------------------------------------------------------------------
! Local integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                    &
 k,l,m,n,o,t,i,j     ! Loop counters.

INTEGER ::                                                                    &
  dom(land_pts,nsub),                                                         &
    ! Dominance hierachy.
  pft_idx(nsub)                                                                
    ! Index of the PFTs to use (either crop or non-crop PFTS).

!-----------------------------------------------------------------------------
! Local real variables.
!-----------------------------------------------------------------------------
REAL ::                                                                       &
  fracn,                                                                      &
    ! Fractions used in the spreading calculation.
  tallest
    ! Height of the tallest PFT from temp_ht (m).

REAL ::                                                                       &
  com(land_pts,nsub,nsub),                                                    &
    ! Coefficients representing the influence of one type (second argument) on
    ! another (first argument).
  nosoil(land_pts),                                                           &
    ! Fractional area not available to vegetation.
  space(land_pts,nsub),                                                       &
    ! Space available for invasion.
  spacemax(land_pts),                                                         &
    ! Total space in the grid cell.
  temp_ht(nsub)
    ! Store height for assigning dominance (m).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LOTKA_NOEQ_SUBSET'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Define the subset of PFTs to use
!!----------------------------------------------------------------------------
m = 1
DO n = 1,npft
  IF ( crop(n) == crop_lotka ) THEN
    pft_idx(m) = n
    m = m + 1
  END IF
END DO

!-----------------------------------------------------------------------------
! Take working copy of frac.
!-----------------------------------------------------------------------------
DO n = 1,nsub
  o = pft_idx(n)
  DO l = 1,land_pts
    frac_na(l,o) = frac(l,o)
  END DO
END DO

!-----------------------------------------------------------------------------
! Define competition coefficients and the dominance hierachy
!-----------------------------------------------------------------------------
DO n = 1,nsub
  DO m = 1,nsub
    DO t = 1,trif_pts
      l = trif_index(t)
      com(l,n,m) = 1.0
    END DO
  END DO
END DO

!NEW: COM(I,J) BASED ON HEIGHT OF PFT I & J
DO t = 1,trif_pts
  l = trif_index(t)

  !DOM IS THE DOMINANCE HIERARCHY BASED ON HEIGHTS
  !WARNING: If two or more PFTs have exactly the same height,
  !the first PFT in the list will get the higher dominance.

  DO n = 1,nsub
    o = pft_idx(n)
    temp_ht(n) = ht(l,o)
  END DO

  DO i = 1,nsub
    tallest = MAXVAL(temp_ht)
    DO j = 1,nsub
      IF (ABS(temp_ht(j) - tallest) < EPSILON(1.0)) THEN
        dom(l,i)   = j
        temp_ht(j) = -999.0
        EXIT
      END IF
    END DO
  END DO

  DO j = 1,nsub
    n = dom(l,j)
    com(l,n,n) = 1.0
    IF (j < nsub) THEN
      DO k = j+1,nsub
        m = dom(l,k)
        com(l,m,n) = 1.0
        com(l,n,m) = 0.0
      END DO
    END IF
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the space available for expansion of vegetation.
! Loop over non-veg types, assuming only the soil type can contribute space.
!-----------------------------------------------------------------------------
nosoil(:) = 0.0
DO n = npft+1, ntype
  IF ( n == soil ) CYCLE
  DO t = 1,trif_pts
    l = trif_index(t)
    nosoil(l) = nosoil(l) + frac(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Exclude non-crop types from agricultural regions
!-----------------------------------------------------------------------------
DO k = 1,nsub
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,k)
    IF ( crop_lotka >= 1 ) THEN
      space(l,n) = frac_agric(l) - frac_min * REAL(nsub - k)
    ELSE
      space(l,n) = 1.0 - nosoil(l) - frac_agric(l) - frac_min * REAL(nsub - k)
    END IF
  END DO
END DO

DO n = 1,nsub
  DO m = 1,nsub
    o = pft_idx(m)
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - (com(l,n,m) * frac(l,o))
    END DO
  END DO
END DO

!-----------------------------------------------------------------------------
! We are removing the implicit timestepping approach to equilibrium.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
! Update the areal fractions (Compete code now here:)
!-----------------------------------------------------------------------------
spacemax(:) = 0.0
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,nsub
    o = pft_idx(n)
    dfrac(l,o) = 0.0
  END DO
  IF ( crop_lotka >= 1 ) THEN
    spacemax(l) = frac_agric(l) - frac_min * REAL(nsub-1)
  ELSE
    spacemax(l) = 1.0 - nosoil(l) - frac_agric(l) - frac_min * REAL(nsub-1)
  END IF
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nsub
    n = dom(l,j)
    o = pft_idx(n)
    fracn      = frac(l,o)
    fracn      = MAX(fracn,frac_seed)
    dfrac(l,o) = (pc_s(l,o) * space(l,n) / c_veg(l,o) - g_area(o))            &
                 / (r_gamma / fracn)
    frac(l,o)  = frac(l,o) + dfrac(l,o)

    IF (frac(l,o) <  frac_min) THEN
      dfrac(l,o) = dfrac(l,o) + (frac_min - frac(l,o))
      frac(l,o)  = frac_min
    ELSE IF (frac(l,o) >  (spacemax(l))) THEN
      IF ((spacemax(l)) <  frac_min ) THEN
        frac(l,o)  = frac_min
        dfrac(l,o) = frac(l,o) - fracn
      ELSE
        dfrac(l,o) = dfrac(l,o) + spacemax(l) - frac(l,o)
        frac(l,o)  = spacemax(l)
      END IF
    END IF

    spacemax(l) = spacemax(l) - frac(l,o) + frac_min

  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac(l,soil) = 1.0 - nosoil(l)
  DO n = 1,nnpft
    frac(l,soil) = frac(l,soil) - frac(l,n)
  END DO
  IF (frac(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac(l,soil) = frac_min
  END IF
END DO

!-----------------------------------------------------------------------------
! For the dynamic land use we require a double call to COMPETE
! in order to be able to clearly diagnose what change is caused by the
! land use change.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Exclude non-crop types from agricultural regions
!-----------------------------------------------------------------------------
DO k = 1,nsub
  DO t = 1,trif_pts
    l = trif_index(t)
    n = dom(l,k)
    IF ( crop_lotka >= 1 ) THEN
      space(l,n) = frac_agr_prev(l) - frac_min * REAL(nsub - k)
    ELSE
      space(l,n) = 1.0 - nosoil(l) - frac_agr_prev(l)                         &
                                   - frac_min * REAL(nsub - k)
    END IF
  END DO
END DO

DO n = 1,nsub
  DO m = 1,nsub
    o = pft_idx(m)
    DO t = 1,trif_pts
      l = trif_index(t)
      space(l,n) = space(l,n) - (com(l,n,m) * frac_na(l,o))
    END DO
  END DO
END DO

!SECOND CALL TO COMPETE TO ACCOUNT FOR CHANGES FROM LU
!-----------------------------------------------------------------------------
! We are removing the implicit timestepping approach to equilibrium.
! Divide the update equation by FRAC to eliminate the (unstable)
! bare soil solution.
!-----------------------------------------------------------------------------
! Update the areal fractions (Compete code now here:)
!-----------------------------------------------------------------------------
spacemax(:) = 0.0
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,nsub
    o = pft_idx(n)
    dfrac_na(l,o) = 0.0
  END DO
  IF ( crop_lotka >= 1 ) THEN
    spacemax(l) = frac_agr_prev(l) - frac_min * REAL(nsub-1)
  ELSE
    spacemax(l) = 1.0 - nosoil(l) - frac_agr_prev(l) - frac_min * REAL(nsub-1)
  END IF
END DO

DO t = 1,trif_pts
  l = trif_index(t)
  DO j = 1,nsub
    n = dom(l,j)
    o = pft_idx(n)
    fracn = frac_na(l,o)
    fracn = MAX(fracn,frac_seed)

    dfrac_na(l,o) = (pc_s(l,o) * space(l,n) / c_veg(l,o) - g_area(o))         &
                     / (r_gamma / fracn)
    frac_na(l,o)  = frac_na(l,o) + dfrac_na(l,o)

    IF (frac_na(l,o) < frac_min) THEN
      dfrac_na(l,o) = dfrac_na(l,o) + (frac_min - frac_na(l,o))
      frac_na(l,o)  = frac_min
    ELSE IF (frac_na(l,o) > (spacemax(l))) THEN
      IF ( spacemax(l) <  frac_min ) THEN
        frac_na(l,o)  = frac_min
        dfrac_na(l,o) = frac_na(l,o) - fracn
      ELSE
        dfrac_na(l,o) = dfrac_na(l,o) + spacemax(l) - frac_na(l,o)
        frac_na(l,o)  = spacemax(l)
      END IF
    END IF

    spacemax(l) = spacemax(l) - frac_na(l,o) + frac_min

  END DO
END DO

!-----------------------------------------------------------------------------
! Diagnose the new bare soil fraction
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  frac_na(l,soil) = 1.0 - nosoil(l)
  DO n = 1,nnpft
    frac_na(l,soil) = frac_na(l,soil) - frac_na(l,n)
  END DO
  IF (frac_na(l,soil) < frac_min) THEN !AVOID NEGATIVE SOIL FRACTIONS
    frac_na(l,soil) = frac_min
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE lotka_noeq_subset

END MODULE lotka_noeq_subset_mod

