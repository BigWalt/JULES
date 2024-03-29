! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE cancap_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CANCAP_MOD'

CONTAINS

! Subroutine CANCAP ------------------------------------------------

! Purpose : Calculate the heat capacity of a given PFT from its LAI

! -----------------------------------------------------------------
SUBROUTINE cancap (land_pts,veg_pts,veg_index,can_model,ft                    &
,                  ht,lai,canhc,vfrac)

USE trif
USE pftparm
USE jules_vegetation_mod, ONLY: l_trait_phys
USE jules_surface_mod, ONLY: hleaf,hwood,cmass
USE jules_surface_types_mod, ONLY: nnpft
USE crop_vars_mod, ONLY: dvi_cpft
USE crop_utils_mod, ONLY:                                                    &
   leafc_from_prognostics, stemc_from_prognostics

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                       &
land_pts                                                                     &
                           ! IN Total number of land points.
,veg_pts                                                                      &
                           ! IN Number of vegetated points.
,veg_index(land_pts)                                                          &
                           ! IN Index of vegetated points.
,can_model                  ! IN Swith for thermal vegetation
!                                 !    canopy

INTEGER, INTENT(IN) ::                                                        &
 ft                         ! IN Plant functional type.

REAL, INTENT(IN) ::                                                           &
 ht(land_pts)                                                                 &
                            ! IN Vegetation height (m).
,lai(land_pts)
                            ! IN Leaf area index.
REAL, INTENT(OUT) ::                                                          &
 canhc(land_pts)                                                              &
                            ! OUT Areal heat capacity of
!                                 !     vegetation canopy (J/K/m2).
,vfrac(land_pts)            ! OUT Fractional canopy coverage.

REAL ::                                                                       &
 lai_bal(land_pts)                                                            &
                            ! WORK Leaf area index in balanced
!                                 !      growth state.
,leaf                                                                         &
                            ! WORK Leaf biomass (kg C/m2).
,wood                                                                         &
                            ! WORK Woody biomass (kg C/m2).
,sla
                            ! WORK specific leaf area

INTEGER ::                                                                    &
 j,l                        ! WORK Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CANCAP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(j,l,leaf,wood)                                                  &
!$OMP SHARED(veg_pts,veg_index,canhc,vfrac,can_model,kext,lai,dvi_cpft,ht,    &
!$OMP        lai_bal,l_trait_phys,ft,nnpft,a_ws,eta_sl,a_wl,b_wl,lma,sigl,    &
!$OMP        hleaf,hwood)

!$OMP DO SCHEDULE(STATIC)
DO j = 1,veg_pts
  l = veg_index(j)
  canhc(l) = 0.0
  vfrac(l) = 0.0
END DO
!$OMP END DO

IF (can_model  ==  2) THEN
  !     Radiative canopy without heat capacity

!$OMP DO SCHEDULE(STATIC)
  DO j = 1,veg_pts
    l = veg_index(j)
    canhc(l) = 0.0
    vfrac(l) = 1.0 - EXP(-kext(ft) * lai(l))
  END DO
!$OMP END DO

ELSE IF (can_model == 3 .OR. can_model == 4) THEN
  !     Radiative canopy with heat capacity
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,veg_pts
    l = veg_index(j)

    IF ( ft > nnpft ) THEN
      leaf = leafc_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft), lai(l))
      wood = stemc_from_prognostics(ft - nnpft, ht(l) )
    ELSE
      lai_bal(l) = ( a_ws(ft) * eta_sl(ft) * ht(l) /                          &
                     a_wl(ft) )**(1.0 / (b_wl(ft) - 1))
      ! lma replaces sigl
      IF ( l_trait_phys ) THEN
        leaf = cmass * lma(ft) * lai_bal(l)
        !kgC/m2 = kgC/kgleaf * gleaf/m2 * kg/g
      ELSE
        leaf = sigl(ft) * lai_bal(l)      !leaf=kgC/m2
      END IF
      wood = a_wl(ft) * (lai_bal(l)**b_wl(ft))
    END IF

    canhc(l) = hleaf * leaf + hwood * wood
    vfrac(l) = 1.0 - EXP(-kext(ft) * lai(l))
  END DO
!$OMP END DO NOWAIT
END IF
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE cancap
END MODULE cancap_mod
