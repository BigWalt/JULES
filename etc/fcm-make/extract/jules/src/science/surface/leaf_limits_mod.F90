! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE leaf_limits_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LEAF_LIMITS_MOD'

CONTAINS
! *********************************************************************
! Purpose:
! Calculates the leaf resistance and net photosynthesis using:
!  (i) Collatz et al. (1992) C3 photosynthesis model
! (ii) Jacobs (1994) CI/CA closure.
! *********************************************************************
SUBROUTINE leaf_limits(land_field,veg_pts,veg_index,ft                        &
,                      nleaf,dq,apar,tl,ca,oa,pstar,fsmc                      &
,                      clos_pts,open_pts,clos_index,open_index                &
,                      ci,rd,wcarb,wexpt,wlite,                               &
                       denom, tau, qtenf_term, kc, ko)

! nleaf has replaced nl

USE pftparm
USE jules_surface_mod
USE jules_vegetation_mod, ONLY: l_trait_phys

! Need to work out how to set this
!      REAL                                                              &
!     & FD(NPFT)                                                         &
!                              ! Dark respiration coefficient.
!     &,NEFF(NPFT)             ! Constant relating VCMAX and leaf N

!      DATA FD      /   0.015,  0.015,   0.015,     0.025, 0.015 /
!      DATA NEFF    /  0.8e-3 , 0.8e-3 , 0.8e-3 ,  0.4e-3, 0.8e-3  /

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

!  (mol/sec) / (watts) conversion for PAR:
REAL, PARAMETER      ::  conpar = 2.19e5

INTEGER, INTENT(IN) ::                                                        &
 land_field                                                                   &
                            ! IN Total number of land points.
,veg_pts                                                                      &
                            ! IN Number of vegetated points.
,veg_index(land_field)                                                        &
                            ! IN Index of vegetated points
                            !    on the land grid.
,ft                         ! IN Plant functional type.

INTEGER, INTENT(OUT) ::                                                       &
 clos_index(land_field)                                                       &
                            ! OUT Index of land points
                            !     with closed stomata.
,clos_pts                                                                     &
                            ! OUT Number of land points
                            !     with closed stomata.
,open_index(land_field)                                                       &
                            ! OUT Index of land points
                            !     with open stomata.
,open_pts                   ! OUT Number of land points
!                                 !     with open stomata.

REAL, INTENT(IN) ::                                                           &
 nleaf(land_field)                                                            &
                            ! IN Leaf nitrogen concentration
                            ! If l_trait_phys = (kg N/m2).
                            ! else = kgN/KgC, ABH
,dq(land_field)                                                               &
                            ! IN Canopy level specific humidity
!                                 !    deficit (kg H2O/kg air).
,apar(land_field)                                                             &
                            ! IN Absorbed PAR (W/m2)
,tl(land_field)                                                               &
                            ! IN Leaf temperature (K).
,ca(land_field)                                                               &
                            ! IN Canopy CO2 pressure (Pa).
,oa(land_field)                                                               &
                            ! IN Atmospheric O2 pressure (Pa).
,pstar(land_field)                                                            &
                            ! IN Atmospheric pressure (Pa).
,fsmc(land_field)
                            ! IN Soil water factor.


REAL, INTENT(OUT) ::                                                          &
 ci(land_field)                                                               &
                            ! OUT Internal CO2 pressure (Pa).
,rd(land_field)                                                               &
                            ! OUT Dark respiration (mol CO2/m2/s).
,wcarb(land_field)                                                            &
                            ! OUT Carboxylation, ...
,wlite(land_field)                                                            &
                            !     ... Light, and ...
,wexpt(land_field)
                            !     ... export limited gross ...
!                                 !     ... photosynthetic rates ...
!                                 !     ... (mol CO2/m2/s).


!Variables that can be pre-calculated outside of the layer and iteration loops
REAL, INTENT(IN) :: denom(land_field) ! Denominator in equation for VCM
  != (1 + EXP (0.3 * (tdegc(l) - tupp(ft)))) *
  !   (1 + EXP (0.3 * (tlow(ft) - tdegc(l))))

REAL, INTENT(IN) :: tau(land_field)   ! CO2/O2 specificity ratio.
  !tau(l)   = 2600.0 * (0.57 ** (0.1 * (tdegc(l) - 25.0)))

REAL, INTENT(IN) :: qtenf_term(land_field)
  !qtenf_term(l) = (q10_leaf(ft) ** (0.1 * (tdegc(l) - 25.0)))

REAL, INTENT(IN) :: kc(land_field) ! Michaelis constant for CO2 (Pa)
  !kc(l)    = 30.0 * (2.1 ** (0.1 * (tdegc(l) - 25.0)))

REAL, INTENT(IN) :: ko(land_field) ! Michaelis constant for O2 (Pa)
  !ko(l)    = 30000.0 * (1.2 ** (0.1 * (tdegc(l) - 25.0)))

REAL ::                                                                       &
 acr                                                                          &
                            ! WORK Absorbed PAR
!                                 !      (mol photons/m2/s).
,ccp(land_field)                                                              &
                            ! WORK Photorespiratory compensatory
!                                 !      point (Pa).
,qtenf                                                                        &
                            ! WORK Q10 function.
,vcm(land_field)                                                              &
                            ! WORK Maximum rate of carboxylation
!                                 !      of Rubisco (mol CO2/m2/s).
,vcmax                      ! WORK Maximum rate of carboxylation
!                                 !      of Rubisco - without the
!                                 !      temperature factor
!                                 !      (mol CO2/m2/s).

LOGICAL :: l_open(land_field)  !Logical to mark open points to help parallel
                            !performance

INTEGER ::                                                                    &
 j,l                        ! WORK Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LEAF_LIMITS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                             &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(j,l,qtenf,vcmax)                                                &
!$OMP SHARED(veg_pts,veg_index,l_trait_phys,vsl,nleaf,vint,ft,neff,           &
!$OMP        ccp,oa,tau,c3,qtenf_term,vcm,denom,rd,fd,ci,ca,f0,dq,dqcrit,     &
!$OMP        l_open,fsmc,apar)
DO j = 1,veg_pts
  l = veg_index(j)
  ! Calculate the photosynthetic parameters
  IF (l_trait_phys) THEN
    vcmax = (vsl(ft) * nleaf(l) + vint(ft)) * 1.0e-6  ! Kattge 2009
  ELSE
    vcmax = neff(ft) * nleaf(l)
  END IF

  ! TAU is the Rubisco specificity for CO2 relative to O2. The numbers
  ! in this equation are from Cox, HCTN 24, "Description ... Vegetation
  ! Model", equation 53.
  ccp(l) = 0.5 * oa(l) / tau(l) * REAL(c3(ft))

  ! q10_leaf pft-dependent
  qtenf  = vcmax * qtenf_term(l)
  vcm(l) = qtenf / denom(l)  ! Cox, HCTN 24, equation 49.
  rd(l)  = fd(ft) * vcm(l)

  ! Calculate the internal CO2 pressure (Jacobs, 1994).
  ci(l) = (ca(l) - ccp(l)) * f0(ft) * (1 - dq(l) / dqcrit(ft)) + ccp(l)

  IF (fsmc(l) == 0.0 .OR. dq(l) >= dqcrit(ft) .OR. apar(l) == 0.0) THEN
    l_open(l) = .TRUE.
  ELSE
    l_open(l) = .FALSE.
  END IF

END DO
!$OMP END PARALLEL DO

!Isolate the piece of work that won't go easily into OpenMP
clos_pts = 0
open_pts = 0
DO j = 1,veg_pts
  l = veg_index(j)
  !   IF (fsmc(l)==0.0 .OR. dq(l) >= dqcrit(ft) .OR. apar(l)==0.0) THEN
  IF ( l_open(l)) THEN
    clos_pts = clos_pts + 1
    clos_index(clos_pts) = j
  ELSE
    open_pts = open_pts + 1
    open_index(open_pts) = j
  END IF
END DO

! Calculate the gross photosynthesis for RuBP-Carboxylase, Light and
! Export limited photosynthesis (Collatz et al., 1992).
IF (c3(ft) == 1) THEN

!$OMP PARALLEL DO IF(open_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(l,j,acr)                                                        &
!$OMP SHARED(open_pts,veg_index,open_index,wcarb,vcm,ci,ccp,kc,oa,ko,wlite,   &
!$OMP        ft,wexpt,fwe_c3,alpha,apar)
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    ! Convert absorbed PAR into mol PAR photons/m2/s
    acr  = apar(l) / conpar

    ! The numbers
    ! in these 2 equations are from Cox, HCTN 24, "Description ... Vegetation
    ! Model", equations 54 and 55.
    wcarb(l) = vcm(l) * (ci(l) - ccp(l)) / (ci(l) + kc(l) * (1.0 + oa(l) / ko(l)))
    wlite(l) = alpha(ft) * acr * (ci(l) - ccp(l)) / (ci(l) + 2.0 * ccp(l))
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    wexpt(l) = fwe_c3 * vcm(l)
  END DO
!$OMP END PARALLEL DO

ELSE

!$OMP PARALLEL DO IF(open_pts > 1)                                            &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(l,j,acr)                                                        &
!$OMP SHARED(open_pts,veg_index,open_index,wcarb,vcm,wlite,ft,wexpt,pstar,    &
!$OMP        alpha,fwe_c3,fwe_c4,ci,apar)
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    ! Convert absorbed PAR into mol PAR photons/m2/s
    acr  = apar(l) / conpar

    wcarb(l) = vcm(l)
    wlite(l) = alpha(ft) * acr
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    wexpt(l) = fwe_c4 * vcm(l) * ci(l) / pstar(l)
  END DO
!$OMP END PARALLEL DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE leaf_limits
END MODULE leaf_limits_mod
