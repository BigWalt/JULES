! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sf_stom_mod

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_STOM_MOD'

CONTAINS

! *********************************************************************
! Routine to calculate the bulk stomatal resistance and the canopy
! CO2 fluxes

! *********************************************************************

SUBROUTINE sf_stom  (land_pts,land_index                                      &
,                    veg_pts,veg_index                                        &
,                    ft,co2,co2_3d,co2_dim_len                                &
,                    co2_dim_row,l_co2_interactive                            &
,                    fsmc,ht,ipar,lai                                         &
,                    canht,pstar                                              &
,                    q1,ra,tstar,o3                                           &
,                    can_rad_mod,ilayers,faparv                               &
,                    gpp,npp,resp_p,resp_l,resp_r,resp_w                      &
,                    n_leaf,n_root,n_stem,lai_bal,gc                          &
,                    fapar_sun,fapar_shd,fsun                                 &
,                    flux_o3,fo3,fapar_diag                                   &
,                    isoprene,terpene,methanol,acetone                        &
,                    open_index,open_pts)

USE leaf_mod, ONLY: leaf
USE leaf_limits_mod, ONLY: leaf_limits
USE bvoc_emissions_mod, ONLY: bvoc_emissions

USE conversions_mod, ONLY: zerodegc
USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY: t_i_length

USE jules_surface_types_mod, ONLY: nnpft

USE jules_vegetation_mod, ONLY:                                              &
! imported scalars that are not changed
    l_bvoc_emis, l_fapar_diag, l_trait_phys, l_stem_resp_fix, l_o3_damage,    &
    l_scale_resp_pm

USE CN_utils_mod, ONLY:                                                      &
! imported procedures
    get_can_ave_fac, nleaf_from_lai

USE pftparm, ONLY:                                                           &
! imported arrays that are not changed
    a_wl,a_ws,b_wl,eta_sl,kpar,nl0,nr_nl,ns_nl,omega,r_grow,                  &
    sigl, glmin, lma, nmass, kn, knl, tupp, tlow, q10_leaf,                   &
    nsw, nr, hw_sw

USE ccarbon, ONLY:                                                           &
! imported scalar parameters
   epco2,epo2

USE c_rmol, ONLY: rmol

USE jules_surface_mod, ONLY:                                                 &
! imported scalar parameters
   iter,o2,cmass

USE water_constants_mod, ONLY: lc

USE planet_constants_mod, ONLY: rair=>r

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE crop_vars_mod, ONLY: dvi_cpft, rootc_cpft
USE crop_utils_mod, ONLY:                                                     &
   leafc_from_prognostics, stemc_from_prognostics,                            &
   lma_from_prognostics
USE cropparm, ONLY: cfrac_l

USE qsat_mod, ONLY: qsat_new => qsat,                                     &
                    l_new_qsat_jules

IMPLICIT NONE


INTEGER, INTENT(IN) ::                                                        &
 land_pts                                                                     &
                            ! IN Number of land points to be
!                                 !    processed.
,land_index(land_pts)                                                         &
                            ! IN Index of land points on the
!                                 !    P-grid.
,veg_pts                                                                      &
                            ! IN Number of vegetated points.
,veg_index(land_pts)                                                          &
                            ! IN Index of vegetated points
!                                 !    on the land grid.
,co2_dim_len                                                                  &
                            ! IN Length of a CO2 field row.
,co2_dim_row                ! IN Number of CO2 field rows.

INTEGER, INTENT(IN) ::                                                        &
 ft                         ! IN Plant functional type.

LOGICAL, INTENT(IN) :: l_co2_interactive   ! switch for 3D CO2 field

INTEGER, INTENT(IN) ::                                                        &
  can_rad_mod                                                                 &
!                           !Switch for canopy radiation model
 ,ilayers
!                           !No of layers in canopy radiation model

REAL, INTENT(IN) ::                                                           &
 co2                                                                          &
                            ! IN Atmospheric CO2 concentration
,co2_3d(co2_dim_len,co2_dim_row)                                              &
!                                 ! IN 3D atmos CO2 concentration
!                                 !    (kg CO2/kg air).
,fsmc(land_pts)                                                               &
                            ! IN Soil water factor.
,ht(land_pts)                                                                 &
                            ! IN Canopy height (m).
,ipar(land_pts)                                                               &
                            ! IN Incident PAR (W/m2).
,lai(land_pts)                                                                &
                            ! IN Leaf area index.
,canht(land_pts)                                                              &
                            ! IN Canopy Height
,pstar(land_pts)                                                              &
                            ! IN Surface pressure (Pa).
,faparv(land_pts,ilayers)                                                     &
                            ! IN Profile of absorbed PAR.
,fapar_shd(land_pts,ilayers)                                                  &
                            ! IN Profile of absorbed DIFF_PAR.
,fapar_sun(land_pts,ilayers)                                                  &
                            ! IN Profile of absorbed DIR_PAR.
,fsun(land_pts,ilayers)                                                       &
                            ! IN fraction of sunlit leaves
,q1(land_pts)                                                                 &
                            ! IN Specific humidity at level 1
,ra(land_pts)                                                                 &
                            ! IN Aerodynamic resistance (s/m).
,tstar(land_pts)                                                              &
                            ! IN Surface temperature (K).
,o3(land_pts)
                            ! IN Surface ozone concentration (ppb).

REAL, INTENT(OUT) ::                                                          &
 gpp(land_pts)                                                                &
                            ! OUT Gross Primary Productivity
!                                 !     (kg C/m2/s).
,npp(land_pts)                                                                &
                            ! OUT Net Primary Productivity
!                                 !     (kg C/m2/s).
,resp_p(land_pts)                                                             &
                            ! OUT Plant respiration rate
!                                 !     (kg C/m2/sec).
,resp_r(land_pts)                                                             &
                            ! OUT Root respiration rate
!                                 !     (kg C/m2/sec).
,resp_l(land_pts)                                                             &
                            ! OUT Leaf maintanence respiration rate
!                                 !     (kg C/m2/sec).
,resp_w(land_pts)                                                             &
                            ! OUT Wood respiration rate
!                                 !     (kg C/m2/sec).
,flux_o3(land_pts)                                                            &
                            ! OUT Flux of O3 to stomata (nmol O3/m2/s).
,fo3(land_pts)
                            ! OUT Ozone exposure factor.

REAL, INTENT(INOUT) ::                                                        &
 gc(land_pts)                                                                 &
                            ! INOUT Canopy resistance to H2O
!                                 !       (m/s).
,fapar_diag(land_pts)
                            ! OUT FAPAR diagnostic

! BVOC variables
REAL, INTENT(OUT) ::                                                          &
 isoprene(land_pts)                                                           &
                   ! OUT Isoprene Emission Flux (kgC/m2/s)
,terpene(land_pts)                                                            &
                   ! OUT (Mono-)Terpene Emission Flux (kgC/m2/s)
,methanol(land_pts)                                                           &
                   ! OUT Methanol Emission Flux (kgC/m2/s)
,acetone(land_pts)
                   ! OUT Acetone Emission Flux (kgC/m2/s)

INTEGER, INTENT(OUT) ::                                                       &
open_index(land_pts)                                                          &
                            ! OUT Index of land points
!                                 !      with open stomata.
,open_pts                   ! OUT Number of land points
!                                 !      with open stomata.

REAL ::                                                                       &
 anetc(land_pts)                                                              &
                            ! WORK Net canopy photosynthesis
!                                 !     (mol CO2/m2/s).
,apar_crit(land_pts)                                                          &
                            ! WORK Critical APAR below which
!                                 !      light is limiting (W/m2)
,co2c(land_pts)                                                               &
                            ! WORK Canopy level CO2 concentration
!                                 !      (kg CO2/kg air).
,ci(land_pts)                                                                 &
                            ! WORK Internal CO2 pressure (Pa).
,dq(land_pts)                                                                 &
                            ! WORK Specific humidity deficit
!                                 !      (kg H2O/kg air).
,dqc(land_pts)                                                                &
                            ! WORK Canopy level specific humidity
!                                 !      deficit (kg H2O/kg air).
,fpar(land_pts)                                                               &
                            ! WORK PAR absorption factor.
,lai_bal(land_pts)                                                            &
                            ! WORK Leaf area index in balanced
!                                 !      growth state.
,nleaf_top(land_pts)                                                          &
                            ! WORK Nitrogen concentration of top leaf.
!                           ! if(l_trait_phys)= g N/m2
                            ! else= kg N/kg C
,n_leaf(land_pts)                                                             &
                            ! WORK Nitrogen contents of the leaf,
,n_root(land_pts)                                                             &
                            !      root,
,n_stem(land_pts)                                                             &
                            !      and stem (kg N/m2).
,qs(land_pts)                                                                 &
                            ! WORK Saturated specific humidity
!                                 !      (kg H2O/kg air).
,ra_rc(land_pts)                                                              &
                            ! WORK Ratio of aerodynamic resistance
!                                 !      to canopy resistance.
,rdc(land_pts)                                                                &
                            ! WORK Canopy dark respiration,
!                                 !      without soil water dependence
!                                 !      (mol CO2/m2/s).
,rdmean(land_pts)                                                             &
                            ! WORK Mean dark respiration
!                                 !      per unit leaf area
!                                 !      over canopy
!                                 !      without soil water dependence
!                                 !   (mol CO2/s per m2 leaf area).
,nlmean(land_pts)                                                             &
                            ! WORK Mean nitrogen per unit leaf area
!                                 !      over canopy
!                                 !      without soil water dependence
!                                 !      (kg N per m2 leaf area).
!                                 !      Used in place of n_leaf when LAI
!                                 !      is very small to avoid blowing up
!                                 !      the calculation of respiration.
,resp_p_g(land_pts)                                                           &
                            ! WORK Plant growth respiration rate
!                                 !      (kg C/m2/sec).
,resp_p_m(land_pts)                                                           &
                            ! WORK Plant maintenance respiration
!                                 !      rate (kg C/m2/sec).
,root(land_pts)                                                               &
                            ! WORK Root carbon (kg C/m2).
,faparv_layer(land_pts,ilayers)                                               &
                            ! WORK absorbed par(layers)
,flux_o3_l(land_pts)                                                          &
                            ! WORK Flux of O3 to stomata (nmol O3/m2/s).
,flux_o3_l_sun(land_pts)                                                      &
                            ! WORK Flux of O3 to stomata
                            !      for sunlit leaves
!                             !      (for can_rad_mod=5)
                            !      (nmol O3/m2/s).
,flux_o3_l_shd(land_pts)                                                      &
                            ! WORK Flux of O3 to stomata
                            !      for shaded leaves
                            !      (for can_rad_mod=5)
                            !      (nmol O3/m2/s).
,fo3_l(land_pts)                                                              &
                            ! WORK Ozone exposure factor.
,fo3_l_sun(land_pts)                                                          &
                            ! WORK Ozone exposure factor
                            !      for sunlit leaves
                            !      (for can_rad_mod=5)
,fo3_l_shd(land_pts)                                                          &
                            ! WORK Ozone exposure factor
                            !      for shaded leaves
                            !      (for can_rad_mod=5)
,o3mol(land_pts)                                                              &
                            ! WORK Surface ozone concentration (moles).
,fsmc_scale(land_pts)                                                         &
                            ! WORK Scaling of stem and root plant maintenance
                            ! respiration.
,fstem                                                                        &
                            ! WORK Ratio of Respiring stem wood to Total Wood
,stem_resp_scaling
                            ! WORK Scaling factor to reduce stem respiration


!Variables that can be pre-calculated outside of the layer and iteration loops
REAL :: denom(land_pts)
!denom(l) = (1 + EXP (0.3 * (tdegc(l) - tupp(ft)))) *
!              (1 + EXP (0.3 * (tlow(ft) - tdegc(l))))
REAL :: tau(land_pts)
!tau(l)   = 2600.0 * (0.57 ** (0.1 * (tdegc(l) - 25.0)))
REAL :: qtenf_term(land_pts)
!qtenf_term(l) = (q10_leaf(ft) ** (0.1 * (tdegc(l) - 25.0)))
REAL :: kc(land_pts)
!kc(l)    = 30.0 * (2.1 ** (0.1 * (tdegc(l) - 25.0)))
REAL :: ko(land_pts)
!ko(l)    = 30000.0 * (1.2 ** (0.1 * (tdegc(l) - 25.0)))

INTEGER ::                                                                    &
 i,j,k,l,m,n                  ! WORK Loop counters.


!-----------------------------------------------------------------------
! Parameters
!-----------------------------------------------------------------------
REAL, PARAMETER      :: cconu = 12.0e-3
                            ! kg C in 1 mol CO2
!  (mol/sec) / (watts) conversion for PAR:
REAL, PARAMETER      ::  conpar = 2.19e5


REAL ::                                                                       &
 anetl(land_pts)                                                              &
                            ! WORK Net leaf photosynthesis
!                                 !      (mol CO2/m2/s/LAI).
,anetl_sun(land_pts)                                                          &
!                                 ! WORK Net leaf photosynthesis of
!                                 !      sunlit leaves
!                                 !      (mol CO2/m2/s/LAI)
,anetl_shd(land_pts)                                                          &
!                                 ! WORK Net leaf photosynthesis of
!                                 !      shaded leaves
!                                 !      (mol CO2/m2/s/LAI
,apar(land_pts)                                                               &
                            ! WORK PAR absorbed by the top leaf
!                                 !      (W/m2).
,acr(land_pts)                                                                &
                            ! WORK Absorbed PAR
!                                 !      (mol photons/m2/s).
,ca(land_pts)                                                                 &
                            ! WORK Canopy level CO2 pressure
!                                 !      (Pa).
,gl(land_pts)                                                                 &
                            ! WORK Leaf conductance for H2O
!                                 !      (m/s).
,gl_sun(land_pts)                                                             &
                            ! WORK Leaf conductance for H2O of
!                                 !      sunlit leaves (m/s).
,gl_shd(land_pts)                                                             &
                            ! WORK Leaf conductance for H2O of
!                                 !      shaded leaves (m/s).
,oa(land_pts)                                                                 &
                            ! WORK Atmospheric O2 pressure
!                                 !      (Pa).
,rd(land_pts)                                                                 &
                            ! WORK Dark respiration of top leaf
!                                 !      (mol CO2/m2/s).
,rd_sun(land_pts)                                                             &
                            ! WORK Dark respiration of sunlit leaves
!                                 !      (mol CO2/m2/s).
,rd_shd(land_pts)                                                             &
                            ! WORK Dark respiration of shaded leaves
!                                 !      (mol CO2/m2/s).

,wcarb(land_pts)                                                              &
                            ! WORK Carboxylation, ...
,wlite(land_pts)                                                              &
                            !      ... Light, and ...
,wexpt(land_pts)                                                              &
                            !      ... export limited gross ...
!                                 !      ... photosynthetic rates ...
!                                 !      ... (mol CO2/m2/s).
,wlitev(land_pts)                                                             &
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for each layer
!                                 !      (mol CO2/m2/s).
,wlitev_sun(land_pts)                                                         &
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for sunlit leaves
!                                 !      (mol CO2/m2/s).
,wlitev_shd(land_pts)
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for shaded leaves
!                                 !      (mol CO2/m2/s).

REAL ::                                                                       &
 aparv(land_pts)                                                              &
                            ! WORK APAR for each leaf layer
!                                 !      (W/m2).
,apar_lit(land_pts)                                                           &
                            ! WORK Mean APAR for non-light
!                                 !      limited leaves (W/m2/LAI).
,apar_unlit(land_pts)                                                         &
                            ! WORK Mean APAR for light
!                                 !      limited leaves (W/m2/LAI).
,dlai(land_pts)                                                               &
                            ! WORK LAI Increment.
,lai_lit(land_pts)                                                            &
                            ! WORK Total LAI of non-light
!                                 !      limited leaves.
,lai_unlit(land_pts)
                            ! WORK Total LAI of light
!                                 !      limited leaves.

REAL ::                                                                       &
 stemc
                            ! WORK stem carbon

INTEGER ::                                                                    &
 clos_index(land_pts)                                                         &
                            ! WORK Index of land points
!                                 !      with closed stomata.
,clos_pts
                            ! WORK Number of land points
!                                 !      with closed stomata.

REAL ::                                                                       &
 nleaf_layer(land_pts)                                                        &
                            ! WORK Leaf nitrogen concentration in a layer.
                            ! kgN/kgC if l_trait_phys=F
                            ! gN/m2   if l_trait_phys=T.
,can_averaging_fac(land_pts)                                                  &
                            ! WORK factor to convert top of canopy
                            ! value to canopy average.
,lma_tmp
                            ! WORK Temporary leaf mass per area  for
                            ! crops, in kg leaf per m2 leaf area

REAL :: power, tdegc
REAL :: expkn

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_STOM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------

!$OMP PARALLEL DO IF(land_pts > 1)                                            &
!$OMP DEFAULT(NONE)                                                           &
!$OMP SHARED(land_pts,anetl,anetc,gl,rdc,rdmean,ci,fo3_l,fo3_l_sun,fo3_l_shd, &
!$OMP         flux_o3_l,fo3,flux_o3,o3mol,isoprene,terpene,methanol,acetone,  &
!$OMP         fsmc_scale)                                                     &
!$OMP PRIVATE(l)
DO l = 1, land_pts
  anetl(l)     = 0.0
  anetc(l)     = 0.0
  gl(l)        = 0.0
  rdc(l)       = 0.0
  rdmean(l)    = 0.0
  ci(l)        = 0.0
  fo3_l(l)     = 1.0
  fo3_l_sun(l) = 1.0
  fo3_l_shd(l) = 1.0
  flux_o3_l(l) = 0.0
  fo3(l)       = 0.0
  flux_o3(l)   = 0.0
  o3mol(l)     = 0.0

  isoprene(l)  = 0.0
  terpene(l)   = 0.0
  methanol(l)  = 0.0
  acetone(l)   = 0.0

  fsmc_scale(l) = 1.0
END DO
!$OMP END PARALLEL DO

SELECT CASE ( can_rad_mod )
CASE ( 4,5,6 )
  DO l = 1, land_pts
    gc(l) = 0.0
  END DO
END SELECT

!Pre-calculate some of the expensive parts found in sf_stom => leaf_limits
!tstar is for the current pft, so can't be moved up another level

!$OMP PARALLEL DO IF(land_pts > 1) DEFAULT(NONE) PRIVATE(l,power,tdegc)        &
!$OMP& SHARED(land_pts, tstar, tupp, ft, tlow, q10_leaf, denom,       &
!$OMP& qtenf_term, tau, kc, ko) SCHEDULE(STATIC)
DO l = 1, land_pts
  tdegc         = tstar(l) - zerodegc
  power         = 0.1 * (tdegc- 25.0)
  denom(l)      = (1 + EXP (0.3 * (tdegc - tupp(ft)))) *                      &
                  (1 + EXP (0.3 * (tlow(ft) - tdegc)))
  qtenf_term(l) = q10_leaf(ft)** power
  tau(l)        = 2600.0  * (0.57 ** power)
  kc(l)         = 30.0    * (2.1 ** power)
  ko(l)         = 30000.0 * (1.2 ** power)
END DO
!$OMP END PARALLEL DO

IF (l_new_qsat_jules) THEN
  CALL qsat_new(qs,tstar,pstar,land_pts)
ELSE
  ! DEPENDS ON: qsat
  CALL qsat(qs,tstar,pstar,land_pts)
END IF

!-----------------------------------------------------------------------
! Set the canopy CO2 concentration.
!-----------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(i, j, l, m, lma_tmp)     &
!$OMP SHARED(l_co2_interactive, veg_pts, veg_index, land_index, t_i_length,   &
!$OMP        co2c, co2_3d, co2, dq, qs, q1, can_rad_mod, fpar, kpar, lai, ft, &
!$OMP        apar, omega, ipar, l_trait_phys, nnpft, dvi_cpft, nleaf_top,     &
!$OMP        nmass, nl0, dlai, ilayers, o3mol, o3, pstar, tstar, lma)
IF ( l_co2_interactive ) THEN
  !       Use full 3D CO2 field.
!$OMP  DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    co2c(l) = co2_3d(i,j)
  END DO
!$OMP END DO NOWAIT
ELSE
  !       Use single CO2_MMR value.
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    co2c(l) = co2
  END DO
!$OMP END DO NOWAIT
END IF

!-----------------------------------------------------------------------
! Calculate the surface to level 1 humidity deficit and the surface
! density of the air
!-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO m = 1,veg_pts
  l     = veg_index(m)
  dq(l) = MAX(0.0,(qs(l) - q1(l)))
END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------
! Calculate the PAR absorption factor
!-----------------------------------------------------------------------
IF ( can_rad_mod == 1 ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    fpar(l) = (1.0 - EXP(-kpar(ft) * lai(l))) / kpar(ft)
  END DO
!$OMP END DO NOWAIT
END IF

!-----------------------------------------------------------------------
! Calculate the PAR absorbed by the top leaf and set top leaf N value.
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m  = 1,veg_pts
  l = veg_index(m)
  apar(l) = (1.0 - omega(ft)) * ipar(l)
  IF ( l_trait_phys ) THEN
    IF ( ft > nnpft ) THEN
      lma_tmp = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
      nleaf_top(l) = nmass(ft) * lma_tmp * 1000.0   ! gN/gleaf * gleaf/m2 = gN/m2
    ELSE
      nleaf_top(l) = nmass(ft) * lma(ft) * 1000.0   ! gN/gleaf * gleaf/m2 = gN/m2
    END IF
  ELSE
    nleaf_top(l) = nl0(ft)
  END IF
END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------
! Calculate the LAI in each canopy layer.
!-----------------------------------------------------------------------
SELECT CASE ( can_rad_mod )
CASE ( 2:6 )
!$OMP DO SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    dlai(l) = lai(l) / REAL(ilayers)
  END DO
!$OMP END DO NOWAIT
END SELECT

!-----------------------------------------------------------------------
! Convert O3 concentration from ppb to moles
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m  = 1,veg_pts
  l = veg_index(m)
  o3mol(l) = o3(l) * pstar(l) / (rmol * tstar(l))
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

!-----------------------------------------------------------------------
! Iterate to ensure that the canopy humidity deficit is consistent with
! the H2O flux. Ignore the (small) difference between the canopy and
! reference level CO2 concentration. Intially set the canopy humidity
! deficit using the previous value of GC.
!-----------------------------------------------------------------------
! Some values of can_rad_mod require iteration for each layer and hence
! follow a different path through the code.
!-----------------------------------------------------------------------

IF ( can_rad_mod == 4 ) THEN

  !-----------------------------------------------------------------------
  !       Varying N model+altered leaf respiration
  !       Multiple canopy layers
  !       N varies through canopy as exponential function of layers.
  !-----------------------------------------------------------------------

  DO n = 1,ilayers
    !-----------------------------------------------------------------------
    ! Initialise GL and calculate the PAR absorbed in this layer.
    !-----------------------------------------------------------------------
    expkn = EXP((n-1) / REAL(ilayers) * (-kn(ft)))
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(dlai, expkn, faparv, faparv_layer, gl, nleaf_layer, n,&
!$OMP                    nleaf_top, veg_index, veg_pts)                        &
!$OMP             SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      gl(l) = 0.0
      nleaf_layer(l) = nleaf_top(l) * expkn
      faparv_layer(l,n) = faparv(l,n) * dlai(l)
    END DO
!$OMP END PARALLEL DO

    !-----------------------------------------------------------------------
    ! Iterate to ensure that the canopy humidity deficit is consistent with
    ! the H2O flux. Ignore the (small) difference between the canopy and
    ! reference level CO2 concentration. Intially set the canopy humidity
    ! deficit to using GL=0.
    !-----------------------------------------------------------------------

    DO k = 1,iter
      !-----------------------------------------------------------------------
      ! Diagnose the canopy level humidity deficit and CO2 concentration
      !-----------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                     &
!$OMP          SHARED(ca, co2c, dq, dqc, gl, oa, pstar, ra, ra_rc, veg_index,  &
!$OMP                 veg_pts)
!$OMP DO SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        ra_rc(l) = ra(l) * gl(l)
        dqc(l) = dq(l) / (1.0 + ra_rc(l))
      END DO
!$OMP END DO NOWAIT

      !-----------------------------------------------------------------------
      ! Calculate the canopy resistance and photosynthesis
      !-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
      DO m  = 1,veg_pts
        l = veg_index(m)
        ca(l) = co2c(l) / epco2 * pstar(l)
        oa(l) = o2 / epo2 * pstar(l)
      END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Calculate the limiting factors for leaf photosynthesis
      !-----------------------------------------------------------------------
      CALL leaf_limits (land_pts,veg_pts,veg_index,ft                         &
,                       nleaf_layer                                           &
,                       dqc,apar,tstar,ca,oa,pstar,fsmc                       &
,                       clos_pts,open_pts,clos_index,open_index               &
,                       ci,rd,wcarb,wexpt,wlite,                              &
                        denom, tau, qtenf_term, kc, ko)

!$OMP PARALLEL DO IF(open_pts > 1) DEFAULT(NONE) PRIVATE(l, m)           &
!$OMP             SHARED(acr, apar, faparv, faparv_layer, ipar, land_index, n, &
!$OMP                    open_index, open_pts, rd, t_i_length, wlite, wlitev,  &
!$OMP                    veg_index) SCHEDULE(STATIC)
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        wlitev(l) = wlite(l) / apar(l) * faparv(l,n) * ipar(l)
        acr(l) = apar(l) / conpar
        IF (acr(l) * 1.0e6 * faparv_layer(l,n) >  10.0) THEN
          rd(l) = ( 0.5 - 0.05 * LOG(acr(l) * faparv_layer(l,n) * 1.0e6))     &
                   * rd(l)
        END IF
      END DO
!$OMP END PARALLEL DO

      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l                                           &
,                fsmc,tstar,ca,ci,rd,wcarb,wexpt,wlitev                       &
,                gl,anetl,fo3_l)

    END DO                 ! K-ITER

!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(anetc, anetl, dlai, flux_o3, flux_o3_l, fo3, fo3_l,   &
!$OMP                    rdmean,ilayers,gc, gl, rd, rdc, veg_index, veg_pts,   &
!$OMP                    LAI,l_o3_damage                                     ) &
!$OMP             SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      anetc(l) = anetc(l) + anetl(l) * dlai(l)
      gc(l) = gc(l) + gl(l) * dlai(l)
      rdc(l) = rdc(l) + rd(l) * dlai(l)

      rdmean(l) = rdmean(l) + rd(l) / REAL(ilayers)

      IF (l_o3_damage) THEN
        flux_o3(l) = flux_o3(l) + flux_o3_l(l) * dlai(l)
        fo3(l) = fo3(l) + fo3_l(l) * dlai(l) / lai(l)
      ELSE
        flux_o3(l) = 0.0
        fo3(l) = 1.0
      END IF
    END DO
!$OMP END PARALLEL DO

  END DO                   ! N LAYERS


ELSE IF ( can_rad_mod == 5 .OR. can_rad_mod == 6 ) THEN

  !-----------------------------------------------------------------------
  !       Sunlit and shaded leaves treated separately
  !       Multiple canopy layers
  !       N varies through canopy as exponential
  !-----------------------------------------------------------------------

  DO n = 1,ilayers

    !-----------------------------------------------------------------------
    ! Initialise GL for this layer.
    ! We could initialise to gl(n-1) here, but simpler to use zero
    ! and seems to converge pretty quickly anyway.
    !-----------------------------------------------------------------------
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(ft, gl, ilayers, kn, knl, n, nleaf_top, nleaf_layer,  &
!$OMP   veg_index,dlai,can_rad_mod, veg_pts) SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      gl(l) = 0.0
      IF ( can_rad_mod == 6 ) THEN
        nleaf_layer(l) = nleaf_top(l) * EXP((n-1) * dlai(l) * (-knl(ft)))
      ELSE
        nleaf_layer(l) = nleaf_top(l) * EXP((n-1) / REAL(ilayers) * (-kn(ft)))
      END IF
    END DO
!$OMP END PARALLEL DO

    !-----------------------------------------------------------------------
    ! Iterate to ensure that the canopy humidity deficit is consistent with
    ! the H2O flux. Ignore the (small) difference between the canopy and
    ! reference level CO2 concentration.
    !-----------------------------------------------------------------------

    DO k = 1,iter

      !-----------------------------------------------------------------------
      ! Diagnose the canopy level humidity deficit and CO2 concentration
      !-----------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                     &
!$OMP          SHARED(ca, co2c, dq, dqc, gl, oa, pstar, ra, ra_rc, veg_index,  &
!$OMP                 veg_pts)
!$OMP DO SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        ra_rc(l) = ra(l) * gl(l)
        dqc(l) = dq(l) / (1.0 + ra_rc(l))
      END DO
!$OMP END DO NOWAIT

      !-----------------------------------------------------------------------
      ! Calculate the canopy resistance and photosynthesis
      !-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
      DO m  = 1,veg_pts
        l = veg_index(m)
        ca(l) = co2c(l) / epco2 * pstar(l)
        oa(l) = o2 / epo2 * pstar(l)
      END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Calculate the limiting factors for leaf photosynthesis
      !-----------------------------------------------------------------------
      CALL leaf_limits (land_pts,veg_pts,veg_index,ft                         &
,                       nleaf_layer                                           &
,                       dqc,apar,tstar,ca,oa,pstar,fsmc                       &
,                       clos_pts,open_pts,clos_index,open_index               &
,                       ci,rd,wcarb,wexpt,wlite,                              &
                        denom, tau, qtenf_term, kc, ko)

!$OMP PARALLEL IF(veg_pts > 1)                                                &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(m,l)                                                            &
!$OMP SHARED(veg_pts,veg_index,rd_sun,rd_shd,rd,open_pts,                     &
!$OMP        open_index,wlitev_sun,wlite,apar,fapar_sun,ipar,wlitev_shd,      &
!$OMP        fapar_shd,acr,n,fsun,dlai)

      !-----------------------------------------------------------------------
      ! Set the sunlit and shaded respiration rates to the uninhibited, sunlit
      ! respiration rate from leaf_limits.
      !-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        rd_sun(l) = rd(l)
        rd_shd(l) = rd(l)
      END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        wlitev_sun(l) = wlite(l) / apar(l) * fapar_sun(l,n) * ipar(l)
        wlitev_shd(l) = wlite(l) / apar(l) * fapar_shd(l,n) * ipar(l)

        acr(l) = ipar(l) / conpar
        !-----------------------------------------------------------------------
        ! Introducing inhibition of leaf respiration in the light for sunlit and
        ! shaded leaves, from papers by Atkin et al. This is an improvement over
        ! the description used for can_rad_mod=4.
        !-----------------------------------------------------------------------
        IF ( fapar_sun(l,n) * acr(l) * fsun(l,n) * dlai(l) *                  &
             1.0e6 >  10.0 ) rd_sun(l) = 0.7 * rd_sun(l)
        IF ( fapar_shd(l,n) * acr(l) * (1.0 - fsun(l,n)) * dlai(l) *            &
             1.0e6 >  10.0 ) rd_shd(l) = 0.7 * rd_shd(l)
      END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Call leaf routine separately for sunlit and shaded leaves.
      !-----------------------------------------------------------------------
      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l_sun                                       &
,                fsmc,tstar,ca,ci,rd_sun,wcarb,wexpt,wlitev_sun               &
,                gl_sun,anetl_sun,fo3_l_sun)

      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l_shd                                       &
,                fsmc,tstar,ca,ci,rd_shd,wcarb,wexpt,wlitev_shd               &
,                gl_shd,anetl_shd,fo3_l_shd)


      !           Update layer conductance.

!$OMP PARALLEL DO                                                             &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(m,l)                                                            &
!$OMP SHARED(veg_pts,veg_index,gl,fsun,n,gl_sun,gl_shd,rd,rd_sun,rd_shd)
      DO m = 1,veg_pts
        l = veg_index(m)
        gl(l) = fsun(l,n) * gl_sun(l) + (1.0 - fsun(l,n)) * gl_shd(l)
        rd(l) = fsun(l,n) * rd_sun(l) + (1.0 - fsun(l,n)) * rd_shd(l)
      END DO
!$OMP END PARALLEL DO

    END DO                 ! K-ITER

!$OMP PARALLEL DO                                                             &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP PRIVATE(m,l)                                                            &
!$OMP SHARED(veg_pts,veg_index,anetl,fsun,anetl_sun,anetl_shd,anetc,dlai,gc,  &
!$OMP        gl,rdc,rd,rdmean,ilayers,l_o3_damage,flux_o3_l,flux_o3_l_sun,    &
!$OMP        flux_o3_l_shd,fo3_l_sun,fo3_l_shd,flux_o3,fo3,lai,n,fo3_l)
    DO m = 1,veg_pts
      l = veg_index(m)

      anetl(l)     = fsun(l,n) * anetl_sun(l) + (1.0 - fsun(l,n)) * anetl_shd(l)
      anetc(l)     = anetc(l) + anetl(l) * dlai(l)

      gc(l)        = gc(l)  + gl(l) * dlai(l)
      rdc(l)       = rdc(l) + rd(l) * dlai(l)

      rdmean(l)    = rdmean(l) + rd(l) / REAL(ilayers)

      IF (l_o3_damage) THEN
        flux_o3_l(l) = fsun(l,n) * flux_o3_l_sun(l)                           &
                       + (1.0 - fsun(l,n)) * flux_o3_l_shd(l)
        fo3_l(l)     = fsun(l,n) * fo3_l_sun(l)                               &
                       + (1.0 - fsun(l,n)) * fo3_l_shd(l)

        flux_o3(l)   = flux_o3(l) + flux_o3_l(l) * dlai(l)
        fo3(l)       = fo3(l)     + fo3_l(l) * dlai(l) / lai(l)
      ELSE
        flux_o3(l) = 0.0
        fo3(l) = 1.0
      END IF
    END DO
!$OMP END PARALLEL DO

  END DO                   ! N LAYERS

  !-----------------------------------------------------------------------
ELSE                      ! CAN_RAD_MOD

  !-----------------------------------------------------------------------
  ! can_rad_mod=1, 2 or 3
  ! Iterate to ensure that the canopy humidity deficit is consistent with
  ! the H2O flux. Ignore the (small) difference between the canopy and
  ! reference level CO2 concentration. Intially set the canopy humidity
  ! deficit using the previous value of GC.
  !-----------------------------------------------------------------------

  DO k = 1,iter

    !-----------------------------------------------------------------------
    ! Diagnose the canopy level humidity deficit and CO2 concentration
    !-----------------------------------------------------------------------
    DO m = 1,veg_pts
      l = veg_index(m)
      ra_rc(l) = ra(l) * gc(l)
      dqc(l) = dq(l) / (1.0 + ra_rc(l))
    END DO

    !-----------------------------------------------------------------------
    ! Calculate the canopy resistance and photosynthesis
    !-----------------------------------------------------------------------
    DO m  = 1,veg_pts
      l = veg_index(m)
      ca(l) = co2c(l) / epco2 * pstar(l)
      oa(l) = o2 / epo2 * pstar(l)
    END DO

    !-----------------------------------------------------------------------
    ! Calculate the limiting factors for leaf photosynthesis
    !-----------------------------------------------------------------------
    CALL leaf_limits (land_pts,veg_pts,veg_index,ft                           &
,                     nleaf_top,dqc,apar,tstar,ca,oa,pstar,fsmc               &
,                     clos_pts,open_pts,clos_index,open_index                 &
,                     ci,rd,wcarb,wexpt,wlite,                                &
                      denom, tau, qtenf_term, kc, ko)

    DO m = 1,veg_pts
      l = veg_index(m)
      apar_crit(l) = 0.0
    END DO

    ! Treat closed stomata as unlit when using can_rad_mod==3.
    DO j = 1,clos_pts
      l = veg_index(clos_index(j))
      apar_crit(l) = 1.0e20
    END DO

    !-----------------------------------------------------------------------
    ! Calculate leaf level quantities and scale up to canopy through user
    ! requested approach
    !-----------------------------------------------------------------------
    IF ( can_rad_mod == 2 ) THEN

      !-----------------------------------------------------------------------
      !           Multiple canopy layers
      !           N constant through canopy
      !-----------------------------------------------------------------------

      DO m = 1,veg_pts
        l = veg_index(m)
        anetc(l) = 0.0
        gc(l) = 0.0
        wlitev(l) = wlite(l)
      END DO

      DO n = 1,ilayers
        DO m = 1,open_pts
          l         = veg_index(open_index(m))
          wlitev(l) = wlite(l) / apar(l) * faparv(l,n) * ipar(l)
        END DO

        CALL leaf (land_pts,veg_pts,veg_index,ft                              &
,                  clos_pts,open_pts,clos_index,open_index                    &
,                  o3mol,ra,flux_o3_l                                         &
,                  fsmc,tstar,ca,ci,rd,wcarb,wexpt,wlitev                     &
,                  gl,anetl,fo3_l)

        DO m = 1,veg_pts
          l = veg_index(m)

          anetc(l) = anetc(l) + anetl(l) * dlai(l)
          gc(l) = gc(l) + gl(l) * dlai(l)

          IF (l_o3_damage) THEN
            flux_o3(l) = flux_o3(l) + flux_o3_l(l) * dlai(l)
            fo3(l) = fo3(l) + fo3_l(l) * dlai(l) / lai(l)
          ELSE
            flux_o3(l) = 0.0
            fo3(l) = 1.0
          END IF

        END DO
      END DO

    ELSE IF ( can_rad_mod == 3 ) THEN

      !-----------------------------------------------------------------------
      !           sunlit and shaded leaves treated separately
      !           N constant through canopy
      !-----------------------------------------------------------------------
      !           Initialise.
      DO m = 1,veg_pts
        l = veg_index(m)
        anetc(l) = 0.0
        gc(l) = 0.0
        wlitev(l) = wlite(l)
        lai_lit(l) = 0.0
        lai_unlit(l) = 0.0
        apar_lit(l) = 0.0
        apar_unlit(l) = 0.0
      END DO

      DO j = 1,open_pts
        l = veg_index(open_index(j))
        ! Not sure why 10^-10 was chosen here - will find out from Doug at CEH
        ! Probably something to do with physically realistic values
        ! Doug says: sort of. It's a recurring issue, don't know answer.
        ! We want to avoid having a "very small" number on the denominator.
        ! EPSILON is not that small for a 32-bit real. TINY is a bit too small.
        IF ( wlite(l) < 1.0e-10 .AND. wcarb(l) < 1.0e-10 ) THEN
          apar_crit(l) = 0.0
        ELSE
          apar_crit(l) = MIN(wcarb(l),wexpt(l)) * apar(l) / wlite(l)
        END IF
      END DO

      !-----------------------------------------------------------------------
      ! Divide LAI into sunlit and shaded layers.  The sum,
      !   lai_lit + lai_unlit = lai
      ! must always hold else plant-level GPP, NPP and respiration rates will
      ! be diagnosed incorrectly towards the end of this subroutine.
      !-----------------------------------------------------------------------
      DO n = 1,ilayers
        DO m = 1,veg_pts
          l        = veg_index(m)
          aparv(l) = faparv(l,n) * ipar(l)

          IF (aparv(l) < apar_crit(l)) THEN
            apar_unlit(l) = apar_unlit(l)                                     &
                        + aparv(l) * dlai(l)
            lai_unlit(l) = lai_unlit(l) + dlai(l)
          ELSE
            apar_lit(l) = apar_lit(l)                                         &
                        + aparv(l) * dlai(l)
            lai_lit(l) = lai_lit(l) + dlai(l)
          END IF
        END DO
      END DO

      ! Calculate net photosynthesis and conductance for sunlit leaves.
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        IF (lai_lit(l) > 0.0)                                                 &
          apar_lit(l) = apar_lit(l) / lai_lit(l)
        wlitev(l) = wlite(l) / apar(l) * apar_lit(l)
      END DO

      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l                                           &
,                fsmc,tstar,ca,ci,rd,wcarb,wexpt,wlitev                       &
,                gl,anetl,fo3_l)

      DO m = 1,veg_pts
        l = veg_index(m)

        anetc(l) = anetc(l) + anetl(l) * lai_lit(l)
        gc(l) = gc(l) + gl(l) * lai_lit(l)

        IF (l_o3_damage) THEN
          flux_o3(l) = flux_o3(l) + flux_o3_l(l) * lai_lit(l)
          fo3(l) = fo3(l) + fo3_l(l) * lai_lit(l) / lai(l)
        ELSE
          flux_o3(l) = 0.0
          fo3(l) = 1.0
        END IF

      END DO

      ! Calculate net photosynthesis and conductance for shaded leaves.
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        IF (lai_unlit(l) > 0.0)                                               &
          apar_unlit(l) = apar_unlit(l) / lai_unlit(l)
        wlitev(l) = wlite(l) / apar(l) * apar_unlit(l)
      END DO

      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l                                           &
,                fsmc,tstar,ca,ci,rd,wcarb,wexpt,wlitev                       &
,                gl,anetl,fo3_l)

      DO m = 1,veg_pts
        l = veg_index(m)

        anetc(l) = anetc(l) + anetl(l) * lai_unlit(l)
        gc(l) = gc(l) + gl(l) * lai_unlit(l)

        IF (l_o3_damage) THEN
          flux_o3(l) = flux_o3(l) + flux_o3_l(l) * lai_unlit(l)
          fo3(l) = fo3(l) + fo3_l(l) * lai_unlit(l) / lai(l)
        ELSE
          flux_o3(l) = 0.0
          fo3(l) = 1.0
        END IF

      END DO

    ELSE  !  can_rad_mod

      !-----------------------------------------------------------------------
      !           can_rad_mod = 1
      !           "big leaf" model
      !           N varies through canopy according to Beers Law
      !-----------------------------------------------------------------------

      CALL leaf (land_pts,veg_pts,veg_index,ft                                &
,                clos_pts,open_pts,clos_index,open_index                      &
,                o3mol,ra,flux_o3_l                                           &
,                fsmc,tstar,ca,ci,rd,wcarb,wexpt,wlite                        &
,                gl,anetl,fo3_l)

      DO m = 1,veg_pts
        l = veg_index(m)

        anetc(l) = anetl(l) * fpar(l)
        gc(l) = fpar(l) * gl(l)
        rdc(l) = rd(l) * fpar(l)

        IF ( lai(l) > EPSILON(0.0) ) THEN
          rdmean(l) = rd(l) * fpar(l) / lai(l)
        ELSE
          rdmean(l) = rd(l)
        END IF

        IF (l_o3_damage) THEN
          flux_o3(l)= flux_o3_l(l) * fpar(l)
          fo3(l)= fo3_l(l)
        ELSE
          flux_o3(l) = 0.0
          fo3(l) = 1.0
        END IF

      END DO

    END IF  !  can_rad_mod (= 2 or 3)

  END DO   ! End of iteration loop

END IF  ! can_rad_mod (= 4 or 5)

IF ( can_rad_mod == 3 ) THEN
  !-----------------------------------------------------------------------
  !       Impose glmin at closed points.
  !-----------------------------------------------------------------------
  DO m = 1,clos_pts
    l = veg_index(clos_index(m))
    gc(l) = glmin(ft)
  END DO
END IF

!-----------------------------------------------------------------------
!     Calculate plant level respiration, NPP and GPP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Calculate the conversion from top-leaf to canopy-average nitrogen.
!-----------------------------------------------------------------------------
can_averaging_fac(:) =                                          &
    get_can_ave_fac( ft, land_pts, veg_pts, veg_index, lai )

SELECT CASE ( can_rad_mod )
CASE ( 2,3 )
  DO m = 1,veg_pts
    l = veg_index(m)
    rdc(l) = rd(l) * lai(l)
    rdmean(l) = rd(l)
  END DO
END SELECT

!$OMP PARALLEL DO                                                             &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(SHARED)                                                         &
!$OMP PRIVATE(m,l,stemc,lma_tmp,fstem,stem_resp_scaling)
DO m = 1,veg_pts
  l = veg_index(m)

  !-----------------------------------------------------------------------
  ! Calculate the actual and balanced mean leaf nitrogen concentration
  ! assuming perfect light acclimation, then
  ! calculate the total nitrogen content of the leaf, root and stem
  ! Assume that root biomass is equal to balanced growth leaf biomass
  !-----------------------------------------------------------------------
  lai_bal(l) = (a_ws(ft) * eta_sl(ft) * ht(l) / a_wl(ft))                           &
        **(1.0 / (b_wl(ft) - 1.0))

  !-----------------------------------------------------------------------
  ! Calculate the total nitrogen content of the leaf, root and stem
  !-----------------------------------------------------------------------
  IF ( ft > nnpft ) THEN
    !Crop PFTs
    stemc = stemc_from_prognostics(ft - nnpft, canht(l) )
    root(l) = rootc_cpft(l,ft - nnpft)

    IF ( l_trait_phys ) THEN

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), can_averaging_fac(l) )
      n_root(l) = nr_nl(ft) * nmass(ft) * (1.0 / cfrac_l(ft - nnpft)) * root(l)
      n_stem(l) = ns_nl(ft) * nmass(ft) * (1.0 / cfrac_l(ft - nnpft)) * stemc
      ! nmass(ft)/cfrac_l(ft-nnpft) is equivalent to nl0(ft)

      lma_tmp   = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
      nlmean(l) = nmass(ft) * lma_tmp * can_averaging_fac(l)
    ELSE
      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), can_averaging_fac(l) )
      n_root(l) = nr_nl(ft) * nl0(ft) * root(l)
      n_stem(l) = ns_nl(ft) * nl0(ft) * stemc

      nlmean(l) = nl0(ft) * can_averaging_fac(l) * cfrac_l(ft - nnpft)          &
                 * lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
    END IF

  ELSE

    !Non-crop PFTs
    IF ( l_trait_phys ) THEN 
      root(l) = lma(ft) * lai_bal(l)
      !Note new units of temporary variable root:
      !kg root/m2= gleaf/m2 * kg/g

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), can_averaging_fac(l) )
      nlmean(l) = nmass(ft) * lma(ft) * can_averaging_fac(l)

      n_root(l) = nr(ft) * root(l) * cmass

      !Initial calculation of N content in respiring stem wood
      n_stem(l) = eta_sl(ft) * ht(l) * lai_bal(l) * nsw(ft)

      !Reduce n_stem for consistency with non-trait n_stem for now
      !This must be done to achieve realistic respiration rates.
      fstem = 1.0 / a_ws(ft)
      stem_resp_scaling = fstem + (1 - fstem) * hw_sw(ft)
      n_stem(l) = n_stem(l) * stem_resp_scaling

    ELSE

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), can_averaging_fac(l) )
      root(l) = sigl(ft) * lai_bal(l)
      n_root(l) = nr_nl(ft) * nl0(ft) * root(l)

      IF ( l_stem_resp_fix ) THEN
        n_stem(l) = ns_nl(ft) * nl0(ft) * eta_sl(ft) * ht(l) * lai_bal(l)
      ELSE
        n_stem(l) = ns_nl(ft) * nl0(ft) * eta_sl(ft) * ht(l) * lai(l)
      END IF
      
      nlmean(l) = nl0(ft) * can_averaging_fac(l) * sigl(ft)

    END IF

  END IF


  !-----------------------------------------------------------------------
  ! Calculate the Gross Primary Productivity, the plant maintenance
  ! respiration rate, and the wood maintenance respiration rate
  ! in kg C/m2/sec
  !-----------------------------------------------------------------------
  gpp(l) = cconu * (anetc(l) + rdc(l) * fsmc(l))
  IF (l_scale_resp_pm) THEN
    fsmc_scale(l) = fsmc(l)
  END IF
  IF ( lai(l) > EPSILON(0.0) ) THEN
    resp_p_m(l) = cconu * rdc(l)                                          &
         * (n_leaf(l) * fsmc(l) + n_stem(l) * fsmc_scale(l) +                 &
            n_root(l) * fsmc_scale(l)) / n_leaf(l)
    resp_w(l) = cconu * rdc(l) * n_stem(l) * fsmc_scale(l) / n_leaf(l)
    resp_r(l) = cconu * rdc(l) * n_root(l) * fsmc_scale(l) / n_leaf(l)
    resp_l(l) = cconu * rdc(l) * fsmc(l)
  ELSE
    resp_w(l) = cconu * rdmean(l) * n_stem(l) * fsmc_scale(l) / nlmean(l)
    resp_r(l) = cconu * rdmean(l) * n_root(l) * fsmc_scale(l) / nlmean(l)
    resp_l(l) = cconu * rdc(l) * fsmc(l)
    resp_p_m(l) = resp_w(l) + resp_r(l) + resp_l(l)
  END IF

  !-----------------------------------------------------------------------
  ! Calculate the total plant respiration and the Net Primary Productivity
  !-----------------------------------------------------------------------
  resp_p_g(l) = r_grow(ft) * (gpp(l) - resp_p_m(l))
  resp_p(l) = resp_p_m(l) + resp_p_g(l)
  npp(l) = gpp(l) - resp_p(l)

END DO
!$OMP END PARALLEL DO
!-----------------------------------------------------------------------
! Calculate BVOC emissions
!-----------------------------------------------------------------------
IF ( l_bvoc_emis )                                                            &
  CALL bvoc_emissions(land_pts,veg_pts,ft,veg_index,                          &
                      open_pts,open_index,clos_pts,clos_index,                &
                      lai,ci,gpp,tstar,                                       &
                      isoprene,terpene,methanol,acetone)
! if ft is a crop that's not emerged in a particular grid box,
! set all the outputted variables to zero
IF ( ft > nnpft ) THEN

!$OMP PARALLEL DO                                                             &
!$OMP SCHEDULE(STATIC)                                                        &
!$OMP DEFAULT(NONE)                                                           &
!$OMP SHARED(veg_pts,veg_index,dvi_cpft,nnpft,gpp,resp_w,resp_p,npp,gc,fo3,   &
!$OMP         flux_o3,isoprene,terpene,methanol,acetone,ft)                   &
!$OMP PRIVATE(m,l)
  DO m = 1,veg_pts
    l = veg_index(m)

    IF ( dvi_cpft(l,ft - nnpft) < 0.0 ) THEN
      gpp(l)      = 0.0
      resp_w(l)   = 0.0
      resp_p(l)   = 0.0
      npp(l)      = 0.0
      gc(l)       = 0.0
      fo3(l)      = 0.0
      flux_o3(l)  = 0.0
      isoprene(l) = 0.0
      terpene(l)  = 0.0
      methanol(l) = 0.0
      acetone(l)  = 0.0
    END IF
  END DO
!$OMP END PARALLEL DO
END IF

!-----------------------------------------------------------------------
! Calculate FAPAR diagnostic (fraction of absorbed photosynthetically
! active radiation). N.b. this is not per LAI.
!-----------------------------------------------------------------------

fapar_diag(:) = 0.0

IF ( l_fapar_diag ) THEN
  SELECT CASE ( can_rad_mod )
  CASE ( 1 )
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = (1.0 - omega(ft)) * fpar(l) * kpar(ft)
    END DO
  CASE ( 2,3,4 )
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = SUM(faparv(l,:)) * dlai(l)
    END DO
  CASE DEFAULT !5,6
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = SUM( fsun(l,:) * fapar_sun(l,:) +                     &
                           ( 1.0 - fsun(l,:) ) * fapar_shd(l,:)             &
                         ) * dlai(l)
    END DO
  END SELECT
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_stom

! **********************************************************************
! Calculates the canopy resistance, net photosynthesis and transpiration
! by scaling-up the leaf level response using the "Big-Leaf" approach
! of Sellers et al. (1994)

! Written by Peter Cox (May 1995)
! **********************************************************************

END MODULE sf_stom_mod
