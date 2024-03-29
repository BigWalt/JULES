! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE crop_utils_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Some useful utilities for the crop code
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
REAL, PARAMETER ::                                                          &
  croplai_min = 0.001,                                                      &
      ! minimum value for crop LAI
  cropcanht_min = 0.001,                                                    &
      ! minimum value for crop canopy height
  croprootc_min = 0.0001,                                                   &
      ! minimum value for crop root carbon
  cropharvc_min = 0.0,                                                      &
      ! minimum value for carbon in harvested crop parts
  cropreservec_min = 0.0
      ! minimum value for crop stem reserve pool

CONTAINS

SUBROUTINE reset_crop(n,nonyield_diag,dvi,lai,canht,rootc,harvc,reservec,   &
                      stemc,leafc)

USE cropparm, ONLY: initial_carbon
    
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Resets the crop after harvest/crop dies
!
! Method:
!   Sets crop variables to either be zero or very small
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n        ! crop tile number

REAL, INTENT(INOUT)  :: nonyield_diag
  ! carbon leaving the crop model that's not yield

REAL, INTENT(OUT) :: dvi        ! crop development index
REAL, INTENT(OUT) :: lai        ! crop leaf area index
REAL, INTENT(OUT) :: canht      ! crop canopy height
REAL, INTENT(INOUT) :: rootc      ! crop root carbon
REAL, INTENT(INOUT) :: harvc      ! crop carbon in harvested parts
REAL, INTENT(INOUT) :: reservec   ! crop stem reserve pool
REAL, INTENT(INOUT) :: stemc      ! crop stem carbon
REAL, INTENT(INOUT) :: leafc      ! crop leaf carbon

dvi          = -2.0   ! crop can be sown again

lai          = croplai_min
canht        = cropcanht_min

nonyield_diag = nonyield_diag                                             &
              + rootc - croprootc_min                                     &
              + harvc - cropharvc_min                                     &
              + reservec - cropreservec_min                               &
              + stemc - calc_cropstemc_min(n)                             &
              + leafc - calc_cropleafc_min(n)                             &
              - initial_carbon(n)

rootc        = croprootc_min
harvc        = cropharvc_min
reservec     = cropreservec_min
stemc        = calc_cropstemc_min(n)
leafc        = calc_cropleafc_min(n)

END SUBROUTINE reset_crop

SUBROUTINE initialise_crop(n, dvi, rootc, harvc, stemc, leafc)

USE cropparm, ONLY: initial_carbon

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises crop carbon pools
!
! Method:
!   The initial total amount of carbon in the crops is distributed to the 
!   carbon pools using the partition fractions at this DVI. 
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n      ! crop tile number

REAL, INTENT(IN) :: dvi    ! crop development index

REAL, INTENT(OUT) :: rootc  ! root carbon pool
REAL, INTENT(OUT) :: harvc  ! harvest carbon pool
REAL, INTENT(OUT) :: leafc  ! leaf carbon pool
REAL, INTENT(OUT) :: stemc  ! stem carbon pool

! Local variables

REAL :: f_root  ! fraction to roots
REAL :: f_stem  ! fraction to stem
REAL :: f_leaf  ! fraction to leaf
REAL :: f_harv  ! fraction to harvested parts

!-----------------------------------------------------------------------------

CALL carbon_fraction_from_dvi(n, dvi, f_root, f_stem, f_leaf, f_harv)

!----------------------------
!  Start crop with a small amount of carbon
!----------------------------

rootc = croprootc_min         + initial_carbon(n) * f_root
leafc = calc_cropleafc_min(n) + initial_carbon(n) * f_leaf
stemc = calc_cropstemc_min(n) + initial_carbon(n) * f_stem
harvc = cropharvc_min         + initial_carbon(n) * f_harv

END SUBROUTINE initialise_crop

RECURSIVE FUNCTION leafc_from_prognostics(n, dvi, lai) RESULT (leafc)
USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates leaf carbon from the DVI (crop development index)
!   and LAI (leaf area index), which are prognostics, and some crop-specific
!   allometric variables.
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL, INTENT(IN)  :: dvi           ! crop development index

REAL, INTENT(IN)  :: lai           ! crop leaf area index

! returns
REAL              :: leafc         ! crop leaf carbon

! Local variables

REAL :: sla                        ! specific leaf area

IF ( dvi >= 0.0 ) THEN

  sla = r_gamma(n) * ( ( dvi + 0.06 )** delta(n) )

  leafc = ( lai / sla ) * cfrac_l(n)
ELSE
  leafc = calc_cropleafc_min(n)
END IF

END FUNCTION leafc_from_prognostics


FUNCTION lma_from_prognostics(n, dvi) RESULT (lma)
USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates leaf mass per unit leaf area (in kg m-2) from the DVI
!   (crop development index), which is a prognostic, and some crop-specific
!   allometric variables. lma is the canopy average.
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n  ! crop tile number

REAL, INTENT(IN)  :: dvi  ! crop development index

! returns
REAL              :: lma  ! leaf mass per unit leaf area (in kg m-2)

! Local variables

REAL :: sla               ! specific leaf area (in m2 leaf area per kg leaf)

IF ( dvi >= 0.0 ) THEN
  sla = r_gamma(n) * ( ( dvi + 0.06 )** delta(n) )
ELSE
  sla = r_gamma(n) * ( 0.06 ** delta(n) )
END IF

lma = 1.0 / sla

END FUNCTION lma_from_prognostics


FUNCTION lai_from_leafc(n, dvi, leafc) RESULT (lai)
USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates leaf area index from from the DVI (crop development index),
!   leaf carbon and some crop-specific allometric variables.
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL, INTENT(IN)  :: dvi           ! crop development index

REAL, INTENT(IN)  :: leafc         ! crop leaf carbon

! returns
REAL              :: lai           ! crop leaf area index

! Local variables

REAL :: sla                        ! specific leaf area

IF ( dvi >= 0.0 ) THEN
  sla = r_gamma(n) * ( ( dvi + 0.06 )** delta(n) )
  lai = ( leafc / cfrac_l(n) ) * sla
ELSE
  lai = croplai_min
END IF


END FUNCTION lai_from_leafc


FUNCTION stemc_from_prognostics(n, canht) RESULT (stemc)
USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates stem carbon from the canopy height, which is a prognostic,
!   and some crop-specific allometric variables.
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL, INTENT(IN)  :: canht         ! crop canopy height

! returns
REAL              :: stemc         ! crop stem carbon

stemc = cfrac_s(n) * ( ( canht / allo1(n) )** (1.0 / allo2(n) ) )

END FUNCTION stemc_from_prognostics


FUNCTION canht_from_stemc(n, stemc) RESULT (canht)
USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates canopy height from the stem carbon
!   and some crop-specific allometric variables.
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n             ! crop tile number

REAL, INTENT(IN)    :: stemc         ! crop stem carbon

! returns
REAL                :: canht         ! crop canopy height

canht = allo1(n) * ( ( stemc / cfrac_s(n) )** allo2(n) )

END FUNCTION canht_from_stemc


SUBROUTINE carbon_fraction_from_dvi(n,dvi,f_root,f_stem,f_leaf,f_harv)

USE cropparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates carbon fraction to root, stem, leaf and harvested part of crop
!   using the crop development index
!
! Method:
!   See JULES-crop technical documentation (Tom Osborne, Josh Hooker).
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL, INTENT(IN)  :: dvi           ! crop development index
REAL, INTENT(OUT) :: f_root        ! carbon fraction to roots
REAL, INTENT(OUT) :: f_stem        ! carbon fraction to stem
REAL, INTENT(OUT) :: f_leaf        ! carbon fraction to leaf
REAL, INTENT(OUT) :: f_harv

! Local variables
REAL :: s1
REAL :: s2
REAL :: s3
REAL :: denom

s1 = EXP( alpha1(n) + ( beta1(n) * dvi ) )
s2 = EXP( alpha2(n) + ( beta2(n) * dvi ) )
s3 = EXP( alpha3(n) + ( beta3(n) * dvi ) )

denom = s1 + s2 + s3 + 1.0

f_root = s1 / denom
f_stem = s2 / denom
f_leaf = s3 / denom

f_harv =  1.0 / denom

END SUBROUTINE carbon_fraction_from_dvi


FUNCTION calc_cropleafc_min(n) RESULT(cropleafc_min)
IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Calculates lower limit for leaf carbon using minimum lai i.e. ensure that
!   lai never goes below croplai_min, whatever the value of DVI is.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n             ! crop tile number
! returns
REAL :: cropleafc_min
! work
REAL, PARAMETER :: dvi_at_sla_min = 2.0

cropleafc_min = leafc_from_prognostics(n, dvi_at_sla_min, croplai_min)

END FUNCTION calc_cropleafc_min


FUNCTION calc_cropstemc_min(n) RESULT(cropstemc_min)
IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Calculates lower limit for stem carbon using minimum canopy height.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n             ! crop tile number
! returns
REAL :: cropstemc_min

cropstemc_min = stemc_from_prognostics(n, cropcanht_min)

END FUNCTION calc_cropstemc_min


SUBROUTINE no_carbon_pools_below_minimum(n,rootc,harvc,reservec,stemc,      &
                                          leafc,extrac)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Makes sure the carbon pools do not drop below their lower limit
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: n           ! crop tile number

REAL, INTENT(INOUT) :: rootc       ! root biomass
REAL, INTENT(INOUT) :: harvc       ! crop harvest pool
REAL, INTENT(INOUT) :: reservec    ! stem reserve pool

REAL, INTENT(INOUT) :: stemc       ! stem carbon pool
REAL, INTENT(INOUT) :: leafc       ! leaf carbon pool

REAL, INTENT(OUT) :: extrac        ! extra carbon that was added to the crop
                                   ! to make sure their carbon pools stayed
                                   ! above their minimum values
! work
REAL :: cropleafc_min              ! minimum carbon in leaf pool
REAL :: cropstemc_min              ! minimum carbon in stem pool

extrac = 0.0

IF ( rootc < croprootc_min ) THEN
  extrac = extrac + croprootc_min - rootc
  rootc = croprootc_min
END IF

IF ( harvc < cropharvc_min ) THEN
  extrac = extrac + cropharvc_min - harvc
  harvc = cropharvc_min
END IF

IF ( reservec < cropreservec_min ) THEN
  extrac = extrac + cropreservec_min - reservec
  reservec = cropreservec_min
END IF

cropleafc_min = calc_cropleafc_min(n)
IF ( leafc < cropleafc_min ) THEN
  extrac = extrac + cropleafc_min - leafc
  leafc = cropleafc_min
END IF

cropstemc_min = calc_cropstemc_min(n)
IF ( stemc < cropstemc_min ) THEN
  extrac = extrac + cropstemc_min - stemc
  stemc = cropstemc_min
END IF

END SUBROUTINE no_carbon_pools_below_minimum

END MODULE crop_utils_mod
