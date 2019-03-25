!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE overbank_rivers_drive_mod

CONTAINS

SUBROUTINE overbank_rivers_drive( global_land_pts, global_frac_fplain )

!-------------------------------------------------------------------------------
!
! Description:
!   Call the calculations of overbank inundation and updating lp arrays
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-------------------------------------------------------------------------------

USE overbank_inundation_mod, ONLY:                                          &
  frac_fplain_rp

USE overbank_update_mod, ONLY: overbank_update

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
    np_rivers, rivers_regrid, il_river_grid

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     rivers_regrid_from_land, rivers_regrid_to_land

IMPLICIT NONE

! Scalar arguments with intent(in)
INTEGER, INTENT(IN) :: global_land_pts
                             ! Size of GLOBAL runoff arrays on land points 

REAL, INTENT(OUT) :: global_frac_fplain(global_land_pts)
       ! Fraction of inundated floodplain predicted by the overbank inundation
       ! routine (=0 if l_riv_overbank=F), defined on global_land_pts

INTEGER :: ip

!------------------------
! Main call
!------------------------

CALL overbank_update( )

!------------------------

  ! If regridding required, call routine
IF ( rivers_regrid ) THEN

  CALL rivers_regrid_to_land( np_rivers, frac_fplain_rp,                   &
                              global_land_pts, global_frac_fplain )

  ! If not regridding, translate between land and river point vectors
ELSE IF (global_land_pts /= np_rivers) THEN
    
  DO ip = 1,np_rivers
    IF (il_river_grid(ip) > 0) THEN
      global_frac_fplain(il_river_grid(ip)) = frac_fplain_rp(ip)
    END IF
  END DO

  ! If grids identical, including land/riv points in same order, no need to regrid
ELSE

  global_frac_fplain(:) = frac_fplain_rp(:)

END IF

END SUBROUTINE overbank_rivers_drive

END MODULE overbank_rivers_drive_mod
