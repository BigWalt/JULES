










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_surface_types_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains JULES surface type information and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE max_dimensions,   ONLY: &
    elev_tile_max,          &
    ntype_max      

USE missing_data_mod, ONLY: imdi


IMPLICIT NONE

INTEGER ::                                                                  &
  nnpft,                                                                    &
                ! Number of natural pfts
                !   Derived from namelist inputs as npft - ncpft
  ncpft = 0,                                                                &
                ! Number of crop pfts
  npft  = imdi,                                                             &
                ! Number of plant functional types
  nnvg  = imdi,                                                             &
                ! Number of non-vegetation surface types
  ntype,                                                                    &
                ! Total number of surface types
  urban = imdi,                                                             &
                ! Index of the urban surface type
  lake  = imdi,                                                             &
                ! Index of the lake surface type
  soil  = imdi,                                                             &
                ! Index of the soil surface type
  ice   = imdi,                                                             &
                ! Index of the ice surface type

! For use when l_urban2t == .TRUE.
    urban_canyon = imdi,                                                      &
                  ! Index of the urban_canyon surface type
    urban_roof   = imdi,                                                      &
                  ! Index of the urban_roof surface type

! For use when l_elev_land_ice == .TRUE.
    elev_ice(elev_tile_max) = imdi,                                           &
                  ! Index of the elevated ice surface types
    elev_rock(elev_tile_max) = imdi,                                          &
                  ! Index of the elevated rock surface types

!-----------------------------------------------------------------------------
! UM only: The following are only required for UM flexible tiles.
!-----------------------------------------------------------------------------
! The UM uses the following to identify the original PFTs in the UM output.
    brd_leaf         = imdi,                                                  &
                  ! Index of surface type 'broad-leaf tree'
    brd_leaf_dec     = imdi,                                                  &
                  ! Index of surface type 'broad-leaf dec tree'
    brd_leaf_eg_trop = imdi,                                                  &
                  ! Index of surface type 'broad-leaf eg trop tree'
    brd_leaf_eg_temp = imdi,                                                  &
                  ! Index of surface type 'broad-leaf eg temp tree'
    ndl_leaf         = imdi,                                                  &
                  ! Index of surface type 'needle-leaf tree'
    ndl_leaf_dec     = imdi,                                                  &
                  ! Index of surface type 'needle-leaf dec tree'
    ndl_leaf_eg      = imdi,                                                  &
                  ! Index of surface type 'needle-leaf eg tree'
    c3_grass         = imdi,                                                  &
                  ! Index of surface type 'C3 grass'
    c3_crop          = imdi,                                                  &
                  ! Index of surface type 'C3 crop'
    c3_pasture       = imdi,                                                  &
                  ! Index of surface type 'C3 pasture'
    c4_grass         = imdi,                                                  &
                  ! Index of surface type 'C4 grass'
    c4_crop          = imdi,                                                  &
                  ! Index of surface type 'C4 crop'
    c4_pasture       = imdi,                                                  &
                  ! Index of surface type 'C4 pasture'
    shrub            = imdi,                                                  &
                  ! Index of surface type 'shrub'
    shrub_dec        = imdi,                                                  &
                  ! Index of surface type 'shrub dec'
    shrub_eg         = imdi,                                                  &
                  ! Index of surface type 'shrub eg'

! Allows new tiles to be added to the UM without a code change.
    usr_type(ntype_max)  = imdi,                                              &
                  ! Index of user defined surface types
    tile_map_ids(ntype_max)    = imdi,                                        &
                  ! User specified tile map
    tile_map_pslevs(ntype_max) = imdi

!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_surface_types/                                              &
  npft, ncpft, nnvg, brd_leaf, brd_leaf_dec, brd_leaf_eg_trop,              &
  brd_leaf_eg_temp, ndl_leaf, ndl_leaf_dec, ndl_leaf_eg, c3_grass,          &
  c3_crop, c3_pasture, c4_grass, c4_crop, c4_pasture, shrub, shrub_dec,     &
  shrub_eg, urban, urban_canyon, urban_roof, lake, soil, ice,               &
  elev_ice, elev_rock, usr_type, tile_map_ids

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SURFACE_TYPES_MOD'

CONTAINS

SUBROUTINE check_jules_surface_types()

USE max_dimensions, ONLY: npft_max, ncpft_max, nnvg_max

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SURFACE_TYPES namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: i ! Loop counter

INTEGER :: errorstatus

!-----------------------------------------------------------------------------
! Derive ntype and nnpft from the namelist values
!-----------------------------------------------------------------------------
ntype = npft + nnvg
nnpft = npft - ncpft

!-----------------------------------------------------------------------------
! Check that the given values are less than the fixed values for IO
!-----------------------------------------------------------------------------
IF ( npft > npft_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "npft > npft_max - increase npft_max and recompile")
END IF
IF ( ncpft > ncpft_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "ncpft > ncpft_max - increase ncpft_max and recompile")
END IF
IF ( nnvg > nnvg_max ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "nnvg > nnvg_max - increase nnvg_max and recompile")
END IF
IF ( ncpft > npft ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "ncpft > npft - total number of PFTs must be >= " //       &
               "number of crop PFTs")
END IF
! If these are true, then we automatically have:
!   ntype <= ntype_max (since ntype(_max) = npft(_max) + nnvg(_max))
!   nnpft <= nnpft_max (since nnpft(_max) = npft(_max) - ncpft(_max))

!-----------------------------------------------------------------------------
! Check values for the specific surface types are sensible
!-----------------------------------------------------------------------------
! PFT surface types must come before non-veg types, so if urban, lake, soil,
! ice, urban_canyon or urban_roof are given (i.e. > 0) then they must be > npft
! A soil type is required
IF ( urban > 0 .AND. ( urban <= npft .OR. urban > ntype ) ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "urban tile is given but is out of range")
END IF

IF ( lake > 0 .AND. ( lake <= npft .OR. lake > ntype ) ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "lake tile is given but is out of range")
END IF

IF ( ice > 0 .AND. ( ice <= npft .OR. ice > ntype ) ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "ice tile is given but is out of range")
END IF

DO i = 1, elev_tile_max
  IF (   ( elev_ice(i) > 0 ) .AND.                                        &
       ( ( elev_ice(i) <= npft ) .OR. ( elev_ice(i) > ntype ) ) ) THEN
    errorstatus = i
    CALL ereport("check_jules_surface_types", errorstatus,                &
                 "elev_ice(:) tile is given but is out of range")
  END IF
  IF (   ( elev_rock(i) > 0 ) .AND.                                       &
       ( ( elev_rock(i) <= npft ) .OR. ( elev_rock(i) > ntype ) ) ) THEN
    errorstatus = i
    CALL ereport("check_jules_surface_types", errorstatus,                &
                 "elev_rock(:) tile is given but is out of range")
  END IF
END DO

IF ( urban_canyon > 0 .AND.                                               &
   ( urban_canyon <= npft .OR. urban_canyon > ntype ) ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "urban_canyon tile is given but is out of range")
END IF

IF ( urban_roof > 0 .AND.                                                 &
   ( urban_roof <= npft .OR. urban_roof > ntype ) ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "urban_roof tile is given but is out of range")
END IF

IF ( soil <= npft .OR. soil > ntype ) THEN
  errorstatus = 101
  CALL ereport("check_jules_surface_types", errorstatus,                  &
               "soil tile is either unset or is out of range")
END IF

END SUBROUTINE check_jules_surface_types


SUBROUTINE print_nlist_jules_surface_types()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

INTEGER :: i ! Loop counter

CHARACTER(LEN=50000) :: lineBuffer


!-----------------------------------------------------------------------------

CALL jules_print('jules_surface_types',                                   &
                 'Contents of namelist jules_surface_types')

WRITE(lineBuffer, *) '  npft = ', npft
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  nnvg = ', nnvg
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  brd_leaf = ', brd_leaf
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  brd_leaf_dec = ', brd_leaf_dec
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  brd_leaf_eg_trop = ', brd_leaf_eg_trop
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  brd_leaf_eg_temp = ', brd_leaf_eg_temp
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  ndl_leaf = ', ndl_leaf
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  ndl_leaf_dec = ', ndl_leaf_dec
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  ndl_leaf_eg = ', ndl_leaf_eg
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c3_grass = ', c3_grass
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c3_crop = ', c3_crop
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c3_pasture = ', c3_pasture
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c4_grass = ', c4_grass
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c4_crop = ', c4_crop
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  c4_pasture = ', c4_pasture
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  shrub = ', shrub
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  shrub_dec = ', shrub_dec
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  shrub_eg = ', shrub_eg
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  urban = ', urban
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  lake = ', lake
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  soil = ', soil
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  ice = ', ice
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  urban_canyon = ', urban_canyon
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  urban_roof = ', urban_roof
CALL jules_print('jules_surface_types', lineBuffer)

DO i = 1, elev_tile_max
  WRITE(lineBuffer, *) '  elev_ice(', i, ') = ', elev_ice(i)
  CALL jules_print('jules_surface_types', lineBuffer)
END DO

DO i = 1, elev_tile_max
  WRITE(lineBuffer, *) '  elev_rock(', i, ') = ', elev_rock(i)
  CALL jules_print('jules_surface_types', lineBuffer)
END DO

WRITE(lineBuffer, *) '  usr_type = ', usr_type(1:ntype)
CALL jules_print('jules_surface_types', lineBuffer)

WRITE(lineBuffer, *) '  tile_map_ids = ', tile_map_ids(1:ntype)
CALL jules_print('jules_surface_types', lineBuffer)

END SUBROUTINE print_nlist_jules_surface_types


END MODULE jules_surface_types_mod
