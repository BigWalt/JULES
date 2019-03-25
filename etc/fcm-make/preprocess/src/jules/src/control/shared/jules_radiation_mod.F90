










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_radiation_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains radiation options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
LOGICAL ::                                                                  &
  l_spec_albedo     = .FALSE.,                                              &
                   ! Switch spectrally varying land albedo
  l_spec_alb_bs     = .FALSE.,                                              &
                   ! Switch to have only a bluesky albedo when using
                   ! spectrally varying albedo
  l_niso_direct     = .FALSE.,                                              &
                   ! Switch to use the true non-isotropic form of direct 
                   ! back-scattering for the canopy albedo
  l_snow_albedo     = .FALSE.,                                              &
                   ! Switch for prognostic snow albedo (on land)
  l_embedded_snow   = .FALSE.,                                              &
                   ! Switch for calculation of albedo with snow in
                   ! the canopy (exclusive of l_snow_albedo)
  l_mask_snow_orog  = .FALSE.,                                              &
                   ! Switch for orographic masking of snow
  l_albedo_obs      = .FALSE.,                                              &
                   ! scale the albedo on tiles to agree with obs
  l_dolr_land_black = .TRUE.,                                               &
                   ! Do not use the surface emissivity in adjusting the OLR
                   ! at land points.
                   ! This flag is introduced for historical compatibility
                   ! only. There is no equivalent choice at sea points.
  l_spec_sea_alb    = .FALSE.,                                              &
                   ! Switch spectrally varying open sea albedo
  l_sea_alb_var_chl = .FALSE.
                   ! Switch varying chlorophyll in open sea albedo

LOGICAL ::                                                                  &
  l_cosz = .TRUE.
      ! Switch for turning on calculations of cosz
      ! Used in standalone JULES only

INTEGER ::                                                                  &
  i_sea_alb_method = 1
                   ! Method of diagnosing the Ocean Surface Albedo
                   !   1 - Briegleb and Ramanathan, 1982, J. Appl. Met.
                   !       (doi:10.1175/1520-0450(1982)021<1160:SADVIC>2.0.CO;2)
                   !   2 - Modified Barker and Li, 1995, J. Climate,
                   !       (doi:10.1175/1520-0442(1995)008<2213:ISOCSS>2.0.CO;2)
                   !   3 - Jin et al. 2011, Optics Express
                   !       (doi:10.1364/OE.19.026429)

REAL ::                                                                     &
  wght_alb(4) = (/ 0.0, 0.5, 0.0, 0.5 /)
      ! Weights to form broad-band albedo from components

REAL ::                                                                     &
  fixed_sea_albedo = 0.0
      ! Value for open sea albedo if using simple fixed value method


!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_radiation/                                                &
  l_spec_albedo, l_spec_alb_bs, l_albedo_obs, l_niso_direct,                &
  l_snow_albedo, l_embedded_snow, l_mask_snow_orog,                         &
  l_dolr_land_black, l_spec_sea_alb, l_sea_alb_var_chl, i_sea_alb_method,   &
  wght_alb, fixed_sea_albedo,                                               &
! Standalone only switches
  l_cosz


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_RADIATION_MOD'

CONTAINS

SUBROUTINE check_jules_radiation()

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_RADIATION namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatus

! The snow scheme only calculates spectrally varying albedos
IF ( l_snow_albedo .AND. .NOT. l_spec_albedo ) THEN
  errorstatus = 101
  CALL ereport("check_jules_radiation", errorstatus,                      &
               "If l_snow_albedo = T then l_spec_albedo must also be T")
END IF

! The embedded snow scheme only calculates spectrally varying albedos
IF ( l_embedded_snow .AND. .NOT. l_spec_albedo ) THEN
  errorstatus = 1001
  CALL ereport("check_jules_radiation", errorstatus,                      &
               "If l_embedded_snow = T then l_spec_albedo must also be T")
END IF

END SUBROUTINE check_jules_radiation


SUBROUTINE print_nlist_jules_radiation()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_radiation',                                       &
                 'Contents of namelist jules_radiation')

WRITE(lineBuffer, *) '  l_spec_albedo = ', l_spec_albedo
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_spec_alb_bs = ', l_spec_alb_bs
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_niso_direct = ', l_niso_direct
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_snow_albedo = ', l_snow_albedo
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_embedded_snow = ', l_embedded_snow
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_mask_snow_orog = ', l_mask_snow_orog
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_albedo_obs = ', l_albedo_obs
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_dolr_land_black = ', l_dolr_land_black
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_spec_sea_alb = ', l_spec_sea_alb
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  l_sea_alb_var_chl = ', l_sea_alb_var_chl
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  i_sea_alb_method = ', i_sea_alb_method
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  fixed_sea_albedo = ', fixed_sea_albedo
CALL jules_print('jules_radiation', lineBuffer)

WRITE(lineBuffer, *) '  wght_alb = ', wght_alb
CALL jules_print('jules_surface', lineBuffer)

END SUBROUTINE print_nlist_jules_radiation


END MODULE jules_radiation_mod
