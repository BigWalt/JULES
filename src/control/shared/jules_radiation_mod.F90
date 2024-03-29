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

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_radiation (unitnumber)

! Description:
! Read the JULES_RADIATION namelist

USE setup_namelist, ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_RADIATION'

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 1
INTEGER, PARAMETER :: n_log = 11
INTEGER, PARAMETER :: n_real = 5

TYPE my_namelist
  SEQUENCE
  INTEGER :: i_sea_alb_method
  LOGICAL :: l_spec_albedo
  LOGICAL :: l_spec_alb_bs
  LOGICAL :: l_niso_direct
  LOGICAL :: l_snow_albedo
  LOGICAL :: l_embedded_snow
  LOGICAL :: l_mask_snow_orog
  LOGICAL :: l_albedo_obs
  LOGICAL :: l_dolr_land_black
  LOGICAL :: l_spec_sea_alb
  LOGICAL :: l_sea_alb_var_chl
  LOGICAL :: l_cosz
  REAL    :: wght_alb(4)
  REAL    :: fixed_sea_albedo
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,            &
                     n_log_in = n_log, n_real_in = n_real)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_radiation, IOSTAT = errorstatus,         &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_radiation", iomessage)

  my_nml % i_sea_alb_method  = i_sea_alb_method
  my_nml % l_spec_albedo     = l_spec_albedo
  my_nml % l_spec_alb_bs     = l_spec_alb_bs
  my_nml % l_niso_direct     = l_niso_direct
  my_nml % l_snow_albedo     = l_snow_albedo
  my_nml % l_embedded_snow   = l_embedded_snow
  my_nml % l_mask_snow_orog  = l_mask_snow_orog
  my_nml % l_albedo_obs      = l_albedo_obs
  my_nml % l_dolr_land_black = l_dolr_land_black
  my_nml % l_spec_sea_alb    = l_spec_sea_alb
  my_nml % l_sea_alb_var_chl = l_sea_alb_var_chl
  my_nml % l_cosz            = l_cosz
  my_nml % wght_alb          = wght_alb
  my_nml % fixed_sea_albedo  = fixed_sea_albedo
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  i_sea_alb_method  = my_nml % i_sea_alb_method
  l_spec_albedo     = my_nml % l_spec_albedo
  l_spec_alb_bs     = my_nml % l_spec_alb_bs
  l_snow_albedo     = my_nml % l_snow_albedo
  l_niso_direct     = my_nml % l_niso_direct
  l_embedded_snow   = my_nml % l_embedded_snow
  l_masK_snow_orog  = my_nml % l_masK_snow_orog
  l_albedo_obs      = my_nml % l_albedo_obs
  l_dolr_land_black = my_nml % l_dolr_land_black
  l_spec_sea_alb    = my_nml % l_spec_sea_alb
  l_sea_alb_var_chl = my_nml % l_sea_alb_var_chl
  l_cosz            = my_nml % l_cosz
  wght_alb          = my_nml % wght_alb
  fixed_sea_albedo  = my_nml % fixed_sea_albedo
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_radiation
#endif

END MODULE jules_radiation_mod
